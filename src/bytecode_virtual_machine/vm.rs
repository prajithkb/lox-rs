use std::collections::LinkedList;
use std::convert::{TryFrom};
use std::f64::EPSILON;
use std::io::{stdout, Write};
use std::mem::{self, MaybeUninit};
use std::ops::Range;
use std::rc::Rc;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

use log::{error, info, log_enabled, trace, Level};

use crate::bytecode_virtual_machine::chunk::Chunk;
use crate::bytecode_virtual_machine::compiler::Compiler;
use crate::bytecode_virtual_machine::instructions::{Opcode};
use crate::bytecode_virtual_machine::objects::{
    shared, Class, Closure, Function, Instance, Location, Object, Upvalue, Value, Values, ObjectPtr,
};
use crate::common::lox::{utf8_to_string, Shared, Writer};
use crate::common::scanner::Scanner;
use crate::common::tokens::pretty_print;
use crate::errors::*;

use super::objects::{NativeFn, NativeFunction, Byte, SharedString, StackValue};

const STACK_SIZE: usize = 1024;

#[derive(Debug)]
struct CallFrame {
    fn_start_stack_index: usize,
    ip: usize,
}

impl CallFrame {
    fn new(fn_start_stack_index: usize) -> Self {
        CallFrame {
            fn_start_stack_index,
            ip: 0,
        }
    }
}

pub fn define_native_fn(name: String, arity: usize, vm: &mut VirtualMachine, native_fn: NativeFn) {
    let key = SharedString::from_str(&name);
    let object = vm.allocate_object(Object::Function(Rc::new(Function::Native(
        NativeFunction::new(key.clone(), arity, native_fn)))));
    let stack_value =StackValue::Object(ObjectPtr::new(object));
    vm.runtime_values.insert(key, stack_value)
    
}

pub struct VirtualMachine<'a> {
    stack: [StackValue; STACK_SIZE],
    stack_top: usize,
    call_frames: Vec<CallFrame>,
    runtime_values: Values,
    allocated_objects: Vec<Object>,
    // runtime_strings: Strings,
    up_values: LinkedList<Shared<Upvalue>>,
    custom_writer: Writer<'a>,
}

impl<'a> std::fmt::Debug for VirtualMachine<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VirtualMachine")
            .field("stack", &self.stack)
            .field("runtime_values", &self.runtime_values)
            .finish()
    }
}

fn init_stack() -> [StackValue; STACK_SIZE] {
    let data = {
        // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
        // safe because the type we are claiming to have initialized here is a
        // bunch of `MaybeUninit`s, which do not require initialization.
        let mut data: [MaybeUninit<StackValue>; STACK_SIZE] =
            unsafe { MaybeUninit::uninit().assume_init() };

        // Dropping a `MaybeUninit` does nothing. Thus using raw pointer
        // assignment instead of `ptr::write` does not cause the old
        // uninitialized value to be dropped. Also if there is a panic during
        // this loop, we have a memory leak, but there is no memory safety
        // issue.
        for elem in &mut data[..] {
            elem.write(StackValue::default());
        }

        // Everything is initialized. Transmute the array to the
        // initialized type.
        unsafe { mem::transmute::<_, [StackValue; STACK_SIZE]>(data) }
    };
    data
}

impl<'a> VirtualMachine<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        VirtualMachine::new_with_writer(None)
    }

    pub fn new_with_writer(custom_writer: Writer<'a>) -> Self {
        VirtualMachine {
            stack: init_stack(),
            stack_top: 0,
            call_frames: Vec::new(),
            runtime_values: Values::new(),
            // runtime_strings: Strings::new(),
            up_values: LinkedList::new(),
            allocated_objects: Vec::with_capacity(STACK_SIZE),
            custom_writer,
        }
    }

    pub fn clock() -> NativeFn {
        Rc::new(|_| {
            let start = SystemTime::now();
            let since_the_epoch = start
                .duration_since(UNIX_EPOCH)
                .expect("Time went backwards")
                .as_secs_f64();
            StackValue::Number(since_the_epoch)
        })
    }

    pub fn interpret(&mut self, source: String) -> Result<()> {
        self.reset_vm();
        let mut scanner = Scanner::new(source);
        let start_time = Instant::now();
        let tokens = scanner.scan_tokens()?;
        info!("Tokens created in {} us", start_time.elapsed().as_micros());
        if log_enabled!(Level::Trace) {
            pretty_print(tokens);
        }
        let start_time = Instant::now();
        let compiler = Compiler::new(tokens);
        let main_function = Rc::new(compiler.compile()?);
        info!("Compiled in {} us", start_time.elapsed().as_micros());
        self.check_arguments(&main_function, 0)?;
        let script = Object::Closure(Rc::new(Closure::new(main_function)));
        self.push(StackValue::Object(ObjectPtr::new(&script)))?;
        self.call_frames.push(CallFrame::new(0));
        let start_time = Instant::now();
        let result = self.run();
        info!("Ran in {} us", start_time.elapsed().as_micros());
        result
    }

    fn reset_vm(&mut self) {
        self.call_frames.clear();
        self.stack_top = 0;
    }

    fn allocate_object(&mut self, object: Object) -> &Object {
        self.allocated_objects.push(object);
        let index = self.allocated_objects.len();
        &self.allocated_objects[index -1]
    }

    #[inline]
    fn call_frame_mut(&mut self) -> &mut CallFrame {
        self.call_frame_peek_at_mut(0)
    }
    #[inline]
    fn call_frame(&self) -> &CallFrame {
        self.call_frame_peek_at(0)
    }
    #[inline]
    fn call_frame_peek_at_mut(&mut self, index: usize) -> &mut CallFrame {
        let len = self.call_frames.len();
        &mut self.call_frames[len - 1 - index]
    }
    #[inline]
    fn call_frame_peek_at(&self, index: usize) -> &CallFrame {
        let len = self.call_frames.len();
        &self.call_frames[len - 1 - index]
    }

    #[inline]
    fn ip(&self) -> usize {
        self.call_frame().ip
    }

    #[inline]
    fn set_ip(&mut self, index: usize) {
        self.call_frame_mut().ip = index;
    }

    #[inline]
    fn read_byte_at(&self, chunk: &Chunk, ip: usize) -> Result<Byte> {
        Ok(*chunk.code.read_item_at(ip))
    }

    #[inline]
    fn read_byte(&mut self, chunk: &Chunk, ip: &mut usize) -> Result<Byte> {
        let v = self.read_byte_at(chunk, *ip)?;
        *ip += 1;
        self.set_ip(*ip);
        Ok(v)
    }

    // #[inline]
    // fn read_constant(&mut self, chunk: &Chunk, ip: &mut usize) -> Result<Value> {
    //     let v = self.read_constant_as_ref(chunk, ip)?.clone();
    //     Ok(v)
    // }

    #[inline]
    fn read_constant_as_ref<'b>(&mut self, chunk: &'b Chunk, ip: &mut usize) -> Result<&'b Value> {
        let v = chunk.read_constant_at(*ip);
        *ip += 1;
        self.set_ip(*ip);
        Ok(v)
    }

    #[inline]
    fn current_chunk(&self) -> Result<&Chunk> {
        let function = self.current_function()?;
        match function {
            Function::UserDefined(u) => Ok(&u.chunk),
            Function::Native(_) => bail!(self.runtime_error("VM BUG: Native function cannot have a chunk")),
        }
    }

    #[inline]
    fn current_closure(&self) -> Result<&Closure> {
        let index = self.call_frame().fn_start_stack_index;
        self.closure_from_stack(index)
    }

    fn get_stack(&self, index: usize) -> Result<StackValue> {
        if index > self.stack_top {
            bail!(self.runtime_error(&format!(
                "VM BUG: index {} out of bounds, stack size = {}",
                index, STACK_SIZE
            )))
        }
        Ok(self.stack[index])
    }

    fn get_stack_ref(&self, index: usize) -> Result<&StackValue> {
        if index > self.stack_top {
            bail!(self.runtime_error(&format!(
                "VM BUG: index {} out of bounds, stack size = {}",
                index, STACK_SIZE
            )))
        }
        Ok(&self.stack[index])
    }

    fn set_stack_mut(&mut self, index: usize, v: StackValue) -> Result<()> {
        if index > STACK_SIZE {
            bail!(self.runtime_error(&format!(
                "Stack overflow, stack size = {}, index = {}",
                STACK_SIZE, index
            )));
        }
        self.stack[index] = v;
        Ok(())
    }

    #[inline]
    fn closure_from_stack(&self, index: usize) -> Result<&Closure> {
        let v = self.get_stack_ref(index)?;
        match v {
            StackValue::Object(o) => {
                match o.object_ref() {
                    Object::Closure(c) => Ok(c),
                    Object::Receiver(_, closure) => Ok(closure),
                    _ => {
                        if log::log_enabled!(Level::Trace) {
                            trace!(
                                "Stack at error [{:?}]",
                                self.sanitized_stack(0..self.stack_top, false),
                            );
                        }
                        bail!(self.runtime_error(&format!(
                            "VM BUG: Expected closure at stack index: {} but got ({})",
                            index, v,
                        )))
                    }
                }
            }
            _ =>  bail!(self.runtime_error(&format!(
                "VM BUG: Expected closure at stack index: {} but got ({})",
                index, v,
            )))
        }
    }

    #[inline]
    fn current_function(&self) -> Result<&Function> {
        let v = self.current_closure()?;
        Ok(&v.function)
    }

    #[inline]
    fn read_short(&mut self, chunk: &Chunk, ip: &mut usize) -> Result<u16> {
        let first = self.read_byte(chunk, ip)? as u16;
        let second = self.read_byte(chunk, ip)? as u16;
        Ok(first << 8 | second)
    }
    #[inline]
    fn chunk( function: &Function) -> Result<& Chunk> {
        match function {
            Function::UserDefined(u) => Ok(&u.chunk),
            Function::Native(_) => todo!(),
        }
    }

    fn run(&mut self) -> Result<()> {
        let mut current_function = self.current_closure()?.function.clone();
        let mut current_chunk  = VirtualMachine::chunk(&current_function)?;
        let ip = &mut 0;
        #[allow(unused_assignments)]
        loop {
            let mut buf = None;
            if log_enabled!(Level::Trace) {
                buf = Some(vec![]);
                trace!(
                    "IP: {} Current Stack: {:?}",
                    ip,
                    self.sanitized_stack(0..self.stack_top, false)
                );
                current_chunk
                    .disassemble_instruction_with_writer(*ip, buf.as_mut().unwrap());
            }
            let byte = self.read_byte(current_chunk, ip)?;
            let instruction: Opcode = Opcode::try_from(byte).map_err(|e| {
                self.runtime_error(&format!(
                    "Invalid instruction (byte:{}) at {}, error: {}",
                    byte,
                    ip,
                    e
                ))
            })?;
            if log_enabled!(Level::Trace) {
                let fun_name = current_function.to_string();
                trace!(
                    "IP: {}, In function {} Next Instruction: [{}]",
                    ip,
                    fun_name,
                    utf8_to_string(buf.as_ref().unwrap()).trim()
                );
            }
            match instruction {
                Opcode::Constant => {
                    let constant = self.read_constant_as_ref(current_chunk,ip)?;
                    self.push(constant.into())?;
                }
                Opcode::Return => {
                    let fn_starting_pointer = self.call_frame().fn_start_stack_index;
                    let result = self.pop();
                    self.close_upvalues(fn_starting_pointer)?;
                    if self.call_frames.len() == 1 {
                        return Ok(());
                    }
                    let _frame = self.call_frames.pop().expect("expect frame");
                    current_function = self.current_closure()?.function.clone();
                    current_chunk = VirtualMachine::chunk(&current_function)?;
                    *ip = self.ip();
                    // drop all the local values for the last function
                    self.stack_top = fn_starting_pointer;
                    // push the return result
                    self.push(result)?;
                }
                Opcode::Negate => {
                    if let StackValue::Number(v) = self.peek_at(0)? {
                        let result = StackValue::Number(-v);
                        self.pop();
                        self.push(result)?;
                    } else {
                        bail!(self.runtime_error("Can only negate numbers."));
                    }
                }
                Opcode::Add => self.add()?,
                Opcode::Subtract => self.binary_op(|a, b| StackValue::Number(a - b))?,
                Opcode::Multiply => self.binary_op(|a, b| StackValue::Number(a * b))?,
                Opcode::Divide => self.binary_op(|a, b| StackValue::Number(a / b))?,
                Opcode::Nil => self.push(StackValue::Nil)?,
                Opcode::True => self.push(StackValue::Bool(true))?,
                Opcode::False => self.push(StackValue::Bool(false))?,
                Opcode::Not => {
                    let v = self.pop();
                    self.push(StackValue::Bool(is_falsey(&v)))?
                }
                Opcode::BangEqual => {
                    let v = self.equals()?;
                    self.push(StackValue::Bool(!v))?
                }
                Opcode::Greater => self.binary_op(|a, b| StackValue::Bool(a > b))?,
                Opcode::GreaterEqual => self.binary_op(|a, b| StackValue::Bool(a >= b))?,
                Opcode::Less => self.binary_op(|a, b| StackValue::Bool(a < b))?,
                Opcode::LessEqual => self.binary_op(|a, b| StackValue::Bool(a <= b))?,
                Opcode::EqualEqual => {
                    let v = self.equals()?;
                    self.push(StackValue::Bool(v))?
                }
                Opcode::Print => {
                    let v = self.pop();
                    self.print_stack_value(&v);
                    self.new_line();
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::DefineGlobal => {
                    let value = self.pop();
                    let name = self.read_string(current_chunk, ip)?;
                    self.runtime_values.insert(name, value);
                }
                Opcode::GetGlobal => {
                    let name = self.read_str(current_chunk, ip)?;
                    let value = self.runtime_values.get(name);
                    if let Some(v) = value {
                        let v = *v;
                        self.push(v)?
                    } else {
                        bail!(self.runtime_error(&format!("Undefined variable '{}'", &name)))
                    }
                }
                Opcode::SetGlobal => {
                    let name = self.read_str(current_chunk, ip)?;
                    let value = self.peek_at(0)?;
                    let v = self.runtime_values.get_mut(name);
                    match v {
                        Some(e) => {
                            *e = value;
                        }
                        None => {
                            drop(v);
                            bail!(self.runtime_error(&format!("Undefined variable '{}'", name)))
                        }
                    }
                }
                Opcode::GetLocal => {
                    let index = self.read_byte(current_chunk, ip)? as usize;
                    let fn_start_pointer = self.call_frame().fn_start_stack_index;
                    println!("call frames at get local : {:?}, current fn {}", self.call_frames, current_function);
                    let v = self.get_stack(fn_start_pointer + index)?;
                    self.push(v)?;
                }
                Opcode::SetLocal => {
                    let index = self.read_byte(current_chunk, ip)?;
                    let fn_start_pointer = self.call_frame().fn_start_stack_index;
                    self.stack[fn_start_pointer + index as usize] = self.peek_at(0)?;
                }
                Opcode::JumpIfFalse => {
                    let offset = self.read_short(current_chunk, ip)?;
                    if is_falsey(&self.peek_at(0)?) {
                        *ip += offset as usize;
                        self.set_ip(*ip);
                    }
                }
                Opcode::Jump => {
                    let offset = self.read_short(current_chunk, ip)?;
                    *ip += offset as usize;
                    self.set_ip(*ip);
                }
                Opcode::JumpIfTrue => {
                    let offset = self.read_short(current_chunk, ip)?;
                    if !is_falsey(&self.peek_at(0)?) {
                        *ip +=  offset as usize;
                        self.set_ip(*ip);
                    }
                }
                Opcode::Loop => {
                    let offset = self.read_short(current_chunk, ip)?;
                    *ip -= offset as usize;
                    self.set_ip(*ip);
                }
                Opcode::Call => {
                    let arg_count = self.read_byte(current_chunk,ip)?;
                    self.call(arg_count)?;
                    current_function = self.current_closure()?.function.clone();
                    current_chunk = VirtualMachine::chunk(&current_function)?;
                    *ip = self.ip();
                }
                Opcode::Closure => {
                    let function = self.read_function(current_chunk, ip)?;
                    let current_fn_stack_ptr = self.call_frame().fn_start_stack_index;
                    let mut closure = Closure::new(function.clone());
                    match &*function {
                        Function::UserDefined(u) => {
                            for _ in 0..u.upvalue_count {
                                let is_local = self.read_byte(current_chunk, ip)?;
                                let index = self.read_byte(current_chunk, ip)?;
                                if is_local > 0 {
                                    let upvalue_index_on_stack =
                                        current_fn_stack_ptr + index as usize;
                                    let captured_upvalue =
                                        self.capture_upvalue(upvalue_index_on_stack);
                                    closure.upvalues.push(captured_upvalue);
                                } else {
                                    let current_closure = self.current_closure()?;
                                    let upvalue = current_closure.upvalues[index as usize].clone();
                                    closure.upvalues.push(upvalue);
                                }
                            }
                        }
                        Function::Native(_) => todo!(),
                    }
                    let object = self.allocate_object(Object::Closure(Rc::new(closure)));
                    let stack_value = StackValue::Object(ObjectPtr::new(object));
                    self.push(stack_value)?;
                }
                Opcode::GetUpvalue => {
                    let slot = self.read_byte(current_chunk, ip)?;
                    let closure = self.current_closure()?;
                    let value = {
                        let upvalue = (&*closure.upvalues[slot as usize]).borrow();
                        match &upvalue.location {
                            Location::Stack(index) => self.get_stack(*index)?,
                            Location::Heap(shared_value) => *shared_value,
                        }
                    };
                    self.push(value)?;
                }
                Opcode::SetUpvalue => {
                    let slot = self.read_byte(current_chunk, ip)?;
                    let value = self.peek_at(slot as usize)?;
                    let closure = self.current_closure()?;
                    let upvalue = &closure.upvalues[slot as usize];
                    let mut upvalue = (&**upvalue).borrow_mut();
                    let location = &mut upvalue.location;
                    match location {
                        Location::Stack(index) => {
                            let i = *index;
                            drop(upvalue);
                            self.set_stack_mut(i, value)?;
                        }
                        Location::Heap(shared_value) => {
                            let sv = &mut *shared_value;
                            *sv = value;
                        }
                    }
                }
                Opcode::CloseUpvalue => {
                    self.close_upvalues(self.stack_top - 1)?;
                    self.pop();
                }
                Opcode::Class => {
                    let class = self.read_string(current_chunk, ip)?;
                    let object_ptr = self.allocate_object(Object::Class(Rc::new(Class::new(class))));
                    let v = StackValue::Object(ObjectPtr::new(object_ptr));
                    self.push(v)?
                }
                Opcode::SetProperty => {
                    let property = self.read_string(current_chunk, ip)?;
                    let value = self.peek_at(0)?;
                    let instance = self.peek_at(1)?;
                    self.set_property(&instance, property, value)?;
                    let value = self.pop();
                    self.pop();
                    self.push(value)?;
                }
                Opcode::GetProperty => {
                    let property = self.read_str(current_chunk, ip)?;
                    let instance = self.peek_at(0)?;
                    let (value, method) = self.get_property(&instance, property)?;
                    if let Some(method) = method {
                        self.bind_method(value, method)?;
                    } else {
                        self.pop(); // instance
                        self.push(value)?;
                    }
                }
                Opcode::Method => {
                    let method_name = self.read_string(current_chunk, ip)?;
                    self.define_method(method_name)?;
                }
                Opcode::Invoke => {
                    let method = self.read_str(current_chunk, ip)?;
                    let arg_count = self.read_byte(current_chunk, ip)? as usize;
                    let receiver = self.peek_at(arg_count)?;
                    let fn_start_stack_index = self.stack_top - arg_count - 1;
                    match receiver {
                        StackValue::Object(o) => match o.object_ref() {
                            Object::Receiver(v, _) => {
                                let v = &**v;
                                match v {
                                    StackValue::Object(o) => match o.object_ref() {
                                        Object::Instance(instance) => {
                                            let class = instance.class.clone();
                                            let (method, receiver) =
                                                self.get_method_and_receiver(class, &receiver, method)?;
                                            self.set_stack_mut(fn_start_stack_index, receiver)?;
                                            self.call_function(
                                                &method.function,
                                                arg_count,
                                                fn_start_stack_index,
                                            )?;
                                        }
                                        _ => bail!(self.runtime_error("Only instance can have methods")),
                                    }
                                    _ => bail!(self.runtime_error("Only instance can have methods")),
                                }
                            }
                            Object::Instance(instance) => {
                                let class = instance.class.clone();
                                let (method, receiver) =
                                    self.get_method_and_receiver(class, &receiver, method)?;
                                self.set_stack_mut(fn_start_stack_index, receiver)?;
                                self.call_function(&method.function, arg_count, fn_start_stack_index)?;
                            }
                            _ => bail!(self.runtime_error("Only instance can have methods"))
                        }
                        _ => bail!(self.runtime_error("Only instance can have methods"))
                    };
                    current_function = self.current_closure()?.function.clone();
                    current_chunk = VirtualMachine::chunk(&current_function)?;
                    *ip = self.ip();
                }
            };
        }
    }
    fn get_method_and_receiver(
        &mut self,
        class: Rc<Class>,
        receiver: &StackValue,
        method: &SharedString,
    ) -> Result<(Rc<Closure>, StackValue)> {
        let method = self.get_method(class, method)?;
        let receiver = match receiver {
            StackValue::Object(o) => match o.object_ref() {
                Object::Receiver(r, _) => {
                   StackValue::Object(ObjectPtr::new(self.allocate_object(Object::Receiver(r.clone(), method.clone()))))
                }
            _ => {
                let object = Object::Receiver(Rc::new(*receiver), method.clone());
                StackValue::Object(ObjectPtr::new(self.allocate_object(object)))
            },
            }
            _ => {
                let object = self.allocate_object(Object::Receiver(Rc::new(*receiver), method.clone()));
                StackValue::Object(ObjectPtr::new(object))
            }
        };
        Ok((method, receiver))
    }

    fn set_property(&self, instance: &StackValue, property: SharedString, value: StackValue) -> Result<()> {
        match instance {
            StackValue::Object(o) => {
                match o.object_ref() {
                    Object::Instance(instance) => {
                        let mut fields_mut = (*instance.fields).borrow_mut();
                        fields_mut.insert(property, value);
                    },
                    Object::Receiver(instance, _) => {
                        let instance = &**instance;
                        self.set_property(instance, property, value)?;
                    },
                    _ => bail!(self.runtime_error("Only instances have properties")),
                }
            },
            _ => bail!(self.runtime_error("Only instances have properties")),
           
        }
        Ok(())
    }

    fn get_property(
        &self,
        instance: &StackValue,
        property: &SharedString,
    ) -> Result<(StackValue, Option<Rc<Closure>>)> {
        match instance {
            StackValue::Object(o) => {
                match o.object_ref() {
                    Object::Instance(instance) => {
                        let fields = (*instance.fields).borrow();
                        println!("{:?}", fields);
                        if let Some(v) = fields.get(property) {
                            let v = *v;
                            drop(fields);
                            Ok((v, None))
                        } else {
                            drop(fields);
                            let class = instance.class.clone();
                            let method = self.get_method(class, property)?;
                            let receiver = self.peek_at(0)?; // receiver
                            Ok((receiver, Some(method)))
                        }
                    },
                    Object::Receiver(instance, _) => self.get_property(instance, property),
                    _ => bail!(self.runtime_error("Only instances have properties")),
                }
            },
            _ => bail!(self.runtime_error("Only instances have properties")),
        }
    }

    fn get_method(&self, class: Rc<Class>, name: &SharedString) -> Result<Rc<Closure>> {
        let method = (*class.methods).borrow();
        if let Some(c) = method.get(name) {
            Ok(c.clone())
        } else {
            bail!(self.runtime_error(&format!("Undefined property {}", name)))
        }
    }

    fn bind_method(&mut self, receiver: StackValue, method: Rc<Closure>) -> Result<()> {
        let method_with_receiver = match receiver {
            StackValue::Object(o) => match o.object_ref() {
                Object::Receiver(r, _) => StackValue::Object(ObjectPtr::new(self.allocate_object(Object::Receiver(r.clone(), method)))),
                _ => StackValue::Object(ObjectPtr::new(self.allocate_object(Object::Receiver(Rc::new(receiver), method)))),
            },
            _ => StackValue::Object(ObjectPtr::new(self.allocate_object(Object::Receiver(Rc::new(receiver), method))))
        };
        self.pop(); // remove the instance
        self.push(method_with_receiver)?;
        Ok(())
    }

    fn define_method(&mut self, method_name: SharedString) -> Result<()> {
        let method = self.peek_at(0)?;
        match method  {
            StackValue::Object(o) => match o.object_ref() {
                Object::Closure(closure) => match self.peek_at(1)? {
                    StackValue::Object(o) => match o.object_ref() {
                        Object::Class(c) => {
                            let mut methods = (*c.methods).borrow_mut();
                            methods.insert(method_name, closure.clone());
                        }
                        _ => bail!(self.runtime_error("VM BUG: Unreachable code")),
                    }
                    _ => bail!(self.runtime_error("VM BUG: Unreachable code")),
                },
                _ => bail!(self.runtime_error("VM BUG: Unreachable code")),
            }
           _ => bail!(self.runtime_error("VM BUG: Unreachable code")),
        };
        self.pop(); //method closure
        Ok(())
    }

    fn sanitized_stack(&self, range: Range<usize>, with_address: bool) -> Vec<String> {
        let s: Vec<String> = self.stack[range]
            .iter()
            .enumerate()
            .map(|(i, v)| {
                if with_address {
                    format!("{}:({:p}->{})", i, v as *const _, v)
                } else {
                    format!("{}:({})", i, v)
                }
            })
            .collect();
        s
    }

    fn close_upvalues(&mut self, last_index: usize) -> Result<()> {
        let upvalue_iter = self.up_values.iter().rev();
        let mut count = 0;
        let _v: Result<()> = upvalue_iter
            .map(|u| (**u).borrow_mut())
            .take_while(|u| match &u.location {
                Location::Stack(index) => *index >= last_index,
                _ => false,
            })
            .try_for_each(|mut u| {
                count += 1;
                if let Location::Stack(index) = u.location {
                    let value = self.get_stack(index)?;
                    u.location = Location::Heap(value);
                }
                Ok(())
            });
        let _captured_values = self.up_values.split_off(self.up_values.len() - count);
        Ok(())
    }

    fn capture_upvalue(&mut self, stack_index: usize) -> Shared<Upvalue> {
        let upvalue_iter = self.up_values.iter().rev();
        let upvalue = upvalue_iter
            .take_while(|u| {
                let u = (***u).borrow();
                match &u.location {
                    Location::Stack(index) => *index >= stack_index,
                    _ => false,
                }
            })
            .find(|u| {
                let u = (***u).borrow();
                match &u.location {
                    Location::Stack(index) => *index == stack_index,
                    _ => false,
                }
            });
        if let Some(u) = upvalue {
            u.clone()
        } else {
            let created_value = shared(Upvalue::new_with_location(Location::Stack(stack_index)));
            self.up_values.push_back(created_value.clone());
            created_value
        }
    }

    fn call(&mut self, arg_count: Byte) -> Result<()> {
        let arg_count = arg_count as usize;
        let value = self.peek_at(arg_count)?;
        let start_index = self.stack_top - 1 - arg_count;
        match value {
            StackValue::Object(ptr) => match ptr.object_ref() {
                Object::Closure(c) => {
                    let function = c.function.clone();
                    self.call_function(&function, arg_count, start_index)
                }
                Object::Class(c) => {
                    let class = c.clone();
                    let methods = c.methods.clone();
                    let receiver = Object::Instance(Rc::new(Instance::new(class)));
                    let receiver = StackValue::Object(ObjectPtr::new(self.allocate_object(receiver)));
                    let init = SharedString::from_str("init");
                    if let Some(initializer) = (*methods).borrow().get(&init) {
                        // set the receiver at start index for the constructor;
                        let instance = self.allocate_object(Object::Receiver(Rc::new(receiver), initializer.clone()));
                        let sv = StackValue::Object(ObjectPtr::new(instance) );
                        self.set_stack_mut(
                            start_index,
                           sv
                        )?;
                        self.call_function(&initializer.function, arg_count, start_index)?;
                    } else {
                        if arg_count != 0 {
                            bail!(self
                                .runtime_error(&format!("Expected 0  arguments but got {}", arg_count)))
                        }
                        self.set_stack_mut(start_index, receiver)?;
                    }
                    Ok(())
                }
                Object::Receiver(receiver, closure) => {
                    let receiver = receiver.clone();
                    let closure = closure.clone();
                    let receiver  = self.allocate_object(Object::Receiver(receiver, closure.clone()));
                    let sv = StackValue::Object(ObjectPtr::new(receiver));
                    self.set_stack_mut(
                        start_index,
                        sv,
                    )?;
                    self.call_function(&closure.function, arg_count, start_index)
                }
                Object::Function(f)=> {
                    let f = &*f.clone();
                    self.call_function(f, arg_count, start_index)
                }
                _ => bail!(self.runtime_error(&format!(
                    "can only call a function/closure, constructor or a class method, got '{}', at stack index {}",
                    value, 
                    self.stack_top - 1 - arg_count
                ))),
            }
            _ => bail!(self.runtime_error(&format!(
                "can only call a function/closure, constructor or a class method, got '{}', at stack index {}",
                value, 
                self.stack_top - 1 - arg_count
            ))),
            
        }
    }

    fn call_function(
        &mut self,
        function: &Function,
        arg_count: usize,
        fn_start_stack_index: usize,
    ) -> Result<()> {
        let frame = CallFrame::new(fn_start_stack_index);
        self.check_arguments(function, arg_count)?;
        if let Function::Native(n) = function {
            self.call_native_function(n, arg_count, fn_start_stack_index)?;
        } else {
            self.call_frames.push(frame);
        }
        Ok(())
    }

    fn call_native_function(
        &mut self,
        native_function: &NativeFunction,
        arg_count: usize,
        fn_start_stack_index: usize,
    ) -> Result<()> {
        let result = native_function.call(vec![]);
        let mut arguments = Vec::new();
        for i in 0..arg_count {
            arguments.push(std::mem::take(&mut self.stack[fn_start_stack_index + i]));
        }
        self.stack_top = fn_start_stack_index + 1;
        self.set_stack_mut(fn_start_stack_index, result)?;
        Ok(())
    }

    #[inline]
    fn check_arguments(&mut self, function: &Function, arg_count: usize) -> Result<bool> {
        match function {
            Function::UserDefined(u) => {
                let arity = u.arity;
                if arity != arg_count {
                    bail!(self.runtime_error(&format!(
                        "Expected {} arguments but got {}",
                        arity, arg_count
                    )))
                }
            }
            Function::Native(n) => {
                let arity = n.arity;
                if arity != arg_count {
                    bail!(self.runtime_error(&format!(
                        "Expected {} arguments but got {}",
                        arity, arg_count
                    )))
                }
            }
        }
        Ok(true)
    }

    #[inline]
    fn read_str<'b>(&mut self, chunk:  &'b Chunk, ip: &mut usize) -> Result<&'b SharedString> {
        let constant = self.read_constant_as_ref(chunk, ip)?;
        let r: Result<&'b SharedString> = match constant {
            Value::Object(Object::SharedString(s)) => Ok(s),
            _ => Err(self.runtime_error("message").into()),
        };
        r
    }

    #[inline]
    fn read_string(&mut self, chunk:  &Chunk, ip: &mut usize) -> Result<SharedString> {
        let constant = self.read_constant_as_ref(chunk, ip)?;
        match constant {
            Value::Object(Object::SharedString(s)) => Ok(s.clone()),
            _ => Err(self.runtime_error("message").into()),
        }
    }

    #[inline]
    fn read_function(&mut self, chunk:  &Chunk, ip: &mut usize) -> Result<Rc<Function>> {
        let constant = self.read_constant_as_ref(chunk, ip)?;
        match constant {
            Value::Object(Object::Function(s)) => Ok(s.clone()),
            _ => bail!(self.runtime_error("Not a function")),
        }
    }

    fn runtime_error(&self, message: &str) -> ErrorKind {
        let mut error_buf = vec![];
        writeln!(error_buf, "{}", message).expect("Write failed");
        let all_call_frames = (&self.call_frames).iter().rev();
        for frame in all_call_frames {
            let stack_index = frame.fn_start_stack_index;
            if let Ok(closure) = self.closure_from_stack(stack_index) {
                let function = &*closure.function;
                let fun_name = &function.to_string();
                match &*function {
                    Function::UserDefined(u) => {
                        let ip = frame.ip;
                        let line_num = u.chunk.lines[ip];
                        writeln!(error_buf, "[line {}] in {}", line_num, fun_name)
                            .expect("Write failed")
                    }
                    Function::Native(_) => todo!(),
                };
            }
        }
        if self.stack_top < STACK_SIZE {
            // We print stack only if it is not stack overflow
            error!(
                "Current function= {}, ip ={}, stack ={:?}",
                &self
                    .current_function()
                    .map(|f| f.to_string())
                    .unwrap_or_default(),
                self.ip(),
                self.sanitized_stack(0..self.stack_top, false)
            );
        }
        if let Ok(chunk) = self.current_chunk() {
            let line = chunk.lines[self.ip()];
            runtime_vm_error(line, &utf8_to_string(&error_buf))
        } else {
            ErrorKind::RuntimeError(format!(
                "VM BUG Unable to detect line number, message: {}",
                &utf8_to_string(&error_buf)
            ))
        }
    }

    #[inline]
    fn peek_at(&self, distance: usize) -> Result<StackValue> {
        let top = self.stack_top;
        self.get_stack(top - 1 - distance)
    }

    #[inline]
    fn equals(&mut self) -> Result<bool> {
        let left = self.pop();
        let right = self.pop();
        value_equals(left, right)
    }

    #[inline]
    fn binary_op(&mut self, op: fn(f64, f64) -> StackValue) -> Result<()> {
        let (left, right) = (self.peek_at(1)?, self.peek_at(0)?);
        let (left, right) = match (left, right) {
            (StackValue::Number(l), StackValue::Number(r)) => (l, r),
            _ => bail!(self.runtime_error("Can perform binary operations only on numbers.")),
        };
        self.binary_op_with_num(left, right, op)
    }

    fn add(&mut self) -> Result<()> {
        match (self.peek_at(1)?, self.peek_at(0)?) {
            (StackValue::Object(l), StackValue::Object(r)) => {
                match (l.object_ref(), r.object_ref()) {
                    (Object::SharedString(l), Object::SharedString(r)) =>  {
                        let mut concatenated_string = String::new();
                        concatenated_string.push_str(&l.0);
                        concatenated_string.push_str(&r.0);
                        self.pop();
                        self.pop();
                        let allocated_string = self.allocate_object(Object::SharedString(SharedString::from_string(concatenated_string)));
                        let sv = StackValue::Object(ObjectPtr::new(allocated_string));
                        self.push(sv)?;
                        Ok(())
                    },  
                    _ => bail!(self.runtime_error(&format!(
                        "Add can be perfomed only on numbers or strings, got {} and {}",
                        self.peek_at(1)?,
                        self.peek_at(0)?
                    )))
                }
            },
            (StackValue::Number(_), StackValue::Number(_)) => self.binary_op(|a, b| StackValue::Number(a + b)),
            _ => bail!(self.runtime_error(&format!(
                "Add can be perfomed only on numbers or strings, got {} and {}",
                self.peek_at(1)?,
                self.peek_at(0)?
            ))),
        }
    }

    #[inline]
    fn binary_op_with_num(
        &mut self,
        left: f64,
        right: f64,
        op: fn(f64, f64) -> StackValue,
    ) -> Result<()> {
        let result = op(left, right);
        self.pop();
        self.pop();
        self.push(result)?;
        Ok(())
    }

    #[inline]
    fn push(&mut self, value: StackValue) -> Result<()> {
        if self.stack_top == STACK_SIZE {
            bail!(self.runtime_error(&format!(
                "Stack overflow, stack size = {}, index = {}",
                STACK_SIZE, self.stack_top
            )));
        }
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
        Ok(())
    }
    #[inline]
    fn pop(&mut self) -> StackValue {
        self.stack_top -= 1;
        self.stack[self.stack_top]
        // std::mem::take(&mut self.stack[self.stack_top])
    }

    pub fn free(&mut self) {
        //TODO
    }

    fn print_stack_value(&mut self, value: &StackValue) {
        match self.custom_writer.as_deref_mut() {
            Some(w) => print_stack_value(value, w),
            None => print_stack_value(value, &mut stdout()),
        }
    }
    fn new_line(&mut self) {
        match self.custom_writer.as_deref_mut() {
            Some(w) => writeln!(w).expect("Write failed"),
            None => println!(),
        };
    }
}

fn runtime_vm_error(line: usize, message: &str) -> ErrorKind {
    ErrorKind::RuntimeError(format!("Line: {}, message: {}", line, message))
}

fn num_equals(l: f64, r: f64) -> bool {
    (l - r).abs() < EPSILON
}
fn value_equals(l: StackValue, r: StackValue) -> Result<bool> {
    match (l, r) {
        (StackValue::Bool(l), StackValue::Bool(r)) => Ok(l == r),
        (StackValue::Nil, StackValue::Nil) => Ok(true),
        (StackValue::Number(l), StackValue::Number(r)) => Ok(num_equals(l, r)),
        (StackValue::Object(l), StackValue::Object(r)) => match (l.object_ref(), r.object_ref()) {
            (Object::String(l), Object::String(r)) => Ok(l == r),
            (Object::SharedString(l), Object::SharedString(r)) => Ok(l==r),
            _ => Ok(false),
        },
        _ => Ok(false),
    }
}

fn is_falsey(value: &StackValue) -> bool {
    match value {
        StackValue::Bool(b) => !b,
        StackValue::Nil => true,
        _ => false,
    }
}

fn print_stack_value(value: &StackValue, writer: &mut dyn Write) {
    crate::bytecode_virtual_machine::instructions::print_stack_value(value, writer);
}

#[cfg(test)]
mod tests {
    use crate::common::lox::{print_error, utf8_to_string};
    #[allow(unused_imports)]
    use crate::errors::*;

    use super::{define_native_fn, VirtualMachine, STACK_SIZE};

    #[test]
    fn vm_numeric_expressions() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var a = 2;
        print a; //2
        a = -2 + 4 * 2 == 6;
        print a; // true
        var b;
        print !b; // true
        print a == true; //true
        var c = a == !b; // true
        print c; //true
        print !nil; //true
        print 3 == false; //false
        print (2 + 3)/5; //1
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!(
            "2\ntrue\ntrue\ntrue\ntrue\ntrue\nfalse\n1\n",
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn vm_string_expressions() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var a = "hello ";
        var b =" world";
        var c= a;
        print c + b;
        print a ==c;
        print a==b;
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("hello  world\ntrue\nfalse\n", utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn vm_block() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var a = 2;
        {
            print a;
            var a = 3;
            print a;
            var b = a;
        }
        print a;
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("2\n3\n2\n", utf8_to_string(&buf));
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var a = 2;
        {
            print a;
            var a = 3;
            print a;
            var b = a;
        }
        print a;
        print b;
        "#;
        match vm.interpret(source.to_string()) {
            Ok(_) => panic!("Expected to fail"),
            Err(e) => assert_eq!(
                "Runtime Error: Line: 10, message: Undefined variable 'b'\n[line 10] in <fn script>\n",
                e.to_string()
            ),
        }

        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var a = 2;
        {
            var b = (2 + a) * 4;
            a = b;
        }
        print a;
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("16\n", utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn vm_if_statement() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var a = "";
        var condition = true;
        if (condition) {
            a = "if";
        } else {
            a = "else";
        }
        print a;
        condition = 2==3;
        if (condition) {
            a = "if";
        } else {
            a = "else";
        }
        print a;
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("if\nelse\n", utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn vm_logical_operations() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        print 2 or 3;
        print 2 and 3;
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("2\n3\n", utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn vm_while_loop() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var a = 5;
        var b = 1;
        while (a == 5) {
            print b;
            b = b + 1;
            if (b > 5)
                a = "stop";
        }
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("1\n2\n3\n4\n5\n", utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn vm_call_error_stack_trace() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        fun a() { b(); }
        fun b() { c(); }
        fun c() {
            c("too", "many");
        }

        a();
        "#;
        match vm.interpret(source.to_string()) {
            Ok(_) => panic!("Expect this to fail"),
            Err(e) => {
                print_error(e, &mut buf);
            }
        }
        assert_eq!(
            r#"[Runtime Error] Line: 5, message: Expected 0 arguments but got 2
[line 5] in <fn c>
[line 3] in <fn b>
[line 2] in <fn a>
[line 8] in <fn script>

"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn vm_call_success() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var hello = "hello";
        var world = " world";
        fun a() { return b(); }
        fun b() { return c(hello, world); }
        fun c(arg1, arg2) {
            print arg1 + arg2;
        }
        a();
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("hello world\n", utf8_to_string(&buf));

        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var hello = "hello";
        var world = " world";
        fun a() { return b(); }
        fun b() { return c(hello, world); }
        fun c(arg1, arg2) {
            return  arg1 + arg2;
        }
        print a();
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("hello world\n", utf8_to_string(&buf));

        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        fun fib(n) {
            if (n < 2) return n;
            return fib(n - 1) + fib(n - 2); 
          }
          
          print fib(10);
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("55\n", utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn vm_closure() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        fun outer() {
            var x = "outside";
            fun inner() {
              print x;
            }
            inner();
          }
        outer();
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("outside\n", utf8_to_string(&buf));

        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        fun outer() {
            var x = "outside";
            fun inner() {
              print x;
            }
          
            return inner;
          }
          
        var closure = outer();
        closure();
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("outside\n", utf8_to_string(&buf));

        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        var globalSet;
        var globalGet;

        fun main() {
          var a = "initial";

          fun set() { a = "updated"; }
          fun get() { print a; }

          globalSet = set;
          globalGet = get;
        }

        main();
        globalSet();
        globalGet();
        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("updated\n", utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn vm_class_fields() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        class Pair {}

        var pair = Pair();
        pair.first = 1;
        pair.second = 2;
        print pair.first + pair.second; // 3.

        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("3\n", utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn vm_class_methods() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        class Scone {
            topping(first, second) {
              print "scone with " + first + " and " + second;
            }
          }
          
          var scone = Scone();
          scone.topping("berries", "cream");

        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("scone with berries and cream\n", utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn vm_class_initializer_and_this() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        class Brunch {
            init(food, drinks) {
                this.food = food;
                this.drinks = drinks;
            }
        }
                  
        var brunch = Brunch("eggs", "coffee");
        
        print brunch.food + " and " + brunch.drinks;

        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("eggs and coffee\n", utf8_to_string(&buf));

        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        class Brunch {
            init(food, drinks) {
                this.food = food;
                this.drinks = drinks;
            }

            set_dessert(item) {
                this.dessert = item;
                return this;
            }
        }
                  
        var brunch = Brunch("eggs", "coffee");

        var brunch_with_dessert = brunch.set_dessert("cake");
        
        print brunch_with_dessert.food + " and " + brunch_with_dessert.drinks + " with " + brunch_with_dessert.dessert + " as dessert";

        "#;
        vm.interpret(source.to_string())?;
        assert_eq!(
            "eggs and coffee with cake as dessert\n",
            utf8_to_string(&buf)
        );

        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        class Cake {
    
            init(type) {
                this.type = type;
            }
            taste() {
                this.inner_taste();
                this.flavor = "Belgian chocolate";
            }
        
             taste_again() {
                this.inner_taste();
            }
        
            inner_taste() {
                var adjective = "delicious";
                print "The " + this.flavor + " " + this.type + " is " + adjective + "!";
            }
        }

        var cake = Cake("cake");
        cake.flavor = "German chocolate";
        cake.taste();
        cake.taste_again(); 
    
    
        var cookie = Cake("cookie");
        cookie.flavor = "German chocolate";
        cookie.taste();
        cookie.taste_again(); 

        "#;
        vm.interpret(source.to_string())?;
        assert_eq!("The German chocolate cake is delicious!\nThe Belgian chocolate cake is delicious!\nThe German chocolate cookie is delicious!\nThe Belgian chocolate cookie is delicious!\n", utf8_to_string(&buf));

        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        class Brunch {
            init(food, drinks) {
                this.food = food;
                this.drinks = drinks;
            }
        }
                  
        var brunch = Brunch("eggs");
        "#;
        match vm.interpret(source.to_string()) {
            Err(e) => {
                print_error(e, &mut buf);
                assert_eq!("[Runtime Error] Line: 9, message: Expected 2 arguments but got 1\n[line 9] in <fn script>\n\n", utf8_to_string(&buf))
            }
            Ok(_) => panic!("This test is expected to fail"),
        }

        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        class Brunch {
            init(food, drinks) {
                this.food = food;
                this.drinks = drinks;
                return 2;
            }
        }
                  
        var brunch = Brunch("eggs");
        "#;
        match vm.interpret(source.to_string()) {
            Err(e) => {
                print_error(e, &mut buf);
                assert_eq!("[Parse Error] [line: 6] Error at <2>: message: Can't return a value from an initializer\n", utf8_to_string(&buf))
            }
            Ok(_) => panic!("This test is expected to fail"),
        }
        Ok(())
    }

    #[test]
    fn vm_stack_overflow() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        fun infinite_recursion() {
            infinite_recursion();
        }

        infinite_recursion();
        "#;
        let mut expected = format!(
            "[Runtime Error] Line: 3, message: Stack overflow, stack size = {}, index = {}\n",
            STACK_SIZE, STACK_SIZE
        );
        for _ in 0..(STACK_SIZE - 1) {
            expected.push_str("[line 3] in <fn infinite_recursion>\n");
        }
        expected.push_str("[line 6] in <fn script>\n\n");
        match vm.interpret(source.to_string()) {
            Ok(_) => panic!("This test is expected to fail"),
            Err(e) => {
                print_error(e, &mut buf);
                assert_eq!(expected, utf8_to_string(&buf))
            }
        }
        Ok(())
    }

    #[test]
    fn vm_native_clock() -> Result<()> {
        let mut buf = vec![];
        let mut vm = VirtualMachine::new_with_writer(Some(&mut buf));
        let source = r#"
        print clock();
        "#;
        define_native_fn("clock".to_string(), 0, &mut vm, VirtualMachine::clock());
        let _ = vm.interpret(source.to_string())?;
        let output = utf8_to_string(&buf);
        // This will fail if it is not f64
        let _ = output.trim().parse::<f64>().unwrap();
        Ok(())
    }
}

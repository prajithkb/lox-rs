use std::collections::LinkedList;
use std::convert::TryFrom;
use std::f64::EPSILON;
use std::io::{stdout, Write};
use std::mem::{self, MaybeUninit};
use std::ops::Range;
use std::panic;
use std::rc::Rc;
use std::time::Instant;

use log::{info, log_enabled, trace, Level};

use crate::chunk::Chunk;
use crate::compiler::Compiler;
use crate::errors::*;
use crate::instructions::{print_value, Opcode};
use crate::lox::{utf8_to_string, Shared, Writer};
use crate::objects::{
    shared, BoundMethod, Class, Closure, Function, Instance, Location, Object, Upvalue, Value,
    Values,
};
use crate::scanner::Scanner;
use crate::tokens::pretty_print;

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

pub struct VirtualMachine<'a> {
    stack: [Value; 1024],
    stack_top: usize,
    call_frames: Vec<CallFrame>,
    runtime_values: Values,
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

fn init_stack() -> [Value; 1024] {
    let data = {
        // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
        // safe because the type we are claiming to have initialized here is a
        // bunch of `MaybeUninit`s, which do not require initialization.
        let mut data: [MaybeUninit<Value>; 1024] = unsafe { MaybeUninit::uninit().assume_init() };

        // Dropping a `MaybeUninit` does nothing. Thus using raw pointer
        // assignment instead of `ptr::write` does not cause the old
        // uninitialized value to be dropped. Also if there is a panic during
        // this loop, we have a memory leak, but there is no memory safety
        // issue.
        for elem in &mut data[..] {
            elem.write(Value::default());
        }

        // Everything is initialized. Transmute the array to the
        // initialized type.
        unsafe { mem::transmute::<_, [Value; 1024]>(data) }
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
            up_values: LinkedList::new(),
            // up_values_in_stack: LinkedList::new(),
            custom_writer,
        }
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
        self.push(Value::Object(Object::Closure(Closure::new(main_function))));
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
    fn increment_ip_by(&mut self, offset: u16) {
        let ip = self.ip();
        self.set_ip(ip + offset as usize);
    }

    #[inline]
    fn decrement_ip_by(&mut self, offset: u16) {
        let ip = self.ip();
        self.set_ip(ip - offset as usize);
    }

    #[inline]
    fn set_ip(&mut self, index: usize) {
        self.call_frame_mut().ip = index;
    }

    #[inline]
    fn read_byte_at(&self, ip: usize) -> u8 {
        let chunk = &*self.current_chunk();
        *chunk.code.read_item_at(ip)
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        let ip = self.ip();
        let v = self.read_byte_at(ip);
        self.call_frame_mut().ip += 1;
        v
    }

    #[inline]
    fn read_constant(&mut self) -> Value {
        let ip = self.ip();
        let v = self.read_constant_at(ip).clone();
        self.call_frame_mut().ip += 1;
        v
    }

    #[inline]
    fn read_constant_at(&self, ip: usize) -> &Value {
        let chunk = self.current_chunk();
        chunk.read_constant_at(ip)
    }

    #[inline]
    fn current_chunk(&self) -> &Chunk {
        let function = self.current_function();
        match function {
            Function::UserDefined(u) => &u.chunk,
        }
    }

    #[inline]
    fn current_closure(&self) -> &Closure {
        let index = self.call_frame().fn_start_stack_index;
        self.closure_from_stack(index)
    }

    #[inline]
    fn closure_from_stack(&self, index: usize) -> &Closure {
        let v = &self.stack[index];
        match &self.stack[index] {
            Value::Object(Object::Closure(c)) => c,
            Value::Object(Object::BoundMethod(BoundMethod {
                receiver: _,
                closure,
            })) => closure,
            Value::Object(Object::Receiver(_, closure)) => closure,
            _ => {
                if log::log_enabled!(Level::Trace) {
                    trace!(
                        "Stack at error [{:?}]",
                        self.sanitized_stack(0..self.stack_top, false),
                    );
                }
                panic!(
                    "VM BUG: Expected closure at stack index: {} but got ({})",
                    index, v,
                )
            }
        }
    }

    #[inline]
    fn current_function(&self) -> &Function {
        let v = self.current_closure();
        &v.function
    }

    #[inline]
    fn read_short(&mut self) -> u16 {
        let first = self.read_byte() as u16;
        let second = self.read_byte() as u16;
        first << 8 | second
    }

    fn run(&mut self) -> Result<()> {
        loop {
            let mut buf = vec![];
            if log_enabled!(Level::Trace) {
                trace!(
                    "IP: {} Current Stack: {:?}",
                    self.ip(),
                    self.sanitized_stack(0..self.stack_top, false)
                );
                let ip = self.ip();
                self.current_chunk()
                    .disassemble_instruction_with_writer(ip, &mut buf);
            }
            let byte = self.read_byte();
            let instruction: Opcode = Opcode::try_from(byte).map_err(|e| {
                self.runtime_error(&format!(
                    "Invalid instruction (byte:{}) at {}, error: {}",
                    byte,
                    self.ip(),
                    e
                ))
            })?;
            if log_enabled!(Level::Trace) {
                let fun_name = (*self.current_function()).to_string();
                trace!(
                    "In function {} Next Instruction: [{}]",
                    fun_name,
                    utf8_to_string(&buf).trim()
                );
            }
            match instruction {
                Opcode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                Opcode::Return => {
                    trace!("Call frame count: {}", self.call_frames.len());
                    let fn_starting_pointer = self.call_frame().fn_start_stack_index;
                    let result = self.pop();
                    self.close_upvalues(fn_starting_pointer);
                    if self.call_frames.len() == 1 {
                        return Ok(());
                    }
                    let _frame = self.call_frames.pop().expect("expect frame");
                    // drop all the local values for the last function
                    self.stack_top = fn_starting_pointer;
                    // push the return result
                    self.push(result);
                }
                Opcode::Negate => {
                    if let Value::Number(v) = self.peek_at(0) {
                        let result = Value::Number(-v);
                        self.push(result);
                    } else {
                        bail!(self.runtime_error("Can only negate numbers."));
                    }
                }
                Opcode::Add => self.add()?,
                Opcode::Subtract => self.binary_op(|a, b| Value::Number(a - b))?,
                Opcode::Multiply => self.binary_op(|a, b| Value::Number(a * b))?,
                Opcode::Divide => self.binary_op(|a, b| Value::Number(a / b))?,
                Opcode::Nil => self.push(Value::Nil),
                Opcode::True => self.push(Value::Bool(true)),
                Opcode::False => self.push(Value::Bool(false)),
                Opcode::Not => {
                    let v = self.pop();
                    self.push(Value::Bool(is_falsey(&v)))
                }
                Opcode::BangEqual => {
                    let v = self.equals()?;
                    self.push(Value::Bool(!v))
                }
                Opcode::Greater => self.binary_op(|a, b| Value::Bool(a > b))?,
                Opcode::GreaterEqual => self.binary_op(|a, b| Value::Bool(a >= b))?,
                Opcode::Less => self.binary_op(|a, b| Value::Bool(a < b))?,
                Opcode::LessEqual => self.binary_op(|a, b| Value::Bool(a <= b))?,
                Opcode::EqualEqual => {
                    let v = self.equals()?;
                    self.push(Value::Bool(v))
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
                    let name = self.read_string()?;
                    self.runtime_values.insert(name, value);
                }
                Opcode::GetGlobal => {
                    let name = self.read_str_without_increment()?;
                    let value = self.runtime_values.get(name);
                    if let Some(v) = value {
                        let v = v.clone();
                        self.push(v)
                    } else {
                        bail!(self.runtime_error(&format!("Undefined variable '{}'", &name)))
                    }
                    self.increment_ip_by(1);
                }
                Opcode::SetGlobal => {
                    let name = self.read_string()?;
                    let value = self.peek_at(0).clone();
                    let v = self.runtime_values.get_mut(&name);
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
                    let index = self.read_byte();
                    let fn_start_pointer = self.call_frame().fn_start_stack_index;
                    let v = self.stack[fn_start_pointer + index as usize].clone();
                    self.push(v);
                }
                Opcode::SetLocal => {
                    let index = self.read_byte();
                    let fn_start_pointer = self.call_frame().fn_start_stack_index;
                    self.stack[fn_start_pointer + index as usize] = self.peek_at(0).clone();
                }
                Opcode::JumpIfFalse => {
                    let offset = self.read_short();
                    if is_falsey(self.peek_at(0)) {
                        self.increment_ip_by(offset);
                    }
                }
                Opcode::Jump => {
                    let offset = self.read_short();
                    self.increment_ip_by(offset);
                }
                Opcode::JumpIfTrue => {
                    let offset = self.read_short();
                    if !is_falsey(self.peek_at(0)) {
                        self.increment_ip_by(offset);
                    }
                }
                Opcode::Loop => {
                    let offset = self.read_short();
                    self.decrement_ip_by(offset);
                }
                Opcode::Call => {
                    let arg_count = self.read_byte();
                    self.call(arg_count)?;
                }
                Opcode::Closure => {
                    let function = self.read_function()?;
                    let current_fn_stack_ptr = self.call_frame().fn_start_stack_index;
                    let mut closure = Closure::new(function.clone());
                    match &*function {
                        Function::UserDefined(u) => {
                            for _ in 0..u.upvalue_count {
                                let is_local = self.read_byte();
                                let index = self.read_byte();
                                if is_local > 0 {
                                    let upvalue_index_on_stack =
                                        current_fn_stack_ptr + index as usize;
                                    let captured_upvalue =
                                        self.capture_upvalue(upvalue_index_on_stack);
                                    closure.upvalues.push(captured_upvalue);
                                } else {
                                    let current_closure = self.current_closure();
                                    let upvalue = current_closure.upvalues[index as usize].clone();
                                    closure.upvalues.push(upvalue);
                                }
                            }
                        }
                    }
                    self.push(Value::Object(Object::Closure(closure)));
                }
                Opcode::GetUpvalue => {
                    let slot = self.read_byte();
                    let closure = self.current_closure();
                    let value = {
                        let upvalue = (&*closure.upvalues[slot as usize]).borrow();
                        match &upvalue.location {
                            Location::Stack(index) => self.stack[*index].clone(),
                            Location::Heap(shared_value) => shared_value.clone(),
                        }
                    };
                    self.push(value);
                }
                Opcode::SetUpvalue => {
                    let slot = self.read_byte();
                    let value = self.peek_at(slot as usize).clone();
                    let closure = self.current_closure();
                    let upvalue = &closure.upvalues[slot as usize];
                    let mut upvalue = (&**upvalue).borrow_mut();
                    let location = &mut upvalue.location;
                    match location {
                        Location::Stack(index) => {
                            let i = *index;
                            drop(upvalue);
                            self.stack[i] = value
                        }
                        Location::Heap(shared_value) => {
                            let sv = &mut *shared_value;
                            *sv = value;
                        }
                    }
                }
                Opcode::CloseUpvalue => {
                    self.close_upvalues(self.stack_top - 1);
                    self.pop();
                }
                Opcode::Class => {
                    let class = self.read_string()?;
                    self.push(Value::Object(Object::Class(Rc::new(Class::new(class)))))
                }
                Opcode::SetProperty => {
                    let property = self.read_string()?;
                    let value = self.peek_at(0).clone();
                    let instance = self.peek_at(1);
                    self.set_property(instance, property, value)?;
                    let value = self.pop();
                    self.pop();
                    self.push(value);
                }
                Opcode::GetProperty => {
                    let instance = self.peek_at(0);
                    let property = self.read_str_without_increment()?;
                    let (value, method) = self.get_property(instance, property)?;
                    if let Some(method) = method {
                        self.bind_method(value, method);
                    } else {
                        self.pop(); // instance
                        self.push(value);
                    }
                    self.increment_ip_by(1);
                }
                Opcode::Method => {
                    let method_name = self.read_string()?;
                    self.define_method(method_name)?;
                }
            };
        }
    }

    fn set_property(&self, instance: &Value, property: String, value: Value) -> Result<()> {
        match instance {
            Value::Object(Object::Instance(instance)) => {
                let mut fields_mut = (*instance.fields).borrow_mut();
                fields_mut.insert(property, value);
            }
            Value::Object(Object::Receiver(instance, _)) => {
                let instance = &**instance;
                self.set_property(instance, property, value)?;
            }
            _ => bail!(self.runtime_error("Only instances have properties")),
        }
        Ok(())
    }

    fn get_property(
        &self,
        instance: &Value,
        property: &str,
    ) -> Result<(Value, Option<Rc<Closure>>)> {
        match instance {
            Value::Object(Object::Instance(instance)) => {
                let fields = (*instance.fields).borrow();
                if let Some(v) = fields.get(property) {
                    let v = v.clone();
                    drop(fields);
                    Ok((v, None))
                } else {
                    drop(fields);
                    let class = instance.class.clone();
                    let method = self.get_method(class, property)?;
                    let receiver = self.peek_at(0).clone(); // receiver
                    Ok((receiver, Some(method)))
                }
            }
            Value::Object(Object::Receiver(instance, _)) => self.get_property(instance, property),
            _ => bail!(self.runtime_error("Only instances have properties (to call '.' operator)")),
        }
    }

    fn get_method(&self, class: Rc<Class>, name: &str) -> Result<Rc<Closure>> {
        let method = (*class.methods).borrow();
        if let Some(c) = method.get(name) {
            Ok(c.clone())
        } else {
            bail!(self.runtime_error(&format!("Undefined property {}", name)))
        }
    }

    fn bind_method(&mut self, receiver: Value, method: Rc<Closure>) {
        let bound_method = match receiver {
            Value::Object(Object::Receiver(r, _)) => BoundMethod::new(r, method),
            _ => BoundMethod::new(Rc::new(receiver), method),
        };
        self.pop(); // remove the instance
        self.push(Value::Object(Object::BoundMethod(bound_method)));
    }

    fn define_method(&mut self, method_name: String) -> Result<()> {
        let method = self.peek_at(0).clone();
        match method {
            Value::Object(Object::Closure(closure)) => match self.peek_at(1) {
                Value::Object(Object::Class(class)) => {
                    let mut methods = (*class.methods).borrow_mut();
                    methods.insert(method_name, Rc::new(closure));
                }
                _ => bail!(self.runtime_error("VM BUG: Unreachable code")),
            },
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

    fn close_upvalues(&mut self, last_index: usize) {
        let upvalue_iter = self.up_values.iter().rev();
        let mut count = 0;
        upvalue_iter
            .map(|u| (**u).borrow_mut())
            .take_while(|u| match &u.location {
                Location::Stack(index) => *index >= last_index,
                _ => false,
            })
            .for_each(|mut u| {
                count += 1;
                if let Location::Stack(index) = u.location {
                    let value = self.stack[index].clone();
                    u.location = Location::Heap(value);
                }
            });
        let _captured_values = self.up_values.split_off(self.up_values.len() - count);
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

    fn call(&mut self, arg_count: u8) -> Result<()> {
        let arg_count = arg_count as usize;
        let value = self.peek_at(arg_count as usize);
        let start_index = self.stack_top - 1 - arg_count;
        match value {
            Value::Object(Object::Closure(c)) => {
                let function = c.function.clone();
                self.call_function(&function, arg_count, start_index)
            }
            Value::Object(Object::Class(c)) => {
                let class = c.clone();
                let methods = c.methods.clone();
                let receiver = Value::Object(Object::Instance(Instance::new(class)));
                if let Some(initializer) = (*methods).borrow().get("init") {
                    self.stack[start_index] =
                        Value::Object(Object::Receiver(Rc::new(receiver), initializer.clone()));
                    // receiver;
                    self.call_function(&initializer.function, arg_count, start_index)?;
                } else {
                    if arg_count != 0 {
                        bail!(self
                            .runtime_error(&format!("Expected 0  arguments but got {}", arg_count)))
                    }
                    self.stack[start_index] = receiver;
                }
                Ok(())
            }
            Value::Object(Object::BoundMethod(bound_method)) => {
                let closure = bound_method.closure.clone();
                let receiver = bound_method.receiver.clone();
                self.stack[start_index] =
                    Value::Object(Object::Receiver(receiver, closure.clone()));
                self.call_function(&closure.function, arg_count, start_index)
            }
            _ => bail!(self
                .runtime_error("can only call a function/closure, constructor or a class method")),
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
        self.call_frames.push(frame);
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
        }
        Ok(true)
    }

    #[inline]
    fn read_str_without_increment(&self) -> Result<&str> {
        let constant = self.read_constant_at(self.ip());
        match constant {
            Value::Object(Object::String(s)) => Ok(s),
            _ => Err(self.runtime_error("message").into()),
        }
    }

    #[inline]
    fn read_string(&mut self) -> Result<String> {
        let result = self.read_str_without_increment()?.to_string();
        self.increment_ip_by(1);
        Ok(result)
    }

    #[inline]
    fn read_function(&mut self) -> Result<Rc<Function>> {
        let constant = self.read_constant();
        match constant {
            Value::Object(Object::Function(s)) => Ok(s),
            _ => bail!(self.runtime_error("Not a function")),
        }
    }

    fn runtime_error(&self, message: &str) -> ErrorKind {
        let mut error_buf = vec![];
        writeln!(error_buf, "{}", message).expect("Write failed");
        for frame in (&self.call_frames).iter().rev() {
            let stack_index = frame.fn_start_stack_index;
            let function = &*self.closure_from_stack(stack_index).function;
            let fun_name = &function.to_string();
            match &*function {
                Function::UserDefined(u) => {
                    let ip = frame.ip;
                    let line_num = u.chunk.lines[ip];
                    writeln!(error_buf, "[line {}] in {}", line_num, fun_name)
                        .expect("Write failed")
                }
            };
        }
        let line = self.current_chunk().lines[self.ip()];
        runtime_vm_error(line, &utf8_to_string(&error_buf))
    }

    #[inline]
    fn peek_at(&self, distance: usize) -> &Value {
        let top = self.stack_top;
        &self.stack[top - 1 - distance]
    }

    #[inline]
    fn equals(&mut self) -> Result<bool> {
        let left = self.pop();
        let right = self.pop();
        value_equals(left, right)
    }

    #[inline]
    fn binary_op(&mut self, op: fn(f64, f64) -> Value) -> Result<()> {
        let (left, right) = (self.peek_at(1), self.peek_at(0));
        let (left, right) = match (left, right) {
            (Value::Number(l), Value::Number(r)) => (*l, *r),
            _ => bail!(self.runtime_error("Can perform binary operations only on numbers.")),
        };
        self.binary_op_with_num(left, right, op)
    }

    fn add(&mut self) -> Result<()> {
        match (self.peek_at(1), self.peek_at(0)) {
            (Value::Object(Object::String(left)), Value::Object(Object::String(right))) => {
                let mut concatenated_string = String::new();
                concatenated_string.push_str(left);
                concatenated_string.push_str(right);
                self.pop();
                self.pop();
                self.push(Value::Object(Object::String(concatenated_string)));
                Ok(())
            }
            (Value::Number(_), Value::Number(_)) => self.binary_op(|a, b| Value::Number(a + b)),
            _ => bail!(self.runtime_error(&format!(
                "Add can be perfomed only on numbers or strings, got {} and {}",
                self.peek_at(1),
                self.peek_at(0)
            ))),
        }
    }

    #[inline]
    fn binary_op_with_num(
        &mut self,
        left: f64,
        right: f64,
        op: fn(f64, f64) -> Value,
    ) -> Result<()> {
        let result = op(left, right);
        self.pop();
        self.pop();
        self.push(result);
        Ok(())
    }

    #[inline]
    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }
    #[inline]
    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        std::mem::take(&mut self.stack[self.stack_top])
    }

    pub fn free(&mut self) {
        //TODO
    }

    fn print_stack_value(&mut self, value: &Value) {
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
fn value_equals(l: Value, r: Value) -> Result<bool> {
    match (l, r) {
        (Value::Bool(l), Value::Bool(r)) => Ok(l == r),
        (Value::Nil, Value::Nil) => Ok(true),
        (Value::Number(l), Value::Number(r)) => Ok(num_equals(l, r)),
        (Value::Object(l), Value::Object(r)) => match (l, r) {
            (Object::String(l), Object::String(r)) => Ok(l == r),
            _ => Ok(false),
        },
        _ => Ok(false),
    }
}

fn is_falsey(value: &Value) -> bool {
    match value {
        Value::Bool(b) => !b,
        Value::Nil => true,
        _ => false,
    }
}

fn print_stack_value(value: &Value, writer: &mut dyn Write) {
    print_value(value, writer);
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use crate::{
        errors::*,
        lox::{init_logger_for_test, print_error, utf8_to_string},
    };

    use super::VirtualMachine;

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
        init_logger_for_test();
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

            dessert(item) {
                this.dessert = item;
                return this;
            }
        }
                  
        var brunch = Brunch("eggs", "coffee");

        var brunch_with_dessert = brunch.dessert("cake");
        
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
}

use std::cell::{Ref, RefMut};
use std::collections::LinkedList;
use std::convert::TryFrom;
use std::f64::EPSILON;
use std::io::{stdout, Write};
use std::time::Instant;

use log::{info, log_enabled, trace, Level};

use crate::chunk::Chunk;
use crate::compiler::Compiler;
use crate::errors::*;
use crate::instructions::{print_value, Opcode};
use crate::lox::{utf8_to_string, Shared, Writer};
use crate::objects::{shared, Function, Object, Value, Values};
use crate::scanner::Scanner;
use crate::tokens::pretty_print;
#[derive(Debug)]
pub struct CallFrame {
    function: Shared<Function>,
    starting_stack_pointer: usize,
}

impl CallFrame {
    pub fn new(function: Shared<Function>, starting_stack_pointer: usize) -> Self {
        CallFrame {
            function,
            starting_stack_pointer,
        }
    }
}

pub struct VirtualMachine<'a> {
    stack: Vec<Value>,
    call_frames: Vec<CallFrame>,
    call_ips: LinkedList<usize>,
    runtime_values: Values,
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

impl<'a> VirtualMachine<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        VirtualMachine::new_with_writer(None)
    }

    pub fn new_with_writer(custom_writer: Writer<'a>) -> Self {
        VirtualMachine {
            stack: Vec::new(),
            call_frames: Vec::new(),
            runtime_values: Values::new(),
            call_ips: LinkedList::new(),
            custom_writer,
        }
    }

    pub fn interpret(&mut self, source: String) -> Result<()> {
        let mut scanner = Scanner::new(source);
        let start_time = Instant::now();
        let tokens = scanner.scan_tokens()?;
        info!("Tokens created in {} us", start_time.elapsed().as_micros());
        if log_enabled!(Level::Trace) {
            pretty_print(tokens);
        }
        let start_time = Instant::now();
        let compiler = Compiler::new(tokens);
        let main_function = shared(compiler.compile()?);
        info!("Compiled in {} us", start_time.elapsed().as_micros());
        self.push(Value::Object(Object::Function(main_function.clone())));
        self.call_frames
            .push(CallFrame::new(main_function.clone(), 0));
        self.call(main_function, 0)?;
        let start_time = Instant::now();
        let result = self.run();
        info!("Ran in {} us", start_time.elapsed().as_micros());
        result
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
        self.call_frames
            .get_mut(len - 1 - index)
            .expect("Expected a call frame")
    }
    #[inline]
    fn call_frame_peek_at(&self, index: usize) -> &CallFrame {
        let len = self.call_frames.len();
        self.call_frames
            .get(len - 1 - index)
            .expect("Expected a call frame")
    }

    #[inline]
    fn ip(&self) -> usize {
        self.current_chunk().code.read_index
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
        self.current_chunk_mut().code.read_index = index;
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        let mut chunk = self.current_chunk_mut();
        let v = &mut chunk.code;
        let v = *v.read_and_increment();
        v
    }

    #[inline]
    fn current_chunk_mut(&mut self) -> RefMut<Chunk> {
        let function = self.current_function_mut();
        RefMut::map(function, |f| match f {
            Function::UserDefined(u) => &mut u.chunk,
        })
    }

    #[inline]
    fn current_chunk(&self) -> Ref<Chunk> {
        let function = self.current_function();
        Ref::map(function, |f| match f {
            Function::UserDefined(u) => &u.chunk,
        })
    }

    #[inline]
    fn current_function_mut(&mut self) -> RefMut<Function> {
        let v = self.call_frame_mut();
        let v = &*v.function;
        v.borrow_mut()
    }
    #[inline]
    fn current_function(&self) -> Ref<Function> {
        let v = self.call_frame();
        let v = &*v.function;
        v.borrow()
    }

    #[inline]
    fn read_short(&mut self) -> u16 {
        let first = *self.current_chunk_mut().code.read_and_increment() as u16;
        let second = *self.current_chunk_mut().code.read_and_increment() as u16;
        first << 8 | second
    }

    fn run(&mut self) -> Result<()> {
        // enable_log uncomment this line to enable logs for test
        let _ = env_logger::builder().is_test(true).try_init();
        loop {
            let mut buf = vec![];
            if log_enabled!(Level::Trace) {
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
                let sanitized_stack: Vec<String> =
                    self.stack.iter().map(|v| v.to_string()).collect();
                trace!("Current Stack: {:?}", sanitized_stack);
                let fun_name = (*self.current_function()).to_string();
                trace!(
                    "In function {} Next Instruction: [{}]",
                    fun_name,
                    utf8_to_string(&buf).trim()
                );
            }
            match instruction {
                Opcode::Constant => {
                    let mut chunk = self.current_chunk_mut();
                    let value = chunk.read_constant();
                    let constant = value.clone();
                    drop(value);
                    drop(chunk);
                    self.push(constant);
                }
                Opcode::Return => {
                    trace!("Call frame count: {}", self.call_frames.len());
                    let result = self.pop();
                    if self.call_frames.len() == 1 {
                        return Ok(());
                    }
                    let current_ip = self.call_ips.pop_back().expect("Expect ip");
                    let frame = self.call_frames.pop().expect("Expect frame");
                    self.current_chunk_mut().code.read_index = current_ip;
                    let fn_starting_pointer = frame.starting_stack_pointer;
                    // drop all the local values for the last function
                    self.stack.truncate(fn_starting_pointer);
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
                    let name = self.read_string()?;
                    let value = self.pop();
                    self.runtime_values.insert(name, value);
                }
                Opcode::GetGlobal => {
                    let name = self.read_string()?;
                    let value = self.runtime_values.get(&name);
                    if let Some(v) = value {
                        let v = v.clone();
                        self.push(v)
                    } else {
                        bail!(self
                            .runtime_error(&format!("Undefined variable '{}'", (*name).borrow())))
                    }
                }
                Opcode::SetGlobal => {
                    let name = self.read_string()?;
                    let value = self.peek_at(0).clone();
                    match self.runtime_values.get_mut(&name) {
                        Some(e) => {
                            *e = value;
                        }
                        None => {
                            bail!(self.runtime_error(&format!(
                                "Undefined variable '{}'",
                                (*name).borrow()
                            )))
                        }
                    }
                }
                Opcode::GetLocal => {
                    let index = self.read_byte();
                    let fn_start_pointer = self.call_frame().starting_stack_pointer;
                    let v = self.stack[fn_start_pointer + index as usize].clone();
                    self.push(v);
                }
                Opcode::SetLocal => {
                    let index = self.read_byte();
                    let fn_start_pointer = self.call_frame().starting_stack_pointer;
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
                    self.call_value(arg_count)?;
                }
            };
        }
    }

    fn call_value(&mut self, arg_count: u8) -> Result<()> {
        match self.peek_at(arg_count as usize) {
            Value::Object(Object::Function(f)) => {
                let function = f.clone();
                let starting_pointer = self.stack.len() - arg_count as usize - 1;
                self.call_ips.push_back(self.ip());
                let frame = CallFrame::new(function.clone(), starting_pointer);
                self.call_frames.push(frame);
                self.call(function, arg_count as usize)?;
                Ok(())
            }
            _ => bail!(self.runtime_error("Not a function")),
        }
    }

    fn call(&mut self, function: Shared<Function>, arg_count: usize) -> Result<bool> {
        let mut function = &mut *function.borrow_mut();
        match &mut function {
            Function::UserDefined(u) => {
                u.chunk.code.read_index = 0;
                if u.arity != arg_count {
                    self.call_frames.pop();
                    bail!(self.runtime_error(&format!(
                        "Expected {} arguments but got {}",
                        u.arity, arg_count
                    )))
                }
            }
        }
        Ok(true)
    }

    fn read_string(&mut self) -> Result<Shared<String>> {
        let mut chunk = self.current_chunk_mut();
        let constant = chunk.read_constant();
        let nil = &Object::Nil;
        let object = Ref::map(constant, |c| match c {
            Value::Object(o) => o,
            _ => nil,
        });
        if *object != *nil {
            match &*object {
                Object::String(s) => Ok(s.clone()),
                _ => {
                    drop(object);
                    drop(chunk);
                    bail!(self.runtime_error("Not a string"))
                }
            }
        } else {
            drop(object);
            drop(chunk);
            let line = self.current_chunk().current_line();
            bail!(runtime_vm_error(line, "Unable to read object"))
        }
    }

    fn runtime_error(&self, message: &str) -> ErrorKind {
        let mut error_buf = vec![];
        writeln!(error_buf, "{}", message).expect("Write failed");
        for frame in (&self.call_frames).iter().rev() {
            let function = (&*frame.function).borrow();
            let fun_name = &function.to_string();
            match &*function {
                Function::UserDefined(u) => {
                    let ip = u.chunk.code.read_index;
                    let line_num = u.chunk.lines[ip - 1];
                    // if log_enabled!(Level::Trace) {
                    //     u.chunk
                    //         .disassemble_chunk_with_writer(fun_name, &mut error_buf);
                    // }
                    writeln!(error_buf, "[line {}] in {}", line_num, fun_name)
                        .expect("Write failed")
                }
            };
        }
        runtime_vm_error(
            self.current_chunk().current_line(),
            &utf8_to_string(&error_buf),
        )
    }

    fn peek_at(&self, distance: usize) -> &Value {
        let top = self.stack.len();
        self.stack.get(top - 1 - distance).expect("Out of bounds")
    }

    fn equals(&mut self) -> Result<bool> {
        let left = self.pop();
        let right = self.pop();
        value_equals(left, right)
    }

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
                concatenated_string.push_str((&**left).borrow().as_str());
                concatenated_string.push_str((&**right).borrow().as_str());
                self.pop();
                self.pop();
                self.push(Value::Object(Object::string(concatenated_string)));
                Ok(())
            }
            (Value::Number(_), Value::Number(_)) => self.binary_op(|a, b| Value::Number(a + b)),
            _ => bail!(self.runtime_error("Add can be perfomed only on numbers or strings")),
        }
    }

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

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Cannot be empty")
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
    use crate::{
        errors::*,
        lox::{print_error, utf8_to_string},
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
    fn vn_string_expressions() -> Result<()> {
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
}

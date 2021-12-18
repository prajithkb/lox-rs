use std::convert::TryFrom;
use std::f64::EPSILON;
use std::io::{stdout, Write};
use std::time::Instant;

use log::{info, log_enabled, trace, Level};

use crate::chunk::{Chunk, Object, ObjectType, Value};
use crate::compiler::Compiler;
use crate::errors::*;
use crate::instructions::{print_value, Opcode};
use crate::lox::{utf8_to_string, Shared, Writer};
use crate::objects::Values;
use crate::scanner::Scanner;
use crate::tokens::pretty_print;

pub struct VirtualMachine<'a> {
    chunk: Chunk,
    stack: Vec<StackValue>,
    runtime_values: Values,
    custom_writer: Writer<'a>,
}

impl<'a> std::fmt::Debug for VirtualMachine<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VirtualMachine")
            .field("chunk", &self.chunk)
            .field("stack", &self.stack)
            .field("runtime_values", &self.runtime_values)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum StackValue {
    Owned(Value),
    Shared(Shared<Value>),
}

// impl StackValue {
//     fn as_shared(self) -> StackValue {
//         let v = Values::as_shared(self);
//         StackValue::Shared(v)
//     }
// }

impl Value {
    pub fn as_num(&self) -> Result<f64> {
        match &self {
            Value::Number(f) => Ok(*f),
            _ => bail!(ErrorKind::RuntimeError("Not a number".to_string())),
        }
    }

    pub fn as_string(&self) -> Result<String> {
        match &self {
            Value::Object(o) => match &o.object_type {
                ObjectType::String(s) => Ok(s.clone()),
            },
            _ => bail!(ErrorKind::RuntimeError("Not a String".to_string())),
        }
    }
}

impl<'a> VirtualMachine<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        VirtualMachine::new_with_writer(None)
    }

    pub fn new_with_writer(custom_writer: Writer<'a>) -> Self {
        VirtualMachine {
            chunk: Chunk::default(),
            stack: Vec::new(),
            runtime_values: Values::new(),
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
        self.chunk = compiler.compile()?;
        info!("Compiled in {} us", start_time.elapsed().as_micros());
        let start_time = Instant::now();
        self.chunk.code.set_current_index(0);
        let result = self.run();
        info!("Ran in {} us", start_time.elapsed().as_micros());
        result
    }

    #[inline]
    fn ip(&self) -> usize {
        self.chunk.code.read_index
    }

    #[inline]
    fn increment_ip_by(&mut self, offset: u16) {
        let ip = self.ip();
        self.set_ip(ip + offset as usize);
    }

    #[inline]
    fn set_ip(&mut self, index: usize) {
        self.chunk.code.read_index = index;
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        *self.chunk.code.read_and_increment()
    }

    #[inline]
    fn read_short(&mut self) -> u16 {
        let first = *self.chunk.code.read_and_increment() as u16;
        let second = *self.chunk.code.read_and_increment() as u16;
        first << 8 | second
    }

    fn run(&mut self) -> Result<()> {
        loop {
            let mut buf = vec![];
            // if log_enabled!(Level::Trace) {
            self.chunk
                .disassemble_instruction_with_writer(self.ip(), &mut buf);
            // }
            let byte = self.read_byte();
            let instruction: Opcode = Opcode::try_from(byte).map_err(|e| {
                self.runtime_error(&format!(
                    "Invalid instruction (byte:{}) at {}, error: {}",
                    byte,
                    self.ip(),
                    e
                ))
            })?;
            trace!(
                "Stack: <{:?}>, heap: <{:?}>",
                self.stack,
                self.runtime_values
            );
            trace!("Instruction: [{}]", utf8_to_string(&buf).trim());
            match instruction {
                Opcode::Constant => {
                    let constant = self.read_constant().clone();
                    self.push(StackValue::Owned(constant.clone()));
                }
                Opcode::Return => {
                    return Ok(());
                }
                Opcode::Negate => {
                    if let StackValue::Owned(Value::Number(v)) = self.peek_at(0) {
                        let result = Value::Number(-*v);
                        self.push(StackValue::Owned(result));
                    } else {
                        bail!(self.runtime_error("Can only negate numbers."));
                    }
                }
                Opcode::Add => {
                    if self.str_add().is_err()
                        && self.binary_op(|a, b| Value::Number(a + b)).is_err()
                    {
                        bail!(self
                            .runtime_error("Can perform '+' only if both are numbers or strings"))
                    }
                }
                Opcode::Subtract => self.binary_op(|a, b| Value::Number(a - b))?,
                Opcode::Multiply => self.binary_op(|a, b| Value::Number(a * b))?,
                Opcode::Divide => self.binary_op(|a, b| Value::Number(a / b))?,
                Opcode::Nil => self.push(StackValue::Owned(Value::Nil)),
                Opcode::True => self.push(StackValue::Owned(Value::Bool(true))),
                Opcode::False => self.push(StackValue::Owned(Value::Bool(false))),
                Opcode::Not => {
                    let v = self.pop();
                    self.push(StackValue::Owned(Value::Bool(is_falsey(&v))))
                }
                Opcode::BangEqual => {
                    let v = self.equals()?;
                    self.push(StackValue::Owned(Value::Bool(!v)))
                }
                Opcode::Greater => self.binary_op(|a, b| Value::Bool(a > b))?,
                Opcode::GreaterEqual => self.binary_op(|a, b| Value::Bool(a >= b))?,
                Opcode::Less => self.binary_op(|a, b| Value::Bool(a < b))?,
                Opcode::LessEqual => self.binary_op(|a, b| Value::Bool(a <= b))?,
                Opcode::EqualEqual => {
                    let v = self.equals()?;
                    self.push(StackValue::Owned(Value::Bool(v)))
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
                    let name = self.read_string()?.to_string();
                    let value = self.pop();
                    self.runtime_values.insert(name, value);
                }
                Opcode::GetGlobal => {
                    let name = self.read_string()?.to_string();
                    let value = self.runtime_values.get(&name);
                    if let Some(v) = value {
                        let v = v.clone();
                        self.push(StackValue::Shared(v))
                    } else {
                        bail!(self.runtime_error(&format!("Undefined variable {}", name)))
                    }
                }
                Opcode::SetGlobal => {
                    let name = self.read_string()?.to_string();
                    let value = self.pop();
                    match self.runtime_values.get_mut(&name) {
                        Some(e) => {
                            let v = Values::as_shared(value);
                            *e = v.clone();
                            self.push(StackValue::Shared(v));
                        }
                        None => {
                            // push the value back
                            self.push(value);
                            bail!(self.runtime_error(&format!("Undefined variable {}", name)))
                        }
                    }
                }
                Opcode::GetLocal => {
                    let index = self.read_byte();
                    let v = self.stack[index as usize].clone();
                    self.push(v);
                }
                Opcode::SetLocal => {
                    let index = self.read_byte();
                    self.stack[index as usize] = self.peek_at(0).clone();
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
            };
        }
    }

    // fn convert_to_shared(&mut self, index: usize) -> StackValue {
    //     let v = &self.stack[index as usize];
    //     match v {
    //         StackValue::Owned(_) => {
    //             let v = std::mem::replace(
    //                 &mut self.stack[index as usize],
    //                 StackValue::Owned(Value::Nil),
    //             );
    //             let shared_value = v.as_shared();
    //             let _ = std::mem::replace(&mut self.stack[index as usize], shared_value);
    //             self.stack[index as usize].clone()
    //         }
    //         StackValue::Shared(v) => StackValue::Shared(v.clone()),
    //     }
    // }

    fn read_string(&mut self) -> Result<&str> {
        let v = self.read_object()?;
        match &v.object_type {
            ObjectType::String(s) => Ok(s),
        }
    }

    fn read_object(&mut self) -> Result<&Object> {
        let line = self.chunk.current_line();
        let c = self.read_constant();
        if let Value::Object(c) = c {
            Ok(c)
        } else {
            bail!(runtime_vm_error(line, "Unable to read object"))
        }
    }
    fn runtime_error(&self, message: &str) -> ErrorKind {
        runtime_vm_error(self.chunk.current_line(), message)
    }

    fn peek_at(&self, distance: usize) -> &StackValue {
        let top = self.stack.len();
        self.stack.get(top - 1 - distance).expect("Out of bounds")
    }

    fn equals(&mut self) -> Result<bool> {
        let left = self.pop();
        let right = self.pop();
        match (left, right) {
            (StackValue::Owned(left), StackValue::Owned(right)) => value_equals(&left, &right),
            (StackValue::Shared(left), StackValue::Shared(right)) => {
                value_equals(&*left.borrow(), &*right.borrow())
            }
            (StackValue::Shared(left), StackValue::Owned(right)) => {
                value_equals(&*left.borrow(), &right)
            }
            (StackValue::Owned(left), StackValue::Shared(right)) => {
                value_equals(&left, &*right.borrow())
            }
        }
    }

    fn binary_op(&mut self, op: fn(f64, f64) -> Value) -> Result<()> {
        let (left, right) = match (self.peek_at(1), self.peek_at(0)) {
            (StackValue::Owned(l), StackValue::Owned(r)) => {
                let left = l.as_num();
                let right = r.as_num();
                (left, right)
            }

            (StackValue::Owned(l), StackValue::Shared(r)) => {
                let left = l.as_num();
                let right = r.borrow().as_num();
                (left, right)
            }
            (StackValue::Shared(l), StackValue::Owned(r)) => {
                let left = l.borrow().as_num();
                let right = r.as_num();
                (left, right)
            }
            (StackValue::Shared(l), StackValue::Shared(r)) => {
                let left = l.borrow().as_num();
                let right = r.borrow().as_num();
                (left, right)
            }
        };
        if let (Ok(left), Ok(right)) = (left, right) {
            self.binary_op_with_num(left, right, op)
        } else {
            bail!(self.runtime_error("Can perform binary operations only on numbers."))
        }
    }

    fn str_add(&mut self) -> Result<()> {
        let (left, right) = match (self.peek_at(1), self.peek_at(0)) {
            (StackValue::Owned(l), StackValue::Owned(r)) => {
                let left = l.as_string();
                let right = r.as_string();
                (left, right)
            }

            (StackValue::Owned(l), StackValue::Shared(r)) => {
                let left = l.as_string();
                let right = r.borrow().as_string();
                (left, right)
            }
            (StackValue::Shared(l), StackValue::Owned(r)) => {
                let left = l.borrow().as_string();
                let right = r.as_string();
                (left, right)
            }
            (StackValue::Shared(l), StackValue::Shared(r)) => {
                let left = l.borrow().as_string();
                let right = r.borrow().as_string();
                (left, right)
            }
        };
        if let (Ok(left), Ok(right)) = (left, right) {
            self.strings_add_with_str(left, right);
            Ok(())
        } else {
            bail!(self.runtime_error("Can perform binary operations only on numbers."))
        }
    }

    fn strings_add_with_str(&mut self, mut l: String, r: String) {
        self.pop();
        self.pop();
        l.push_str(&r);
        self.push(StackValue::Owned(Value::Object(Object::new(
            1,
            ObjectType::String(l),
        ))));
    }

    fn binary_op_with_num(
        &mut self,
        left: f64,
        right: f64,
        op: fn(f64, f64) -> Value,
    ) -> Result<()> {
        let result = StackValue::Owned(op(left, right));
        self.pop();
        self.pop();
        self.push(result);
        Ok(())
    }

    fn read_constant(&mut self) -> &Value {
        self.chunk.read_constant()
    }

    fn push(&mut self, value: StackValue) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> StackValue {
        self.stack.pop().expect("Cannot be empty")
    }

    // fn print_stack_trace(&self) {
    //     print!("          ");
    //     for v in &self.stack {
    //         print!("[");
    //         print_value(*v);
    //         print!("]");
    //     }
    //     println!()
    // }

    pub fn free(&mut self) {
        self.chunk.free_all();
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

fn equals(l: f64, r: f64) -> bool {
    (l - r).abs() < EPSILON
}
fn value_equals(l: &Value, r: &Value) -> Result<bool> {
    match (l, r) {
        (Value::Bool(l), Value::Bool(r)) => Ok(l == r),
        (Value::Nil, Value::Nil) => Ok(true),
        (Value::Number(l), Value::Number(r)) => Ok(equals(*l, *r)),
        (Value::Object(l), Value::Object(r)) => match (&l.object_type, &r.object_type) {
            (ObjectType::String(l), ObjectType::String(r)) => Ok(l == r),
        },
        _ => Ok(false),
    }
}

fn is_falsey(value: &StackValue) -> bool {
    match value {
        StackValue::Owned(Value::Bool(b)) => !b,
        StackValue::Owned(Value::Nil) => true,
        StackValue::Shared(v) => match *v.borrow() {
            Value::Bool(b) => !b,
            Value::Nil => true,
            _ => false,
        },
        _ => false,
    }
}

fn print_stack_value(value: &StackValue, writer: &mut dyn Write) {
    match value {
        StackValue::Owned(v) => print_value(v, writer),
        StackValue::Shared(v) => print_value(&v.borrow(), writer),
    }
}

#[cfg(test)]
mod tests {
    use crate::{errors::*, lox::utf8_to_string};

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
                "Runtime Error: Line: 10, message: Undefined variable b",
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
}

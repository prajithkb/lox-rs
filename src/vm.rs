use std::convert::TryFrom;
use std::f64::EPSILON;
use std::io::stdout;
use std::time::Instant;

use log::{info, log_enabled, trace, Level};

use crate::chunk::{Chunk, Value};
use crate::compiler::Compiler;
use crate::errors::*;
use crate::instructions::{print_value, Opcode};
use crate::scanner::Scanner;
use crate::tokens::pretty_print;

#[derive(Debug)]
pub struct VirtualMachine {
    chunk: Chunk,
    stack: Vec<Value>,
}

impl VirtualMachine {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        VirtualMachine {
            chunk: Chunk::default(),
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, source: String) -> Result<()> {
        let mut scanner = Scanner::new(source);
        let start_time = Instant::now();
        let tokens = scanner.scan_tokens()?;
        info!("Tokens created in {} us", start_time.elapsed().as_micros());
        pretty_print(tokens);
        let start_time = Instant::now();
        let compiler = Compiler::new(tokens);
        self.chunk = compiler.compile()?;
        info!("Compiled in {} us", start_time.elapsed().as_micros());
        if log_enabled!(Level::Trace) {
            self.chunk.disassemble_chunk("code");
        }
        let start_time = Instant::now();
        self.chunk.code.set_current_index(0);
        self.run()?;
        info!("Ran in {} us", start_time.elapsed().as_micros());
        Ok(())
    }

    #[inline]
    fn ip(&self) -> usize {
        self.chunk.code.current_index
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        *self.chunk.code.read_and_increment()
    }

    fn run(&mut self) -> Result<()> {
        loop {
            if log_enabled!(Level::Trace) {
                self.chunk.disassemble_instruction(self.ip());
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
            trace!("VM Internal state: {:?}, {:?}", instruction, self);
            match instruction {
                Opcode::Constant => {
                    let constant = self.read_constant().clone();
                    self.push(constant.clone());
                }
                Opcode::Return => {
                    print_value(&self.pop(), &mut stdout());
                    println!();
                    return Ok(());
                }
                Opcode::Negate => {
                    if let Value::Number(v) = self.peek_at(0) {
                        let result = Value::Number(-*v);
                        self.push(result);
                    } else {
                        bail!(self.runtime_error("Can only negate numbers."));
                    }
                }
                Opcode::Add => self.binary_op(|a, b| Value::Number(a + b))?,
                Opcode::Subtract => self.binary_op(|a, b| Value::Number(a - b))?,
                Opcode::Multiply => self.binary_op(|a, b| Value::Number(a * b))?,
                Opcode::Divide => self.binary_op(|a, b| Value::Number(a / b))?,
                Opcode::Nil => self.push(Value::Nil),
                Opcode::True => self.push(Value::Bool(true)),
                Opcode::False => self.push(Value::Bool(false)),
                Opcode::Not => {
                    let v = self.pop();
                    self.push(Value::Bool(is_falsey(v)))
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
            };
        }
    }

    fn runtime_error(&self, message: &str) -> ErrorKind {
        runtime_vm_error(self.chunk.current_line(), message)
    }

    fn peek_at(&self, distance: usize) -> &Value {
        let top = self.stack.len();
        self.stack.get(top - 1 - distance).expect("Out of bounds")
    }

    fn equals(&mut self) -> Result<bool> {
        let left = self.pop();
        let right = self.pop();
        match (left, right) {
            (Value::Bool(l), Value::Bool(r)) => Ok(l == r),
            (Value::Nil, Value::Nil) => Ok(true),
            (Value::Number(l), Value::Number(r)) => Ok((l - r).abs() < EPSILON),
            _ => Ok(false),
        }
    }

    #[inline]
    fn binary_op(&mut self, op: fn(f64, f64) -> Value) -> Result<()> {
        if let (Value::Number(right), Value::Number(left)) = (self.peek_at(0), self.peek_at(1)) {
            let result = op(*left, *right);
            self.push(result);
        } else {
            bail!(self.runtime_error("Can perform binary operations only on numbers."))
        }
        Ok(())
    }
    fn read_constant(&mut self) -> Value {
        self.chunk.read_constant().clone()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
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
}

fn runtime_vm_error(line: usize, message: &str) -> ErrorKind {
    ErrorKind::RuntimeError(format!("Line: {}, message: {}", line, message))
}

fn is_falsey(value: Value) -> bool {
    match value {
        Value::Bool(b) => !b,
        Value::Nil => true,
        _ => false,
    }
}

use std::convert::TryFrom;
use std::io::stdout;
use std::time::Instant;

use log::{info, log_enabled, trace, Level};

use crate::chunk::{Chunk, Opcode, Value};
use crate::compiler::Compiler;
use crate::errors::*;
use crate::instructions::print_value;
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
        self.chunk.code.read_and_increment()
    }

    fn run(&mut self) -> Result<()> {
        loop {
            if log_enabled!(Level::Trace) {
                self.chunk.disassemble_instruction(self.ip());
            }
            let byte = self.read_byte();
            let instruction: Opcode = Opcode::try_from(byte).map_err(|e| {
                runtime_vm_error(
                    self.chunk.current_line(),
                    &format!(
                        "Invalid instruction (byte:{}) at {}, error: {}",
                        byte,
                        self.ip(),
                        e
                    ),
                )
            })?;
            trace!("VM Internal state: {:?}, {:?}", instruction, self);
            match instruction {
                Opcode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                Opcode::Return => {
                    print_value(self.pop(), &mut stdout());
                    println!();
                    return Ok(());
                }
                Opcode::Negate => {
                    let v = self.pop();
                    self.push(-v)
                }
                Opcode::Add => self.binary_op(|a, b| a + b),
                Opcode::Subtract => self.binary_op(|a, b| a - b),
                Opcode::Multiply => self.binary_op(|a, b| a * b),
                Opcode::Divide => self.binary_op(|a, b| a / b),
            }
        }
    }

    #[inline]
    fn binary_op(&mut self, op: fn(Value, Value) -> Value) {
        let right = self.pop();
        let left = self.pop();
        self.push(op(left, right));
    }

    fn read_constant(&mut self) -> Value {
        self.chunk.read_constant()
    }

    // fn reset_stack(&mut self) {
    //     self.stack_top = 0;
    // }

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

    fn free(&mut self) {
        self.chunk.free_all();
    }
}

fn runtime_vm_error(line: usize, message: &str) -> ErrorKind {
    ErrorKind::VMRuntimeError(format!("Line: {}, message: {}", line, message))
}

// fn repl() -> Result<()> {}

// fn run_script(path: &str) -> Result<()> {}

pub fn vm_main() -> Result<()> {
    let mut chunk = Chunk::new();

    // -((1.2 + 3.4)/5.6)
    let constant = chunk.add_constant(1.2);
    chunk.write_chunk(Opcode::Constant.into(), 123);
    chunk.write_chunk(constant as u8, 123);

    let constant = chunk.add_constant(3.4);
    chunk.write_chunk(Opcode::Constant.into(), 123);
    chunk.write_chunk(constant as u8, 123);

    chunk.write_chunk(Opcode::Add.into(), 123);

    let constant = chunk.add_constant(5.6);
    chunk.write_chunk(Opcode::Constant.into(), 123);
    chunk.write_chunk(constant as u8, 123);

    chunk.write_chunk(Opcode::Divide.into(), 123);

    chunk.write_chunk(Opcode::Negate.into(), 123);

    chunk.write_chunk(Opcode::Return.into(), 123);
    chunk.disassemble_chunk("test chunk");
    let mut vm = VirtualMachine::new();
    vm.chunk = chunk;
    vm.run()?;
    vm.free();
    Ok(())
}

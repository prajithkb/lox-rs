use std::convert::TryFrom;

use log::{log_enabled, trace, Level};

use crate::chunk::{Chunk, Opcode, Value};
use crate::errors::*;
use crate::instructions::print_value;

#[derive(Debug)]
pub struct VirtualMachine {
    chunk: Chunk,
    stack: Vec<Value>,
}

impl VirtualMachine {
    pub fn new(mut chunk: Chunk) -> Self {
        chunk.code.set_current_index(0);
        VirtualMachine {
            chunk,
            stack: Vec::new(),
        }
    }
    pub fn interpret(&mut self) -> Result<()> {
        self.run()
    }
    // #[inline]
    // fn set_ip(&mut self, offset: usize) {
    //     self.chunk.code.current_index = offset;
    // }

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
                Opcode::OpConstant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                Opcode::OpReturn => {
                    print_value(self.pop());
                    println!();
                    return Ok(());
                }
                Opcode::OpNegate => {
                    let v = self.pop();
                    self.push(-v)
                }
                Opcode::OpAdd => self.binary_op(|a, b| a + b),
                Opcode::OpSubtract => self.binary_op(|a, b| a - b),
                Opcode::OpMultiply => self.binary_op(|a, b| a * b),
                Opcode::OpDivide => self.binary_op(|a, b| a / b),
            }
        }
    }

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

pub fn vm_main() -> Result<()> {
    let mut chunk = Chunk::new();

    // -((1.2 + 3.4)/5.6)
    let constant = chunk.add_constant(1.2);
    chunk.write_chunk(Opcode::OpConstant.into(), 123);
    chunk.write_chunk(constant as u8, 123);

    let constant = chunk.add_constant(3.4);
    chunk.write_chunk(Opcode::OpConstant.into(), 123);
    chunk.write_chunk(constant as u8, 123);

    chunk.write_chunk(Opcode::OpAdd.into(), 123);

    let constant = chunk.add_constant(5.6);
    chunk.write_chunk(Opcode::OpConstant.into(), 123);
    chunk.write_chunk(constant as u8, 123);

    chunk.write_chunk(Opcode::OpDivide.into(), 123);

    chunk.write_chunk(Opcode::OpNegate.into(), 123);

    chunk.write_chunk(Opcode::OpReturn.into(), 123);
    let mut vm = VirtualMachine::new(chunk);
    vm.interpret()?;
    vm.free();
    Ok(())
}

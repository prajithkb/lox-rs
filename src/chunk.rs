use std::convert::TryFrom;

use crate::instructions::{constant_instruction, simple_instruction};
use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Debug, PartialEq, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum Opcode {
    OpConstant,
    OpReturn,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
}

pub type Value = f64;

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Memory<u8>,
    pub constants: Memory<Value>,
    pub lines: Vec<usize>,
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Memory::new(),
            constants: Memory::new(),
            lines: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.write_item(value);
        // /After we add the constant, we return the index where the constant was appended
        // so that we can locate that same constant later.
        self.constants.count - 1
    }

    // #[inline]
    pub fn read_constant(&mut self) -> Value {
        let offset = self.code.read_and_increment();
        self.constants.read_item_at(offset as usize)
    }

    pub fn disassemble_chunk(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < self.code.count {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.lines[offset - 1] == self.lines[offset] {
            print!("   | ");
        } else {
            print!("{:04} ", self.lines[offset]);
        }
        let byte = self.code.read_item_at(offset);

        match Opcode::try_from(byte) {
            Ok(instruction) => match instruction {
                Opcode::OpConstant => constant_instruction("OP_CONSTANT", self, offset),
                _ => simple_instruction(&format!("{:?}", instruction), offset),
            },
            Err(e) => {
                eprintln!(
                    "Invalid instruction {:?}[value={}], error: {}",
                    byte, offset, e
                );
                offset + 1
            }
        }
    }
    pub fn write_chunk(&mut self, byte: u8, line: usize) {
        self.code.write_item(byte);
        self.lines.push(line);
    }
    pub fn free_code(&mut self) {
        self.code.free_items();
    }

    pub fn free_data(&mut self) {
        self.constants.free_items();
    }

    pub fn free_all(&mut self) {
        self.free_code();
        self.free_data();
    }

    pub fn current_line(&self) -> usize {
        self.lines[self.code.current_index]
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Memory<T: Copy> {
    inner: Vec<T>,
    pub count: usize,
    pub current_index: usize,
}

impl<T: Copy> Memory<T> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Memory {
            inner: vec![],
            count: 0,
            current_index: 0,
        }
    }

    // #[inline]
    pub fn write_item(&mut self, byte: T) {
        self.inner.push(byte);
        self.count += 1;
    }

    pub fn free_items(&mut self) {
        self.inner.clear();
        self.count = 0;
    }

    #[inline]
    pub fn set_current_index(&mut self, index: usize) {
        self.current_index = index
    }
    #[inline]
    pub fn read_item_at(&self, index: usize) -> T {
        self.inner[index]
    }

    #[inline]
    pub fn read(&self) -> T {
        self.inner[self.current_index]
    }

    pub fn read_and_increment(&mut self) -> T {
        let v = self.read();
        self.current_index += 1;
        v
    }

    // #[inline]
    pub fn count_as_mut(&mut self) -> &mut usize {
        &mut self.count
    }
}

use crate::instructions::{constant_instruction, simple_instruction};

#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Opcode {
    Invalid = 255,
    OpConstant = 0,
    OpReturn = 1,
}

impl Opcode {
    pub fn as_u8(&self) -> u8 {
        *self as u8
    }
}

impl From<u8> for Opcode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => Opcode::OpConstant,
            1 => Opcode::OpReturn,
            _ => Opcode::Invalid,
        }
    }
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

    pub fn disassemble_chunk(&self, name: &str) {
        println!("== {} ==", name);
        println!("addr ");
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
        let instruction: Opcode = self.code.item_at(offset).into();
        match instruction {
            Opcode::OpReturn => simple_instruction("OP_RETURN", offset),
            Opcode::OpConstant => constant_instruction("OP_CONSTANT", self, offset),
            Opcode::Invalid => {
                eprintln!("Invalid instruction {:?}[value={}]", instruction, offset);
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct Memory<T: Copy> {
    inner: Vec<T>,
    count: usize,
    capacity: usize,
}

impl<T: Copy> Memory<T> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Memory {
            inner: vec![],
            count: 0,
            capacity: 0,
        }
    }

    pub fn write_item(&mut self, byte: T) {
        self.inner.push(byte);
        self.count += 1;
        if self.count > self.capacity {
            self.capacity *= 2;
        }
    }

    pub fn free_items(&mut self) {
        self.inner.clear();
        self.count = 0;
        self.capacity = 0;
    }
    pub fn item_at(&self, index: usize) -> T {
        self.inner[index]
    }
}

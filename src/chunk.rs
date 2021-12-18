use std::{
    convert::TryFrom,
    fmt::Display,
    io::{stdout, Write},
};

use crate::instructions::{constant_instruction, simple_instruction, Opcode};

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Object(Object),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => f.write_str(&b.to_string()),
            Value::Nil => f.write_str("nil"),
            Value::Number(n) => f.write_str(&n.to_string()),
            Value::Object(o) => match &o.object_type {
                ObjectType::String(s) => f.write_str(s),
            },
        }
    }
}
#[derive(Debug, Clone)]
pub struct Object {
    pub id: usize,
    pub object_type: ObjectType,
}

impl Object {
    pub fn new(id: usize, object_type: ObjectType) -> Self {
        Object { id, object_type }
    }
}

#[derive(Debug, Clone)]
pub enum ObjectType {
    String(String),
}

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

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.write_item(value);
        // /After we add the constant, we return the index where the constant was appended
        // so that we can locate that same constant later.
        (self.constants.count - 1) as u8
    }

    #[inline]
    pub fn read_constant(&mut self) -> &Value {
        let offset = *self.code.read_and_increment();
        self.constants.read_item_at(offset as usize)
    }

    pub fn disassemble_chunk(&self, name: &str) {
        self.disassemble_chunk_with_writer(name, &mut stdout());
    }

    pub fn disassemble_chunk_with_writer(&self, name: &str, writer: &mut dyn Write) {
        writeln!(writer, "== {} ==", name).expect("Write failed");
        let mut offset = 0;
        while offset < self.code.count {
            offset = self.disassemble_instruction_with_writer(offset, writer);
        }
    }

    pub fn disassemble_instruction_with_writer(
        &self,
        offset: usize,
        writer: &mut dyn Write,
    ) -> usize {
        write!(writer, "{:04} ", offset).expect("Write failed");
        if offset > 0 && self.lines[offset - 1] == self.lines[offset] {
            write!(writer, "   | ").expect("Write failed");
        } else {
            write!(writer, "{:04} ", self.lines[offset]).expect("Write failed");
        }
        let byte = *self.code.read_item_at(offset);

        match Opcode::try_from(byte) {
            Ok(instruction) => match instruction {
                Opcode::Constant | Opcode::DefineGlobal | Opcode::GetGlobal | Opcode::SetGlobal => {
                    constant_instruction(&instruction.to_string(), self, offset, writer)
                }
                _ => simple_instruction(&instruction.to_string(), offset, writer),
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
    #[allow(dead_code)]
    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        self.disassemble_instruction_with_writer(offset, &mut stdout())
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
pub struct Memory<T> {
    inner: Vec<T>,
    pub count: usize,
    pub current_index: usize,
}

impl<T> Memory<T> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Memory {
            inner: vec![],
            count: 0,
            current_index: 0,
        }
    }

    #[inline]
    pub fn write_item(&mut self, byte: T) {
        self.inner.push(byte);
        self.count += 1;
    }

    #[inline]
    pub fn set_current_index(&mut self, index: usize) {
        self.current_index = index
    }
    #[inline]
    pub fn read_item_at(&self, index: usize) -> &T {
        self.inner.get(index).expect("Index out of bounds")
    }

    #[inline]
    pub fn read_and_increment(&mut self) -> &T {
        self.current_index += 1;
        self.read_item_at(self.current_index - 1)
    }

    pub fn free_items(&mut self) {
        self.inner.clear();
        self.count = 0;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        chunk::{Chunk, Value},
        errors::*,
        instructions::Opcode,
        lox::utf8_to_string,
    };

    #[test]
    fn test_chunk() -> Result<()> {
        let mut chunk = Chunk::new();

        // -((1.2 + 3.4)/5.6)
        let constant = chunk.add_constant(Value::Number(1.2));
        chunk.write_chunk(Opcode::Constant.into(), 123);
        chunk.write_chunk(constant as u8, 123);

        let constant = chunk.add_constant(Value::Number(3.4));
        chunk.write_chunk(Opcode::Constant.into(), 123);
        chunk.write_chunk(constant as u8, 123);

        chunk.write_chunk(Opcode::Add.into(), 123);

        let constant = chunk.add_constant(Value::Number(5.6));
        chunk.write_chunk(Opcode::Constant.into(), 123);
        chunk.write_chunk(constant as u8, 123);

        chunk.write_chunk(Opcode::Divide.into(), 123);

        chunk.write_chunk(Opcode::Negate.into(), 123);

        chunk.write_chunk(Opcode::Return.into(), 123);
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!("== test ==\n0000 0123 OpCode[Constant]    0 '1.2'\n0002    | OpCode[Constant]    1 '3.4'\n0004    | OpCode[Add]\n0005    | OpCode[Constant]    2 '5.6'\n0007    | OpCode[Divide]\n0008    | OpCode[Negate]\n0009    | OpCode[Return]\n", utf8_to_string(&buf));
        Ok(())
    }
}

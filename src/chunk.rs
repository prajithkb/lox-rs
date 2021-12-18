use std::{
    fmt::Display,
    io::{stdout, Write},
};

use crate::instructions::{self};

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
        instructions::disassemble_instruction(byte, self, offset, writer)
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
        self.lines[self.code.read_index]
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Memory<T> {
    inner: Vec<T>,
    pub count: usize,
    pub read_index: usize,
}

impl<T> Memory<T> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Memory {
            inner: vec![],
            count: 0,
            read_index: 0,
        }
    }

    pub fn values_mut(&mut self) -> &mut Vec<T> {
        &mut self.inner
    }

    #[inline]
    pub fn write_item(&mut self, item: T) {
        self.inner.push(item);
        self.count += 1;
    }

    #[inline]
    pub fn set_current_index(&mut self, index: usize) {
        self.read_index = index
    }
    #[inline]
    pub fn read_item_at(&self, index: usize) -> &T {
        self.inner.get(index).expect("Index out of bounds")
    }

    #[inline]
    pub fn read_and_increment(&mut self) -> &T {
        self.read_index += 1;
        self.read_item_at(self.read_index - 1)
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
        assert_eq!(
            r#"== test ==
0000 0123 OpCode[Constant]                  0 '1.2'
0002    | OpCode[Constant]                  1 '3.4'
0004    | OpCode[Add]
0005    | OpCode[Constant]                  2 '5.6'
0007    | OpCode[Divide]
0008    | OpCode[Negate]
0009    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }
}

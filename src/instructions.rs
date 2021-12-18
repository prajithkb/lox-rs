use std::{fmt::Display, io::Write};

use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::chunk::{Chunk, Value};

#[derive(Debug, PartialEq, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum Opcode {
    Constant,
    Return,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Nil,
    True,
    False,
    Not,
    EqualEqual,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("OpCode[{:?}]", self))
    }
}

pub fn simple_instruction(instruction: &Opcode, offset: usize, writer: &mut dyn Write) -> usize {
    writeln!(writer, "{}", instruction).expect("Write failed");
    offset + 1
}

pub fn constant_instruction(
    instruction: &Opcode,
    chunk: &Chunk,
    offset: usize,
    writer: &mut dyn Write,
) -> usize {
    let constant = *chunk.code.read_item_at(offset + 1);
    write!(writer, "{:<16} {:4} '", instruction, constant).expect("Write failed");
    print_value(chunk.constants.read_item_at(constant as usize), writer);
    writeln!(writer, "'").expect("Write failed");
    offset + 2
}

pub fn byte_instruction(
    instruction: &Opcode,
    chunk: &Chunk,
    offset: usize,
    writer: &mut dyn Write,
) -> usize {
    let slot = *chunk.code.read_item_at(offset + 1);
    writeln!(writer, "{:<16} {:4}", instruction, slot).expect("Write failed");
    offset + 2
}

pub fn print_value(value: &Value, writer: &mut dyn Write) {
    write!(writer, "{}", value).expect("Write failed");
}

use std::{convert::TryFrom, fmt::Display, io::Write};

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
    JumpIfFalse,
    Jump,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("OpCode[{:?}]", self))
    }
}

pub fn simple_instruction(instruction: &Opcode, offset: usize, writer: &mut dyn Write) -> usize {
    writeln!(writer, "{}", instruction.to_string()).expect("Write failed");
    offset + 1
}

pub fn constant_instruction(
    instruction: &Opcode,
    chunk: &Chunk,
    offset: usize,
    writer: &mut dyn Write,
) -> usize {
    let constant = *chunk.code.read_item_at(offset + 1);
    write!(writer, "{:<30} {:4} '", instruction.to_string(), constant).expect("Write failed");
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
    writeln!(writer, "{:<30} {:4}", instruction.to_string(), slot).expect("Write failed");
    offset + 2
}

pub fn jump_instruction(
    instruction: &Opcode,
    chunk: &Chunk,
    sign: i32,
    offset: usize,
    writer: &mut dyn Write,
) -> usize {
    let mut jump = as_u16(*chunk.code.read_item_at(offset + 1)) << 8;
    jump |= as_u16(*chunk.code.read_item_at(offset + 2));
    writeln!(
        writer,
        "{:<30} {:4} -> {}",
        instruction.to_string(),
        offset,
        (offset as i32) + 3 + (jump as i32) * sign
    )
    .expect("Write failed");
    offset + 3
}

fn as_u16(i: u8) -> u16 {
    i as u16
}

pub fn print_value(value: &Value, writer: &mut dyn Write) {
    write!(writer, "{}", value).expect("Write failed");
}

pub fn disassemble_instruction(
    byte: u8,
    chunk: &Chunk,
    offset: usize,
    writer: &mut dyn Write,
) -> usize {
    match Opcode::try_from(byte) {
        Ok(instruction) => match instruction {
            Opcode::Constant | Opcode::DefineGlobal | Opcode::GetGlobal | Opcode::SetGlobal => {
                constant_instruction(&instruction, chunk, offset, writer)
            }
            Opcode::SetLocal | Opcode::GetLocal => {
                byte_instruction(&instruction, chunk, offset, writer)
            }
            Opcode::Jump | Opcode::JumpIfFalse => {
                jump_instruction(&instruction, chunk, 1, offset, writer)
            }
            _ => simple_instruction(&instruction, offset, writer),
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

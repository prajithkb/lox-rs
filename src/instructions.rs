use std::{convert::TryFrom, fmt::Display, io::Write};

use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::{
    chunk::Chunk,
    objects::{Function::UserDefined, Object, Value},
};

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
    JumpIfTrue,
    Jump,
    Loop,
    Call,
    Closure,
    GetUpvalue,
    SetUpvalue,
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
    print_value(&chunk.constants.read_item_at(constant as usize), writer);
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
    mut offset: usize,
    writer: &mut dyn Write,
) -> usize {
    match Opcode::try_from(byte) {
        Ok(instruction) => match instruction {
            Opcode::Constant | Opcode::DefineGlobal | Opcode::GetGlobal | Opcode::SetGlobal => {
                constant_instruction(&instruction, chunk, offset, writer)
            }
            Opcode::SetLocal
            | Opcode::GetLocal
            | Opcode::Call
            | Opcode::GetUpvalue
            | Opcode::SetUpvalue => byte_instruction(&instruction, chunk, offset, writer),
            Opcode::Jump | Opcode::JumpIfFalse | Opcode::JumpIfTrue => {
                jump_instruction(&instruction, chunk, 1, offset, writer)
            }
            Opcode::Loop => jump_instruction(&instruction, chunk, -1, offset, writer),
            Opcode::Return => simple_instruction(&instruction, offset, writer),
            Opcode::Add => simple_instruction(&instruction, offset, writer),
            Opcode::Subtract => simple_instruction(&instruction, offset, writer),
            Opcode::Multiply => simple_instruction(&instruction, offset, writer),
            Opcode::Divide => simple_instruction(&instruction, offset, writer),
            Opcode::Negate => simple_instruction(&instruction, offset, writer),
            Opcode::Nil => simple_instruction(&instruction, offset, writer),
            Opcode::True => simple_instruction(&instruction, offset, writer),
            Opcode::False => simple_instruction(&instruction, offset, writer),
            Opcode::Not => simple_instruction(&instruction, offset, writer),
            Opcode::EqualEqual => simple_instruction(&instruction, offset, writer),
            Opcode::BangEqual => simple_instruction(&instruction, offset, writer),
            Opcode::Greater => simple_instruction(&instruction, offset, writer),
            Opcode::GreaterEqual => simple_instruction(&instruction, offset, writer),
            Opcode::Less => simple_instruction(&instruction, offset, writer),
            Opcode::LessEqual => simple_instruction(&instruction, offset, writer),
            Opcode::Print => simple_instruction(&instruction, offset, writer),
            Opcode::Pop => simple_instruction(&instruction, offset, writer),
            Opcode::Closure => {
                offset += 1;
                let constant = *chunk.code.read_item_at(offset);
                offset += 1;
                write!(writer, "{:<30} {:4} '", instruction.to_string(), constant)
                    .expect("Write failed");
                print_value(&chunk.constants.read_item_at(constant as usize), writer);
                writeln!(writer, "'").expect("write failed");
                let v = &*chunk.constants.read_item_at(constant as usize);
                if let Value::Object(Object::Closure(c)) = v {
                    let closure = (**c).borrow();
                    let function = &closure.function;
                    match function {
                        UserDefined(u) => {
                            for _ in 0..u.upvalue_count {
                                let is_local = *chunk.code.read_item_at(offset);
                                offset += 1;
                                let index = *chunk.code.read_item_at(offset);
                                offset += 1;
                                writeln!(
                                    writer,
                                    "{:04}   |{:>38} {} {}",
                                    offset - 2,
                                    "",
                                    if is_local == 1 { "local" } else { "upvalue" },
                                    index
                                )
                                .expect("Write failed");
                            }
                        }
                    }
                }
                offset
            }
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

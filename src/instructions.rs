use std::io::Write;

use crate::chunk::{Chunk, Value};

pub fn simple_instruction(name: &str, offset: usize, writer: &mut dyn Write) -> usize {
    writeln!(writer, "{}", name).expect("Write failed");
    offset + 1
}

pub fn constant_instruction(
    name: &str,
    chunk: &Chunk,
    offset: usize,
    writer: &mut dyn Write,
) -> usize {
    let constant = chunk.code.read_item_at(offset + 1);
    write!(writer, "{:<16} {:4} '", name, constant).expect("Write failed");
    print_value(chunk.constants.read_item_at(constant as usize), writer);
    writeln!(writer, "'").expect("Write failed");
    offset + 2
}

pub fn print_value(value: Value, writer: &mut dyn Write) {
    write!(writer, "{:?}", value).expect("Write failed");
}

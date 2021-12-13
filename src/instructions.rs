use crate::chunk::{Chunk, Value};

pub fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

pub fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk.code.read_item_at(offset + 1);
    print!("{:<16} {:4} '", name, constant);
    print_value(chunk.constants.read_item_at(constant as usize));
    println!("'");
    offset + 2
}

pub fn print_value(value: Value) {
    print!("{:?}", value);
}

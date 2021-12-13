use crate::chunk::{Chunk, Opcode};
use crate::errors::*;

#[derive(Debug)]
pub struct VirtualMachine {
    chunk: Chunk,
}

pub fn vm_main() -> Result<()> {
    let mut chunk = Chunk::new();
    let offset = chunk.add_constant(1.2);
    chunk.write_chunk(Opcode::OpConstant.as_u8(), 123);
    chunk.write_chunk(offset as u8, 123);
    chunk.write_chunk(Opcode::OpReturn.as_u8(), 123);
    chunk.disassemble_chunk("test chunk");
    chunk.free_all();
    Ok(())
}

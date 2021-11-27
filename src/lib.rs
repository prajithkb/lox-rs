#[macro_use]
extern crate error_chain;
mod errors {
    // Create the Error, ErrorKind, ResultExt, and Result types
    error_chain! {}
}
pub use errors::*;

pub mod lox;
mod scan;
mod tokens;

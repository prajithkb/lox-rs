#[macro_use]
extern crate error_chain;
mod errors {
    // Create the Error, ErrorKind, ResultExt, and Result types
    error_chain! {
        errors {
            ScanError(message: String) {
                description("Scan Error")
                display("Scan Error: {}", message)
            }
            ParseError(message: String) {
                description("Parse Error")
                display("Parse Error: {}", message)
            }
            RuntimeError(message: String) {
                description("Runtime Error")
                display("Runtime Error: {}", message)
            }
        }
    }
}
pub use errors::*;

mod interpreter;
pub mod lox;
mod parser;
mod scanner;
mod tokens;

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
            ResolutionError(message: String) {
                description("Resolution Error")
                display("Resolution Error: {}", message)
            }
            RuntimeError(message: String) {
                description("Runtime Error")
                display("Runtime Error: {}", message)
            }
        }

        foreign_links {
            Io(::std::io::Error) #[cfg(unix)];
        }
    }
}
pub use errors::*;

mod interpreter;
pub mod lox_interpreter;
mod parser;
mod resolver;
mod scanner;
mod tokens;

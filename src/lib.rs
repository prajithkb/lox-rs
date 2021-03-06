#[macro_use]
extern crate error_chain;
pub mod errors {

    // Create the Error, ErrorKind, ResultExt, and Result types
    error_chain! {
        errors {
            // Interpreter errors
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

pub mod bytecode_virtual_machine;
pub mod common;
pub mod tree_walk_interpreter;

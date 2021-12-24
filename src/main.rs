use std::env;
use std::io::stderr;

use lox_rs::lox::{print_error, Lox};
use lox_rs::*;
fn main() -> Result<()> {
    env_logger::init();
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::new();
    let result = match args.len() {
        2 => match args[1].as_str() {
            "interpreter" => lox.run_prompt(lox::LoxRunType::Interpreter),
            "vm" => lox.run_prompt(lox::LoxRunType::VirtualMachine),
            _ => print_help(),
        },
        3 => match args[1].as_str() {
            "interpreter" => lox.run_script_with_exit_code(&args[2], lox::LoxRunType::Interpreter),
            "vm" => lox.run_script_with_exit_code(&args[2], lox::LoxRunType::VirtualMachine),
            _ => print_help(),
        },
        _ => print_help(),
    };
    match result {
        Ok(_) => {}
        Err(e) => print_error(e, &mut stderr()),
    };
    Ok(())
}

fn print_help() -> Result<()> {
    eprintln!("Usage: lox-rs [type=interpreter|vm] [script=path to a file]\nNotes: Only values for type are 'interpreter' and 'vm");
    Ok(())
}

use std::env;

use lox_rs::lox_interpreter::Lox;
use lox_rs::*;
fn main() -> Result<()> {
    env_logger::init();
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::new();
    match args.len() {
        2 => match args[1].as_str() {
            "interpreter" => lox.run_prompt()?,
            "vm" => {
                todo!()
            }
            _ => print_help(),
        },
        3 => match args[1].as_str() {
            "interpreter" => lox.run_script_with_exit_code(&args[2]),
            "vm" => {
                todo!()
            }
            _ => print_help(),
        },
        _ => {
            print_help();
        }
    }
    Ok(())
}

fn print_help() {
    eprintln!("Usage: lox-rs [type=interpreter|vm] [script=path to a file]\nNotes: Only values for type are 'interpreter' and 'vm");
}

use std::{env, process::exit};

use lox_rs::lox::Lox;
use lox_rs::*;
fn main() -> Result<()> {
    env_logger::init();
    let args: Vec<String> = env::args().collect();
    let two = &2;
    let mut lox = Lox::new();
    match args.len().cmp(two) {
        std::cmp::Ordering::Less => lox.run_prompt()?,
        std::cmp::Ordering::Equal => lox.run_script_with_exit_code(&args[1]),
        std::cmp::Ordering::Greater => {
            println!("Usage: lox-rs [script]");
            exit(64);
        }
    }
    Ok(())
}

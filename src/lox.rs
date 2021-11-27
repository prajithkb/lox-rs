use std::{
    fs::File,
    io::{self, Read, Write},
    process::exit,
};

use log::debug;

use crate::{errors::*, scan::Scanner};
pub struct Lox {
    had_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Lox { had_error: false }
    }

    pub fn run_script(&self, script: &str) -> Result<()> {
        let mut script = File::open(script).chain_err(|| "Unable to create file")?;
        let mut script_contents = String::new();
        if script
            .read_to_string(&mut script_contents)
            .chain_err(|| "Unable to read file")?
            > 0
        {
            self.run(script_contents)?;
            if self.had_error {
                exit(65)
            }
        }
        Ok(())
    }

    pub fn run(&self, content: String) -> Result<()> {
        let mut scanner = Scanner::new(content);
        match scanner.scan_tokens() {
            Ok(tokens) => {
                debug!("Tokens found: {:?}", tokens);
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    pub fn run_prompt(&mut self) -> Result<()> {
        loop {
            print!("cmd> ");
            io::stdout().flush().chain_err(|| "")?;
            let mut line = String::new();
            let bytes = io::stdin()
                .read_line(&mut line)
                .chain_err(|| "Unable to read stdin")?;
            self.run(line.trim().to_string())?;
            self.had_error = false;
            if bytes == 0 {
                break;
            }
        }
        Ok(())
    }
}

impl Default for Lox {
    fn default() -> Self {
        Self::new()
    }
}

pub fn report_error(line: usize, message: String) {
    report(line, "".into(), message);
}

pub fn report(line: usize, location: String, message: String) {
    eprintln!("[line: {}] Error {}: message {}", line, location, message);
}

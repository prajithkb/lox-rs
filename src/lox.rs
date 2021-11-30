use std::{
    fs::File,
    io::{self, Read, Write},
    process::exit,
};

use log::{debug, error};

use crate::{errors::*, interpreter::Interpreter, parser::Parser, scanner::Scanner};
pub struct Lox {
    had_error: bool,
    had_runtime_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Lox {
            had_error: false,
            had_runtime_error: false,
        }
    }

    pub fn run_script(&mut self, script: &str) -> Result<()> {
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
            if self.had_runtime_error {
                exit(75)
            }
        }
        Ok(())
    }

    pub fn run(&mut self, content: String) -> Result<()> {
        let mut scanner = Scanner::new(content);
        let mut interpreter = Interpreter::new();
        match scanner.scan_tokens() {
            Ok(tokens) => {
                debug!("Created Tokens : {:?}", tokens);
                let mut parser = Parser::new(tokens);
                match parser.parse() {
                    Ok(expr) => {
                        debug!("Created (AST) Expr: {}", expr);
                        match interpreter.interpret(&expr) {
                            Ok(result) => {
                                self.had_runtime_error = true;
                                debug!("Interpreted result: {:?}", result);
                                println!("Interpreted result: {:?}", result);
                                Ok(())
                            }
                            Err(e) => {
                                self.had_error = true;
                                error!("{}", e);
                                Ok(())
                            }
                        }
                    }
                    Err(e) => {
                        self.had_error = true;
                        error!("{}", e);
                        Ok(())
                    }
                }
            }
            Err(e) => {
                self.had_error = true;
                error!("{}", e);
                Err(e)
            }
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
    eprintln!("[line: {}] Error {}: message: {}", line, location, message);
}

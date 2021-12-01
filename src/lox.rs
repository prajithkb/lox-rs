use std::{
    fs::File,
    io::{self, Read, Write},
    process::exit,
};

use log::{debug, error};

use crate::{errors::*, interpreter::Interpreter, parser::Parser, scanner::Scanner};
pub struct Lox<'a> {
    had_error: bool,
    had_runtime_error: bool,
    interpreter: Interpreter<'a>,
}

impl<'a> Lox<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Lox {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::new(),
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

        match scanner.scan_tokens() {
            Ok(tokens) => {
                debug!("Created Tokens : {:?}", tokens);
                let mut parser = Parser::new(tokens);
                match parser.parse() {
                    Ok(statements) => {
                        debug!("Created (AST) Statements: {:?}", statements);
                        match self.interpreter.interpret(&statements) {
                            Ok(_) => Ok(()),
                            Err(e) => {
                                self.had_runtime_error = true;
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

pub fn report_error(line: usize, message: String, error_writer: &mut dyn Write) {
    report(line, "".into(), message, error_writer);
}

pub fn report(line: usize, location: String, message: String, error_writer: &mut dyn Write) {
    writeln!(
        error_writer,
        "[line: {}] Error {}: message: {}",
        line, location, message
    )
    .expect("Write failed");
}

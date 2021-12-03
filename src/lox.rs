use std::{
    fs::File,
    io::{self, stdout, Read, Write},
    process::exit,
};

use log::debug;

use crate::{errors::*, interpreter::Interpreter, parser::Parser, scanner::Scanner};
pub struct Lox<'a> {
    error: Option<LoxError>,
    interpreter: Interpreter<'a>,
}

#[derive(Debug)]
enum LoxError {
    RuntimeError,
    SyntaxError,
}

impl<'a> Lox<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Lox {
            error: None,
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
        }
        Ok(())
    }

    pub fn run_script_with_exit_code(&mut self, script: &str) {
        match self.run_script(script) {
            Ok(_) => exit(0),
            Err(_) => {
                if let Some(lox_error) = &self.error {
                    match lox_error {
                        LoxError::RuntimeError => exit(65),
                        LoxError::SyntaxError => exit(75),
                    }
                }
            }
        }
    }

    pub fn run(&mut self, content: String) -> Result<()> {
        let mut scanner = Scanner::new(content);
        match scanner.scan_tokens() {
            Ok(tokens) => {
                debug!("Created Tokens : {:?}", tokens);
                let mut parser = Parser::new(tokens);
                let statements = parser.parse()?;
                debug!("Created Statements: {:?}", statements);
                match self.interpreter.interpret(&statements) {
                    Ok(_) => {
                        debug!("Interpreted successfully!");
                        Ok(())
                    }
                    Err(runtime_error) => {
                        self.error = Some(LoxError::RuntimeError);
                        report_error(runtime_error.to_string(), &mut stdout());
                        Err(runtime_error)
                    }
                }
            }
            Err(scan_error) => {
                self.error = Some(LoxError::SyntaxError);
                Err(scan_error)
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
            match self.run(line.trim().to_string()) {
                Ok(_) => continue,
                Err(e) => {
                    debug!("Command encountered error [{}], to exit press Ctrl + C", e)
                }
            };
            self.error = None;
            if bytes == 0 {
                break;
            }
        }
        Ok(())
    }
}

pub fn report_error(message: String, error_writer: &mut dyn Write) {
    writeln!(error_writer, "{}", message).expect("Write failed");
}

pub fn report_error_with_line(line: usize, message: String, error_writer: &mut dyn Write) {
    report_error_with_line_and_location(line, "".into(), message, error_writer);
}

pub fn report_error_with_line_and_location(
    line: usize,
    location: String,
    message: String,
    error_writer: &mut dyn Write,
) {
    report_error(
        format!("[line: {}] Error {}: message: {}", line, location, message),
        error_writer,
    );
}

use std::{
    fs::File,
    io::{self, stdout, Read, Write},
    process::exit,
    time::Instant,
};

use log::{debug, info};

use crate::{
    errors::*, interpreter::Interpreter, parser::Parser, resolver::Resolver, scanner::Scanner,
    vm::VirtualMachine,
};
pub struct Lox<'a> {
    error: Option<LoxError>,
    interpreter: Interpreter<'a>,
    vm: VirtualMachine,
}
#[derive(Clone)]
pub enum LoxRunType {
    Interpreter,
    VirtualMachine,
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
            vm: VirtualMachine::new(),
        }
    }

    pub fn run_script(&mut self, script: &str, run_type: LoxRunType) -> Result<()> {
        let mut script = File::open(script).chain_err(|| "Unable to create file")?;
        let mut script_contents = String::new();
        if script
            .read_to_string(&mut script_contents)
            .chain_err(|| "Unable to read file")?
            > 0
        {
            match run_type {
                LoxRunType::Interpreter => self.run_interpreter(script_contents)?,
                LoxRunType::VirtualMachine => self.run_vm(script_contents)?,
            };
        }
        Ok(())
    }

    pub fn run_script_with_exit_code(&mut self, script: &str, run_type: LoxRunType) {
        match self.run_script(script, run_type) {
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

    pub fn run_vm(&mut self, source: String) -> Result<()> {
        self.vm.interpret(source)?;
        Ok(())
    }

    pub fn run_interpreter(&mut self, content: String) -> Result<()> {
        let mut scanner = Scanner::new(content);
        let start_time = Instant::now();
        match scanner.scan_tokens() {
            Ok(tokens) => {
                debug!("Created Tokens : {:?}", tokens);
                info!("Tokens created in {} us", start_time.elapsed().as_micros());
                let start_time = Instant::now();
                let mut parser = Parser::new(tokens);
                let mut resolver = Resolver::new();
                let statements = parser.parse()?;
                debug!("Created Statements: {:?}", statements);
                info!(
                    "Statements created in {} us",
                    start_time.elapsed().as_micros()
                );
                let start_time = Instant::now();
                match resolver.resolve(&statements) {
                    Ok(resolved_variables) => {
                        info!(
                            "Statements resolved in {} us",
                            start_time.elapsed().as_micros()
                        );
                        let start_time = Instant::now();
                        match self.interpreter.interpret(&statements, resolved_variables) {
                            Ok(_) => {
                                info!(
                                    "Statements interpreted in {} us ({} ms)",
                                    start_time.elapsed().as_micros(),
                                    start_time.elapsed().as_millis()
                                );
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
                    Err(resolver_error) => {
                        self.error = Some(LoxError::SyntaxError);
                        report_error(resolver_error.to_string(), &mut stdout());
                        Err(resolver_error)
                    }
                }
            }
            Err(scan_error) => {
                self.error = Some(LoxError::SyntaxError);
                Err(scan_error)
            }
        }
    }

    pub fn run_prompt(&mut self, run_type: LoxRunType) -> Result<()> {
        loop {
            print!("cmd> ");
            io::stdout().flush().chain_err(|| "")?;
            let mut line = String::new();
            let bytes = io::stdin()
                .read_line(&mut line)
                .chain_err(|| "Unable to read stdin")?;
            let result = match run_type {
                LoxRunType::Interpreter => self.run_interpreter(line.trim().to_string()),
                LoxRunType::VirtualMachine => self.run_vm(line.trim().to_string()),
            };
            match result {
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
pub fn utf8_to_string(bytes: &[u8]) -> String {
    match String::from_utf8(bytes.to_vec()) {
        Ok(s) => s,
        Err(_) => String::new(),
    }
}

use std::{
    cell::RefCell,
    fs::File,
    io::{self, stderr, Read, Write},
    rc::Rc,
    time::Instant,
};

use log::{debug, info};

use crate::{
    errors::*, interpreter::Interpreter, parser::Parser, resolver::Resolver, scanner::Scanner,
    vm::VirtualMachine,
};

pub type Shared<T> = Rc<RefCell<T>>;

pub type Writer<'a> = Option<&'a mut dyn Write>;

pub struct Lox<'a> {
    interpreter: Interpreter<'a>,
    vm: VirtualMachine<'a>,
}
#[derive(Clone)]
pub enum LoxRunType {
    Interpreter,
    VirtualMachine,
}

impl<'a> Lox<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Lox {
            interpreter: Interpreter::new(),
            vm: VirtualMachine::new(),
        }
    }

    pub fn run_script(&mut self, path: &str, run_type: LoxRunType) -> Result<()> {
        let mut script = File::open(path).chain_err(|| "Unable to create file")?;
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
        self.vm.free();
        Ok(())
    }

    pub fn run_script_with_exit_code(&mut self, script: &str, run_type: LoxRunType) -> Result<()> {
        self.run_script(script, run_type)
    }

    pub fn run_vm(&mut self, source: String) -> Result<()> {
        self.vm.interpret(source)?;
        Ok(())
    }

    pub fn run_interpreter(&mut self, content: String) -> Result<()> {
        let mut scanner = Scanner::new(content);
        let start_time = Instant::now();
        let tokens = scanner.scan_tokens();
        info!("Tokens created in {} us", start_time.elapsed().as_micros());
        debug!("Created Tokens : {:?}", tokens);
        let start_time = Instant::now();
        let mut parser = Parser::new(tokens?);
        let mut resolver = Resolver::new();
        let statements = parser.parse()?;
        debug!("Created Statements: {:?}", statements);
        info!(
            "Statements created in {} us",
            start_time.elapsed().as_micros()
        );
        let start_time = Instant::now();
        let resolved_variables = resolver.resolve(&statements);
        info!(
            "Statements resolved in {} us",
            start_time.elapsed().as_micros()
        );
        let result = self.interpreter.interpret(&statements, resolved_variables?);
        info!(
            "Statements interpreted in {} us ({} ms)",
            start_time.elapsed().as_micros(),
            start_time.elapsed().as_millis()
        );
        result?;
        Ok(())
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
                    eprintln!("Command encountered error, to exit press Ctrl + C");
                    print_error(e, &mut stderr());
                }
            };
            if bytes == 0 {
                break;
            }
        }
        self.vm.free();
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

pub fn print_error(e: Error, error_writer: &mut dyn Write) {
    match e.0 {
        ErrorKind::ScanError(i) => print_error_kind_message("[Scan Error]", &i, error_writer),
        ErrorKind::ParseError(i) => print_error_kind_message("[Parse Error]", &i, error_writer),
        ErrorKind::ResolutionError(i) => {
            print_error_kind_message("[Resolution Error]", &i, error_writer)
        }
        ErrorKind::RuntimeError(i) => print_error_kind_message("[Runtime Error]", &i, error_writer),
        _ => print_error_kind_message("Unknown", &e.to_string(), error_writer),
    };
}

fn print_error_kind_message(kind: &str, message: &str, error_writer: &mut dyn Write) {
    writeln!(error_writer, "{} {}", kind, message).expect("Write failed");
}

pub fn init_logger_for_test() {
    let _ = env_logger::builder().is_test(true).try_init();
}

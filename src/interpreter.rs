use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;
use std::io::Write;

use crate::parser::{Expr, Stmt};

use crate::errors::*;
use crate::tokens::{Literal, Token, TokenType};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    _Any(Box<Value>),
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::_Any(_) => todo!(),
            Value::Nil => f.write_str("nil"),
            Value::Boolean(b) => f.write_str(&b.to_string()),
            Value::Number(n) => f.write_str(&n.to_string()),
            Value::String(s) => f.write_str(s),
        }
    }
}

struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) -> Option<Value> {
        self.values.insert(name, value)
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned()
    }

    pub fn entry(&mut self, name: &str) -> Entry<String, Value> {
        self.values.entry(name.to_string())
    }
}

pub struct Interpreter<'a> {
    environment: Environment,
    output_writer: Option<&'a mut dyn Write>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new(),
            output_writer: None,
        }
    }

    #[allow(dead_code)]
    fn new_with_writer(output_writer: &'a mut dyn Write) -> Self {
        Interpreter {
            environment: Environment::new(),
            output_writer: Some(output_writer),
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<()> {
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&mut self, statement: &Stmt) -> Result<()> {
        match statement {
            Stmt::Expression(e) => self.evaluate(e).map(|_| Ok(()))?,
            Stmt::Print(e) => self.print_statement(e),
            Stmt::Var(token, expr) => self.var_statement(token, expr),
        }
    }

    fn print_statement(&mut self, expr: &Expr) -> Result<()> {
        let v = self.evaluate(expr)?;
        if let Some(writer) = self.output_writer.as_deref_mut() {
            writeln!(writer, "{}", v)?;
        }
        println!("{}", v);
        Ok(())
    }

    fn var_statement(&mut self, token: &Token, expr: &Option<Box<Expr>>) -> Result<()> {
        let name = token.lexeme.clone();
        let value = match expr {
            Some(e) => self.evaluate(e)?,
            None => Value::Nil,
        };
        self.environment.define(name, value);
        Ok(())
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Binary(left, operator, right) => self.evaluate_binary(left, operator, right),
            Expr::Grouping(expr) => self.evaluate_grouping(expr),
            Expr::Literal(literal) => self.evaluate_literal(literal.clone()),
            Expr::Unary(operator, right) => self.evaluate_unary(operator, right),
            Expr::Var(name) => self.evaluate_var(name),
            Expr::Assign(name, expr) => self.evaluate_assignment(name, expr),
        }
    }

    fn evaluate_assignment(&mut self, name: &Token, expr: &Expr) -> Result<Value> {
        let value = self.evaluate(expr)?;
        match self.environment.entry(&name.lexeme) {
            Entry::Occupied(mut entry) => {
                entry.insert(value.clone());
                Ok(value)
            }
            Entry::Vacant(_) => bail!(runtime_error_with_token(
                format!("var {} is not defined", &name.lexeme),
                name
            )),
        }
    }

    fn evaluate_var(&mut self, name: &Token) -> Result<Value> {
        match self.environment.get(&name.lexeme) {
            Some(v) => Ok(v),
            None => bail!(runtime_error_with_token(
                format!("var {} is not defined", &name.lexeme),
                name
            )),
        }
    }

    fn evaluate_literal(&mut self, literal: Option<Literal>) -> Result<Value> {
        match literal {
            Some(literal) => match literal {
                Literal::String(s) => Ok(Value::String(s)),
                Literal::Number(num) => Ok(Value::Number(num)),
                Literal::Bool(bool) => Ok(Value::Boolean(bool)),
                _ => bail!(runtime_error(format!("Invalid literal: {:?} at ", literal))),
            },
            None => Ok(Value::Nil),
        }
    }

    fn evaluate_grouping(&mut self, expr: &Expr) -> Result<Value> {
        self.evaluate(expr)
    }
    fn evaluate_binary(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Result<Value> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        match operator.token_type {
            TokenType::Minus => subtract(left, right, operator),
            TokenType::Slash => divide(left, right, operator),
            TokenType::Star => multiply(left, right, operator),
            TokenType::Plus => add(left, right, operator),
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => comparison(left, right, operator.token_type, operator),
            TokenType::EqualEqual => is_equal(left, right),
            TokenType::BangEqual => {
                if let Value::Boolean(b) = is_equal(left, right)? {
                    Ok(Value::Boolean(!b))
                } else {
                    bail!(runtime_error("Unreachable code".into()))
                }
            }
            _ => bail!(runtime_error("Unreachable code".into())),
        }
    }

    fn evaluate_unary(&mut self, operator: &Token, right: &Expr) -> Result<Value> {
        let right = self.evaluate(right)?;
        match operator.token_type {
            TokenType::Minus => negate_value(right, operator),
            TokenType::Bang => bang_value(right),
            _ => bail!(runtime_error("Unreachable code".into())),
        }
    }
}

fn subtract(left: Value, right: Value, token: &Token) -> Result<Value> {
    if let (Value::Number(l), Value::Number(r)) = (&left, &right) {
        Ok(Value::Number(l - r))
    } else {
        bail!(runtime_error_with_token(
            format!(
                "Cannot perform {:?} - {:?} as they are not both numbers",
                left, right
            ),
            token
        ))
    }
}

fn divide(left: Value, right: Value, token: &Token) -> Result<Value> {
    if let (Value::Number(l), Value::Number(r)) = (&left, &right) {
        Ok(Value::Number(l / r))
    } else {
        bail!(runtime_error_with_token(
            format!(
                "Cannot perform {:?} / {:?} as they are not both numbers",
                left, right
            ),
            token
        ))
    }
}

fn multiply(left: Value, right: Value, token: &Token) -> Result<Value> {
    if let (Value::Number(l), Value::Number(r)) = (&left, &right) {
        Ok(Value::Number(l * r))
    } else {
        bail!(runtime_error_with_token(
            format!(
                "Cannot perform {:?} * {:?} as they are not both numbers",
                left, right
            ),
            token
        ))
    }
}
fn comparison(left: Value, right: Value, comparison: TokenType, token: &Token) -> Result<Value> {
    if let (Value::Number(l), Value::Number(r)) = (&left, &right) {
        match comparison {
            TokenType::Greater => Ok(Value::Boolean(l > r)),
            TokenType::GreaterEqual => Ok(Value::Boolean(l >= r)),
            TokenType::Less => Ok(Value::Boolean(l < r)),
            TokenType::LessEqual => Ok(Value::Boolean(l <= r)),
            _ => bail!(runtime_error("Unreachable code".into())),
        }
    } else {
        bail!(runtime_error_with_token(
            format!(
                "Cannot perform {:?} {:?} {:?} as they are not both numbers",
                left, comparison, right
            ),
            token
        ))
    }
}

fn is_equal(left: Value, right: Value) -> Result<Value> {
    match (&left, &right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean((l - r).abs() < f64::EPSILON)),
        (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l == r)),
        (Value::Nil, Value::Nil) => Ok(Value::Boolean(true)),
        _ => Ok(Value::Boolean(false)),
    }
}

fn add(left: Value, right: Value, token: &Token) -> Result<Value> {
    match (&left, &right) {
        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
        (Value::String(l), Value::String(r)) => {
            let mut result = String::new();
            result.push_str(l);
            result.push_str(r);
            Ok(Value::String(result))
        }

        _ => bail!(runtime_error_with_token(
            format!(
                "Cannot perform {:?} + {:?}, they need to be numbers or strings",
                left, right
            ),
            token
        )),
    }
}
fn negate_value(value: Value, token: &Token) -> Result<Value> {
    if let Value::Number(num) = value {
        Ok(Value::Number(-num))
    } else {
        bail!(runtime_error_with_token(
            format!("Value {:?} is not a number", value),
            token
        ))
    }
}

fn bang_value(value: Value) -> Result<Value> {
    match value {
        Value::Nil => Ok(Value::Boolean(true)),
        Value::Boolean(b) => Ok(Value::Boolean(!b)),
        _ => Ok(Value::Boolean(false)),
    }
}

fn runtime_error_with_token(message: String, token: &Token) -> ErrorKind {
    ErrorKind::RuntimeError(format!(
        "[line: {}] Error at '{}': {} ",
        token.line, token.lexeme, message
    ))
}

fn runtime_error(message: String) -> ErrorKind {
    ErrorKind::RuntimeError(message)
}

#[cfg(test)]
mod tests {
    use crate::{
        errors::*,
        interpreter::Value,
        parser::{Parser, Stmt},
        scanner::Scanner,
    };
    use std::f64::EPSILON;

    use super::Interpreter;

    fn evaluate_expression_statement(
        interpreter: &mut Interpreter,
        statement: &Stmt,
    ) -> Result<Value> {
        if let Stmt::Expression(e) = statement {
            return interpreter.evaluate(e);
        }
        panic!("unreachable code")
    }

    #[test]
    fn evaluate_literals() -> Result<()> {
        let mut scanner = Scanner::new("1.25;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = 1.25;
        if let Value::Number(b) = evaluate_expression_statement(&mut interpreter, &statements[0])? {
            assert!((expected - b).abs() < EPSILON);
        } else {
            return Err("Not a Number".into());
        }

        let mut scanner = Scanner::new("true;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = true;
        if let Value::Boolean(b) = evaluate_expression_statement(&mut interpreter, &statements[0])?
        {
            assert_eq!(expected, b);
        } else {
            return Err("Not a Boolean".into());
        }

        let mut scanner = Scanner::new("\"String\";".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = "String";
        if let Value::String(b) = evaluate_expression_statement(&mut interpreter, &statements[0])? {
            assert_eq!(expected, b);
        } else {
            return Err("Not a String".into());
        }
        Ok(())
    }

    #[test]
    fn evaluate_expressions() -> Result<()> {
        let mut scanner = Scanner::new("-4;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = -4.0;
        if let Value::Number(num) = evaluate_expression_statement(&mut interpreter, &statements[0])?
        {
            assert!((num - expected).abs() < EPSILON);
        } else {
            return Err("Not a Number".into());
        }
        let mut scanner = Scanner::new("2 + 3 - 4 / (2 * 3);".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = 2.0 + 3.0 - 4.0 / (2.0 * 3.0);
        if let Value::Number(num) = evaluate_expression_statement(&mut interpreter, &statements[0])?
        {
            assert!((num - expected).abs() < EPSILON);
        } else {
            return Err("Not a Number".into());
        }

        let mut scanner = Scanner::new("5 / 5 == 1.0;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = true;
        if let Value::Boolean(b) = evaluate_expression_statement(&mut interpreter, &statements[0])?
        {
            assert_eq!(expected, b);
        } else {
            return Err("Not a Boolean".into());
        }

        let mut scanner = Scanner::new("2 != nil;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = true;
        if let Value::Boolean(b) = evaluate_expression_statement(&mut interpreter, &statements[0])?
        {
            assert_eq!(expected, b);
        } else {
            return Err("Not a Boolean".into());
        }

        let mut scanner = Scanner::new("\"string\" != nil;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = true;
        if let Value::Boolean(b) = evaluate_expression_statement(&mut interpreter, &statements[0])?
        {
            assert_eq!(expected, b);
        } else {
            return Err("Not a Boolean".into());
        }

        let mut scanner = Scanner::new("\"hello\" +\" \" + \"world\" + \"!\";".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = "hello world!".to_string();
        if let Value::String(s) = evaluate_expression_statement(&mut interpreter, &statements[0])? {
            assert_eq!(expected, s);
        } else {
            return Err("Not a Boolean".into());
        }
        Ok(())
    }

    fn utf8_to_string(bytes: &[u8]) -> String {
        match String::from_utf8(bytes.to_vec()) {
            Ok(s) => s,
            Err(_) => String::new(),
        }
    }

    #[test]
    fn execute_statements() -> Result<()> {
        let mut scanner = Scanner::new("print \"hello\" +\" \" + \"world\" + \"!\";".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(&mut buf);
        let expected = "hello world!\n".to_string();
        interpreter.interpret(&statements)?;
        assert_eq!(expected, utf8_to_string(&buf));

        let mut scanner = Scanner::new(
            r#"
        var a = 10;
        var b = 2 * a - a;
        print a == b;
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(&mut buf);
        let expected = "true\n".to_string();
        interpreter.interpret(&statements)?;
        assert_eq!(expected, utf8_to_string(&buf));
        Ok(())
    }
}

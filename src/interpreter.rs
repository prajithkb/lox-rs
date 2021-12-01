use crate::parser::Expr;

use crate::errors::*;
use crate::tokens::{Literal, Token, TokenType};

#[derive(Debug, PartialEq)]
pub enum Value {
    _Any(Box<Value>),
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}
pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn interpret(&mut self, expr: &Expr) -> Result<Value> {
        self.evaluate(expr)
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

    fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Binary(left, operator, right) => self.evaluate_binary(left, operator, right),
            Expr::Grouping(expr) => self.evaluate_grouping(expr),
            Expr::Literal(literal) => self.evaluate_literal(literal.clone()),
            Expr::Unary(operator, right) => self.evaluate_unary(operator, right),
        }
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
    ErrorKind::RuntimeError(format!("{} [line: {}]", message, token.line))
}

fn runtime_error(message: String) -> ErrorKind {
    ErrorKind::RuntimeError(message)
}

#[cfg(test)]
mod tests {
    use crate::{errors::*, interpreter::Value, parser::Parser, scanner::Scanner};
    use std::f64::EPSILON;

    use super::Interpreter;

    #[test]
    fn test_literal() -> Result<()> {
        let mut scanner = Scanner::new("1.25".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = 1.25;
        if let Value::Number(b) = interpreter.interpret(&expr)? {
            assert!((expected - b).abs() < EPSILON);
        } else {
            return Err("Not a Number".into());
        }

        let mut scanner = Scanner::new("true".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = true;
        if let Value::Boolean(b) = interpreter.interpret(&expr)? {
            assert_eq!(expected, b);
        } else {
            return Err("Not a Boolean".into());
        }

        let mut scanner = Scanner::new("\"String\"".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = "String";
        if let Value::String(b) = interpreter.interpret(&expr)? {
            assert_eq!(expected, b);
        } else {
            return Err("Not a String".into());
        }
        Ok(())
    }

    #[test]
    fn interprets_expressions() -> Result<()> {
        let mut scanner = Scanner::new("2 + 3 - 4 / (2 * 3)".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = 2.0 + 3.0 - 4.0 / (2.0 * 3.0);
        if let Value::Number(num) = interpreter.interpret(&expr)? {
            assert!((num - expected).abs() < EPSILON);
        } else {
            return Err("Not a Number".into());
        }

        let mut scanner = Scanner::new("5 / 5 == 1.0".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = true;
        if let Value::Boolean(b) = interpreter.interpret(&expr)? {
            assert_eq!(expected, b);
        } else {
            return Err("Not a Boolean".into());
        }

        let mut scanner = Scanner::new("2 != nil".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = true;
        if let Value::Boolean(b) = interpreter.interpret(&expr)? {
            assert_eq!(expected, b);
        } else {
            return Err("Not a Boolean".into());
        }

        let mut scanner = Scanner::new("\"string\" != nil".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = true;
        if let Value::Boolean(b) = interpreter.interpret(&expr)? {
            assert_eq!(expected, b);
        } else {
            return Err("Not a Boolean".into());
        }

        let mut scanner = Scanner::new("\"hello\" +\" \" + \"world\" + \"!\"".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse()?;
        let mut interpreter = Interpreter::new();
        let expected = "hello world!".to_string();
        if let Value::String(s) = interpreter.interpret(&expr)? {
            assert_eq!(expected, s);
        } else {
            return Err("Not a Boolean".into());
        }
        Ok(())
    }
}

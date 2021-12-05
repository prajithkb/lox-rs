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
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(enclosing: Option<Box<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing,
        }
    }

    pub fn define(&mut self, name: String, value: Value) -> Option<Value> {
        self.values.insert(name, value)
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Option<Value> {
        match self.values.entry(name.to_string()) {
            Entry::Occupied(mut entry) => {
                let previous_value = entry.insert(value);
                Some(previous_value)
            }
            Entry::Vacant(_) => {
                if let Some(e) = &mut self.enclosing {
                    e.assign(name, value)
                } else {
                    None
                }
            }
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.values.get(name) {
            Some(v) => Some(v.clone()),
            None => match &self.enclosing {
                Some(e) => e.get(name),
                None => None,
            },
        }
    }
}

pub struct Interpreter<'a> {
    environment: Option<Box<Environment>>,
    output_writer: Option<&'a mut dyn Write>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Interpreter {
            environment: Some(Box::new(Environment::new(None))),
            output_writer: None,
        }
    }

    #[allow(dead_code)]
    fn new_with_writer(output_writer: &'a mut dyn Write) -> Self {
        Interpreter {
            environment: Some(Box::new(Environment::new(None))),
            output_writer: Some(output_writer),
        }
    }

    fn environment(&mut self) -> &mut Environment {
        self.environment.as_mut().unwrap()
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
            Stmt::Block(statements) => self.block(statements),
            Stmt::If(condition, if_branch, else_branch) => {
                self.if_statement(condition, if_branch, else_branch)
            }
            Stmt::While(condition, body) => self.while_statement(condition, body),
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
        self.environment().define(name, value);
        Ok(())
    }

    fn block(&mut self, statements: &[Stmt]) -> Result<()> {
        self.environment = Some(Box::new(Environment::new(self.environment.take())));
        for statement in statements {
            self.execute(statement).map_err(|e| {
                self.environment = self.environment().enclosing.take();
                e
            })?;
        }
        self.environment = self.environment().enclosing.take();
        Ok(())
    }

    fn if_statement(
        &mut self,
        condition: &Expr,
        if_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<()> {
        let condition_str = condition.to_string();
        if let Value::Boolean(is_true) = self.evaluate(condition)? {
            if is_true {
                return self.execute(if_branch);
            } else if let Some(else_branch) = else_branch {
                return self.execute(else_branch);
            }
        }
        bail!(runtime_error(format!(
            "Condition {} does not evaluate to a boolean",
            condition_str
        )))
    }

    fn while_statement(&mut self, condition: &Expr, body: &Stmt) -> Result<()> {
        let condition_str = condition.to_string();
        loop {
            if let Value::Boolean(result) = self.evaluate(condition)? {
                if result {
                    self.execute(body)?;
                } else {
                    break;
                }
            } else {
                bail!(runtime_error(format!(
                    "Condition {} does not evaluate to a boolean",
                    condition_str
                )))
            }
        }
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
            Expr::Logical(left, operator, right) => self.evaluate_logical(left, operator, right),
        }
    }

    fn evaluate_assignment(&mut self, name: &Token, expr: &Expr) -> Result<Value> {
        let value = self.evaluate(expr)?;
        match self.environment().assign(&name.lexeme, value.clone()) {
            Some(_) => Ok(value),
            None => bail!(runtime_error_with_token(
                format!("var {} is not defined", &name.lexeme),
                name
            )),
        }
    }

    fn evaluate_var(&mut self, name: &Token) -> Result<Value> {
        match self.environment().get(&name.lexeme) {
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

    fn evaluate_logical(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Result<Value> {
        if operator.token_type == TokenType::OR {
            if let Value::Boolean(result) = self.evaluate(left)? {
                if result {
                    return Ok(Value::Boolean(result));
                } else if let Value::Boolean(result) = self.evaluate(right)? {
                    return Ok(Value::Boolean(result));
                }
            }
            bail!(runtime_error_with_token(
                format!("Expression {} does not evaluate to a boolean", left),
                operator
            ))
        } else if operator.token_type == TokenType::And {
            if let (Value::Boolean(l), Value::Boolean(r)) =
                (self.evaluate(left)?, self.evaluate(right)?)
            {
                return Ok(Value::Boolean(l && r));
            }
            bail!(runtime_error_with_token(
                format!("Expression {} does not evaluate to a boolean", left),
                operator
            ))
        } else {
            bail!(runtime_error_with_token(
                format!("Invalid logical operator {}", operator.lexeme),
                operator
            ))
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
        // If one of them is a string, we convert the one to string
        (Value::String(l), any) => {
            let mut result = String::new();
            result.push_str(l);
            result.push_str(&any.to_string());
            Ok(Value::String(result))
        }
        (any, Value::String(r)) => {
            let mut result = String::new();
            result.push_str(&any.to_string());
            result.push_str(r);
            Ok(Value::String(result))
        }
        _ => bail!(runtime_error_with_token(
            format!(
                "Cannot perform {:?} + {:?}, they need to be both numbers or strings",
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
        "[line: {}] Error at <{}>: {}",
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

    use super::{Environment, Interpreter};

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

    #[test]
    fn scoped_statements() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Scopes
            var outer = "outer variable";
            print outer; 
            {
                outer = "outer variable set to inner variable";
                print outer;
                var inner = "created a new inner variable";
                print inner;
            }
            print outer;
            print inner;
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(&mut buf);
        let expected = r#"outer variable
outer variable set to inner variable
created a new inner variable
outer variable set to inner variable
"#
        .to_string();
        match interpreter.interpret(&statements) {
            Ok(_) => panic!("Should fail!"),
            Err(e) => assert_eq!(
                "Runtime Error: [line: 12] Error at <inner>: var inner is not defined",
                e.to_string()
            ),
        }
        assert_eq!(expected, utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn if_statements() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Scopes
            var a = 2;
            if (a == 2 ) {
                print "if";
                var b = 2;
            } else {
                print "else";
            }

            if (a ==3) {
                print "second if";
            } else {
                print "second else";
            }
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(&mut buf);
        let expected = "if\nsecond else\n".to_string();
        interpreter.interpret(&statements)?;
        assert_eq!(expected, utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn logical_expressions() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Scopes
            var a = 2;
            var b = 3;
            if (a == 2 or b== 3) {
                print "one";
            }  else {
                print "fail";
            }
            if (a == 3 or b ==3){
                print "two";
            } else {
                print "fail";
            }

            if (a == 3 and b ==3){
                print "fail";
            } else {
                print "three";
            }

            if (a == 2 and b ==3){
                print "four";
            } else {
                print "fail";
            }
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(&mut buf);
        let expected = "one\ntwo\nthree\nfour\n".to_string();
        interpreter.interpret(&statements)?;
        assert_eq!(expected, utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn while_statement() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Scopes
            var a = 1;
            while (a <= 4) {
                print a;
                a = a +1;
            }
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(&mut buf);
        let expected = "1\n2\n3\n4\n".to_string();
        interpreter.interpret(&statements)?;
        assert_eq!(expected, utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn for_statement() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Scopes
            var a = 0;
            var temp;
            for (var b = 1; a < 100; b = temp + b) {
              print a;
              temp = a;
              a = b;
            }
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(&mut buf);
        let expected = "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n".to_string();
        interpreter.interpret(&statements)?;
        assert_eq!(expected, utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn environment_tests() {
        let mut env = Environment::new(None);
        assert_eq!(None, env.get("outer"));
        assert_eq!(
            None,
            env.define("outer".to_string(), Value::String("outer".into()))
        );
        assert_eq!(Some(Value::String("outer".into())), env.get("outer"));
        assert_eq!(
            Some(Value::String("outer".into())),
            env.assign("outer", Value::String("outer changed".into()))
        );
        assert_eq!(
            Some(Value::String("outer changed".into())),
            env.get("outer")
        );
        let mut env = Environment::new(Some(Box::new(env)));
        assert_eq!(
            Some(Value::String("outer changed".into())),
            env.get("outer")
        );
        assert_eq!(
            Some(Value::String("outer changed".into())),
            env.assign("outer", Value::String("outer changed by inner".into()))
        );
        assert_eq!(
            Some(Value::String("outer changed by inner".into())),
            env.get("outer")
        );
        assert_eq!(
            None,
            env.define("inner".to_string(), Value::String("inner".into()))
        );
        assert_eq!(Some(Value::String("inner".into())), env.get("inner"));
        assert_eq!(None, env.enclosing.unwrap().get("inner"));
    }
}
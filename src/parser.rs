use log::error;

use crate::{errors::*, lox::report, tokens::TokenType};
use std::fmt::Display;

use crate::tokens::{Literal, Token};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Option<Literal>),
    Unary(Token, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&to_string(self))
    }
}
fn literal_to_str(literal: &Option<Literal>) -> String {
    match literal {
        Some(v) => v.to_string(),
        None => "nil".to_string(),
    }
}

fn to_string(expr: &Expr) -> String {
    match expr {
        Expr::Binary(left, operator, right) => parenthesize(&operator.lexeme, &[left, right]),
        Expr::Grouping(expr) => parenthesize("Grouping", &[expr]),
        Expr::Literal(literal) => parenthesize(&literal_to_str(literal), &[]),
        Expr::Unary(operator, expr) => parenthesize(&operator.lexeme, &[expr]),
    }
}

fn parenthesize(name: &str, exprs: &[&Expr]) -> String {
    let mut result = String::new();
    result.push('(');
    result.push_str(name);
    for &expr in exprs {
        result.push(' ');
        result.push_str(&to_string(expr))
    }
    result.push(')');
    result
}

fn report_error_for_token(token: &Token, message: String) {
    if token.token_type == TokenType::Eof {
        report(token.line, "at the end".into(), message)
    } else {
        report(token.line, format!("at '{}'", token.lexeme), message)
    }
}

///  Grammer from https://craftinginterpreters.com/parsing-expressions.html
/// ```txt
/// expression     → equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary
///                | primary ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil"
///                | "(" expression ")" ;
///```
///
#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Expr> {
        match self.expression() {
            Ok(r) => Ok(r),
            Err(e) => {
                error!("Parse error {}", e);
                bail!(ErrorKind::ParseError("Parse failed".into()));
            }
        }
    }

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;
        while self.match_and_advance(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;
        while self.match_and_advance(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;
        while self.match_and_advance(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;
        while self.match_and_advance(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if self.match_and_advance(&[TokenType::Bang, TokenType::Minus]) {
            let token = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(token, Box::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr> {
        let t = self.peek_token();
        match t.token_type {
            TokenType::True => self.advance_and_return(Ok(Expr::Literal(Literal::opt_bool(true)))),
            TokenType::False => {
                self.advance_and_return(Ok(Expr::Literal(Literal::opt_bool(false))))
            }
            TokenType::Nil => self.advance_and_return(Ok(Expr::Literal(Literal::opt_none()))),
            TokenType::String | TokenType::Number => {
                self.advance();
                let previous = self.previous();
                Ok(Expr::Literal(previous.literal))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                let t = self.peek_token();
                if t.token_type != TokenType::RightParen {
                    report_error_for_token(&t, "Expected ')' after  expression".to_string());
                    Err("Expected ')' after expression".into())
                } else {
                    self.advance();
                    Ok(Expr::Grouping(Box::new(expr)))
                }
            }
            _ => {
                report_error_for_token(&t, "Expect expression".to_string());
                Err(format!("Expected expression, unexpected Token -> {:?}", &t).into())
            }
        }
    }

    #[inline]
    fn advance_and_return(&mut self, t: Result<Expr>) -> Result<Expr> {
        self.advance();
        t
    }

    fn _synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }
            match self.peek_token().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => self.advance(),
            }
        }
    }

    #[inline]
    fn peek_token(&self) -> Token {
        self.tokens[self.current].clone()
    }

    #[inline]
    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn match_and_advance(&mut self, token_types_to_match: &[TokenType]) -> bool {
        if self.is_at_end() {
            return false;
        }
        for &token_type in token_types_to_match {
            if token_type == self.peek_token().token_type {
                self.advance();
                return true;
            }
        }
        false
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn is_at_end(&self) -> bool {
        self.peek_token().token_type == TokenType::Eof
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        errors::*,
        scanner::Scanner,
        tokens::{Literal, Token, TokenType},
    };

    use super::{Expr, Parser};

    #[test]
    fn prints_ast_correctly() {
        let expr = Expr::Binary(
            Box::new(Expr::Unary(
                Token::new(
                    TokenType::Minus,
                    "-".to_string(),
                    0,
                    Literal::opt_number(1.0),
                ),
                Box::new(Expr::Literal(Literal::opt_number(123.0))),
            )),
            Token::new(
                TokenType::Star,
                "*".to_string(),
                0,
                Literal::opt_number(1.0),
            ),
            Box::new(Expr::Grouping(Box::new(Expr::Literal(
                Literal::opt_number(45.67),
            )))),
        );
        assert_eq!(
            "(* (- (Number(123.0))) (Grouping (Number(45.67))))",
            expr.to_string()
        );
    }

    #[test]
    fn parses_ast_correctly() -> Result<()> {
        let mut scanner = Scanner::new("2 + 3 - 4 / (2 * 3)".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();
        assert_eq!(
            "(- (+ (Number(2.0)) (Number(3.0))) (/ (Number(4.0)) (Grouping (* (Number(2.0)) (Number(3.0))))))",
            expr.to_string()
        );

        let mut scanner = Scanner::new("3 - 4 / 3 + 4 * 6 -(3 -4)".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();
        assert_eq!("(- (+ (- (Number(3.0)) (/ (Number(4.0)) (Number(3.0)))) (* (Number(4.0)) (Number(6.0)))) (Grouping (- (Number(3.0)) (Number(4.0)))))",
            expr.to_string()
        );

        let mut scanner = Scanner::new("5 / 5 == 1".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();
        assert_eq!(
            "(== (/ (Number(5.0)) (Number(5.0))) (Number(1.0)))",
            expr.to_string()
        );
        Ok(())
    }
}

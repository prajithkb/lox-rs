use log::debug;

use crate::{errors::*, lox_interpreter::report_error, tokens::TokenType};
use std::{fmt::Display, io::stdout, rc::Rc};

use crate::tokens::{Literal, Token};

/// Expressions
#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Option<Literal>),
    Unary(Token, Box<Expr>),
    Var(Token),
    Assign(Token, Box<Expr>),
    Logical(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
    Get(Box<Expr>, Token),
    Set(Box<Expr>, Token, Box<Expr>),
    This(Token),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&to_string_expr(self))
    }
}

/// Statments
#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(Token, Option<Expr>),
    Block(Vec<Stmt>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Function(Token, Vec<Token>, Rc<Vec<Stmt>>),
    Return(Token, Option<Expr>),
    Class(Token, Rc<Vec<Stmt>>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&to_string_stmt(self))
    }
}

fn literal_to_str(literal: &Option<Literal>) -> String {
    match literal {
        Some(v) => v.to_string(),
        None => "nil".to_string(),
    }
}

fn to_string_expr(expr: &Expr) -> String {
    match expr {
        Expr::Binary(left, operator, right) => parenthesize(&operator.lexeme, &[left, right]),
        Expr::Grouping(expr) => parenthesize("Grouping", &[expr]),
        Expr::Literal(literal) => parenthesize(&literal_to_str(literal), &[]),
        Expr::Unary(operator, expr) => parenthesize(&operator.lexeme, &[expr]),
        Expr::Var(token) => parenthesize(&format!("Var {}", &token.lexeme), &[]),
        Expr::Assign(token, expr) => parenthesize(&format!("{} = ", &token.lexeme), &[expr]),
        Expr::Logical(left, operator, right) => parenthesize(&operator.lexeme, &[left, right]),
        Expr::Call(callee, _, arguments) => {
            parenthesize(&format!("Fun call [{}({:?})]", callee, arguments), &[])
        }
        Expr::Get(object, property) => parenthesize(&format!("Get[{}.{}]", object, property), &[]),
        Expr::Set(object, property, value) => {
            parenthesize(&format!("GSetet[{}.{}={}]", object, property, value), &[])
        }
        Expr::This(keyword) => parenthesize(&keyword.lexeme, &[]),
    }
}

fn to_string_stmt(stmt: &Stmt) -> String {
    match stmt {
        Stmt::Expression(expr) => expr.to_string(),
        Stmt::Print(expr) => format!("Print {}", expr.to_string()),
        Stmt::Var(token, expr) => format!("Var {} = {:?}", token.lexeme, expr),
        Stmt::Block(statements) => format!(
            "Block [{}]",
            statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", "),
        ),
        Stmt::If(condition, if_branch, else_branch) => format!(
            "If <{}> [{}] else [{:?}]",
            condition, if_branch, else_branch
        ),
        Stmt::While(condition, body) => format!("While <{}> [{}] ", condition, body),
        Stmt::Function(name, arguments, body) => {
            format!(
                "Fun declaration[{}({:?}) [{:?}]]",
                name.lexeme, arguments, body
            )
        }
        Stmt::Return(_, expr) => format!("Return {:?}", expr),
        Stmt::Class(name, methods) => format!("Class declaration {:?}[{:?}]", name, methods),
    }
}

fn parenthesize(name: &str, exprs: &[&Expr]) -> String {
    let mut result = String::new();
    result.push('(');
    result.push_str(name);
    for &expr in exprs {
        result.push(' ');
        result.push_str(&to_string_expr(expr))
    }
    result.push(')');
    result
}

fn parse_error(token: &Token, message: &str) -> ErrorKind {
    ErrorKind::ParseError(format!(
        "[line: {}] Error at <{}>: message: {}",
        token.line, token.lexeme, message
    ))
}

///  Grammer from https://craftinginterpreters.com
/// ```txt
/// program        → declaration* EOF ;
/// declaration    →  classDecl | funDecl| varDecl | statement ;
/// classDecl      → "class" IDENTIFIER "{" function* "}" ;
/// varDecl        → "var" IDENTIFIER ( "=" expression )? ";"
/// funDecl        → "fun" function ;
/// function       → IDENTIFIER "(" parameters? ")" block ;
/// parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
/// statement      → exprStmt | printStmt | block  | ifStmt | WhileStmt | returnStmt;
/// returnStmt     → "return" expression? ";" ;
/// whileStmt      → "while" "(" expression ")" statement ;      
/// ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
/// block          → "{" declaration* "}" ;
/// arguments      → expression ( "," expression )* ;
/// expression     → assignment ;
/// assignment     → ( call "." )? IDENTIFIER "=" assignment | logic_or ;
/// logic_or       → logic_and ( "or" logic_and )* ;
/// logic_and      → equality ( "and" equality )* ;
/// printStmt      → "print" expression ";" ;
/// expression     → equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary | primary ;
/// call           → primary ( "(" arguments? ")" )* | "." IDENTIFIER )* ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil"
///                | "(" expression ")"  | IDENTIFIER;
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

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut parse_error = false;
        let mut statements = vec![];
        while !self.is_at_end() {
            match self.declaration() {
                Ok(s) => statements.push(s),
                Err(e) => {
                    parse_error = true;
                    debug!("Error: {}", e);
                    report_error(e.to_string(), &mut stdout());
                    self.synchronize();
                }
            }
        }
        if !parse_error {
            Ok(statements)
        } else {
            bail!(ErrorKind::ParseError("Parsing failed".into()))
        }
    }

    fn block(&mut self) -> Result<Stmt> {
        let mut statements = vec![];
        while !self.is_at_end() && self.peek_token().token_type != TokenType::RightBrace {
            let statement = self.declaration()?;
            statements.push(statement);
        }
        self.consume_next_token(TokenType::RightBrace, "Expect '}' at the end of a block")
            .map(|_| Ok(Stmt::Block(statements)))?
    }

    fn declaration(&mut self) -> Result<Stmt> {
        if self.match_and_advance(&[TokenType::Var]) {
            self.var_declaration_statement()
        } else if self.match_and_advance(&[TokenType::Fun]) {
            self.fun_declaration_statement()
        } else if self.match_and_advance(&[TokenType::Class]) {
            self.class_declaration_statement()
        } else {
            self.statement()
        }
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.match_and_advance(&[TokenType::Print]) {
            self.print_statement()
        } else if self.match_and_advance(&[TokenType::LeftBrace]) {
            self.block()
        } else if self.match_and_advance(&[TokenType::If]) {
            self.if_statement()
        } else if self.match_and_advance(&[TokenType::While]) {
            self.while_statement()
        } else if self.match_and_advance(&[TokenType::For]) {
            self.for_statement()
        } else if self.match_and_advance(&[TokenType::Return]) {
            self.return_statement()
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let keyword = self.previous();
        let mut return_expr = None;
        if self.peek_token().token_type != TokenType::Semicolon {
            return_expr = Some(self.expression()?);
        }
        self.consume_next_token(TokenType::Semicolon, "Expect ';' after return")?;
        Ok(Stmt::Return(keyword, return_expr))
    }

    /// for(Option<Initializer>; Option<Condition>; Option<Increment>) { Statement }
    /// Desugaring example
    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume_next_token(TokenType::LeftParen, "Expect '(' after for")?;
        let mut initializer = None;
        if self.peek_token().token_type != TokenType::Semicolon {
            if self.match_and_advance(&[TokenType::Var]) {
                initializer = Some(self.var_declaration_statement()?);
            } else {
                initializer = Some(self.expression_statement()?);
            }
        }
        let mut condition = None;
        if self.peek_token().token_type != TokenType::Semicolon {
            condition = Some(self.expression()?);
        }
        self.consume_next_token(TokenType::Semicolon, "Expect ';' after for condition")?;
        let mut increment = None;
        if self.peek_token().token_type != TokenType::RightParen {
            increment = Some(self.expression()?);
        }
        self.consume_next_token(TokenType::RightParen, "Expect ')' after for increment")?;
        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment)])
        }
        match condition {
            Some(condition) => body = Stmt::While(condition, Box::new(body)),
            None => body = Stmt::While(Expr::Literal(Literal::opt_bool(true)), Box::new(body)),
        }
        match initializer {
            Some(initializer) => Ok(Stmt::Block(vec![initializer, body])),
            None => Ok(body),
        }
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume_next_token(TokenType::LeftParen, "Expect '(' after while")?;
        let condition = self.expression()?;
        self.consume_next_token(TokenType::RightParen, "Expect ')' after while condition")?;
        let body = self.statement()?;
        Ok(Stmt::While(condition, Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume_next_token(TokenType::LeftParen, "Expect '(' after if")?;
        let condition = self.expression()?;
        self.consume_next_token(TokenType::RightParen, "Expect ')' after if condition")?;
        let if_branch = self.statement()?;
        let mut else_branch = None;
        if self.match_and_advance(&[TokenType::Else]) {
            else_branch = Some(Box::new(self.statement()?));
        }
        Ok(Stmt::If(
            Box::new(condition),
            Box::new(if_branch),
            else_branch,
        ))
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume_next_token(TokenType::Semicolon, "Expect ';' after print")
            .map(|_| Ok(Stmt::Print(expr)))?
    }

    fn var_declaration_statement(&mut self) -> Result<Stmt> {
        match self.consume_next_token(TokenType::Identifier, "Expect variable name") {
            Ok(token) => {
                let initializer = if self.match_and_advance(&[TokenType::Equal]) {
                    Some(self.expression()?)
                } else {
                    None
                };
                self.consume_next_token(
                    TokenType::Semicolon,
                    "Expect ';' after variable declaration",
                )
                .map(|_| Ok(Stmt::Var(token, initializer)))?
            }
            Err(e) => Err(e),
        }
    }
    fn class_declaration_statement(&mut self) -> Result<Stmt> {
        let mut methods = vec![];
        match self.consume_next_token(TokenType::Identifier, "Expect class name") {
            Ok(token) => {
                self.consume_next_token(TokenType::LeftBrace, "Expect '{' before class body")?;
                while self.peek_token().token_type != TokenType::RightBrace {
                    let function = self.fun_declaration_statement()?;
                    methods.push(function);
                }
                self.consume_next_token(TokenType::RightBrace, "Expect '}' after class body")?;
                Ok(Stmt::Class(token, Rc::new(methods)))
            }
            Err(e) => Err(e),
        }
    }

    fn fun_declaration_statement(&mut self) -> Result<Stmt> {
        let mut parameters = vec![];
        match self.consume_next_token(TokenType::Identifier, "Expect function name") {
            Ok(token) => {
                self.consume_next_token(TokenType::LeftParen, "Expect '(' after function name")?;
                while self.peek_token().token_type != TokenType::RightParen {
                    let parameter =
                        self.consume_next_token(TokenType::Identifier, "Expect parameter name")?;
                    parameters.push(parameter);
                    if self.peek_token().token_type == TokenType::Comma {
                        self.advance();
                    } else {
                        break;
                    }
                }
                self.consume_next_token(
                    TokenType::RightParen,
                    "Expect ')' for function declaration",
                )?;
                self.consume_next_token(
                    TokenType::LeftBrace,
                    "Expect '{' for function declaration",
                )?;
                if let Stmt::Block(statements) = self.block()? {
                    Ok(Stmt::Function(token, parameters, Rc::new(statements)))
                } else {
                    bail!(parse_error(
                        &self.peek_token(),
                        "Expect a block (starting with '{') for a function declaration"
                    ))
                }
            }
            Err(e) => Err(e),
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume_next_token(TokenType::Semicolon, "Expect ';' after expression")
            .map(|_| Ok(Stmt::Expression(expr)))?
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.logic_or()?;
        if self.match_and_advance(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;
            if let Expr::Var(name) = expr {
                return Ok(Expr::Assign(name, Box::new(value)));
            } else if let Expr::Get(e, name) = expr {
                return Ok(Expr::Set(e, name, Box::new(value)));
            }
            bail!(parse_error(&equals, "Invalid assignment target"))
        }
        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr> {
        let left = self.logic_and()?;
        if self.match_and_advance(&[TokenType::OR]) {
            let operator = self.previous();
            let right = self.logic_and()?;
            Ok(Expr::Logical(Box::new(left), operator, Box::new(right)))
        } else {
            Ok(left)
        }
    }

    fn logic_and(&mut self) -> Result<Expr> {
        let left = self.equality()?;
        if self.match_and_advance(&[TokenType::And]) {
            let operator = self.previous();
            let right = self.equality()?;
            Ok(Expr::Logical(Box::new(left), operator, Box::new(right)))
        } else {
            Ok(left)
        }
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
        self.call()
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        loop {
            if self.match_and_advance(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_and_advance(&[TokenType::Dot]) {
                let name =
                    self.consume_next_token(TokenType::Identifier, "Expect property after '.'")?;
                expr = Expr::Get(Box::new(expr), name);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expr) -> Result<Expr> {
        let mut arguments = vec![];
        if self.peek_token().token_type != TokenType::RightParen {
            loop {
                let argument = self.expression()?;
                arguments.push(argument);
                if self.peek_token().token_type == TokenType::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        let end_token = self.peek_token();
        self.consume_next_token(TokenType::RightParen, "Expect ')' after function call")?;
        Ok(Expr::Call(Box::new(expr), end_token, arguments))
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
            TokenType::Identifier => {
                self.advance();
                let previous = self.previous();
                Ok(Expr::Var(previous))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume_next_token(TokenType::RightParen, "Expected ')' after  expression")
                    .map(|_| Ok(Expr::Grouping(Box::new(expr))))?
            }
            TokenType::This => {
                self.advance();
                Ok(Expr::This(self.previous()))
            }
            _ => {
                bail!(parse_error(&t, "Expect expression"))
            }
        }
    }

    fn consume_next_token(
        &mut self,
        token_type_to_match: TokenType,
        message: &str,
    ) -> Result<Token> {
        let t = self.peek_token();
        let previous = self.previous();
        if t.token_type == token_type_to_match {
            self.advance();
            Ok(t)
        } else {
            bail!(parse_error(&previous, message))
        }
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

    fn synchronize(&mut self) {
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
    fn advance_and_return(&mut self, t: Result<Expr>) -> Result<Expr> {
        self.advance();
        t
    }

    #[inline]
    fn peek_token(&self) -> Token {
        self.tokens[self.current].clone()
    }

    #[inline]
    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    #[inline]
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }

    #[inline]
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
    fn parses_ast_correctly_expressions() -> Result<()> {
        let mut scanner = Scanner::new("-4;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!("(- (Number(4.0)))", statements[0].to_string());
        let mut scanner = Scanner::new("2 + 3 - 4 / (2 * 3);".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let expr = parser.parse()?;
        assert_eq!(
            "(- (+ (Number(2.0)) (Number(3.0))) (/ (Number(4.0)) (Grouping (* (Number(2.0)) (Number(3.0))))))",
            expr[0].to_string()
        );

        let mut scanner = Scanner::new("3 - 4 / 3 + 4 * 6 -(3 -4);".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!("(- (+ (- (Number(3.0)) (/ (Number(4.0)) (Number(3.0)))) (* (Number(4.0)) (Number(6.0)))) (Grouping (- (Number(3.0)) (Number(4.0)))))",
        statements[0].to_string()
        );

        let mut scanner = Scanner::new("5 / 5 == 1;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!(
            "(== (/ (Number(5.0)) (Number(5.0))) (Number(1.0)))",
            statements[0].to_string()
        );

        let mut scanner = Scanner::new("\"hello\" + \"world\";".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!(
            "(+ (String(\"hello\")) (String(\"world\")))",
            statements[0].to_string()
        );

        let mut scanner = Scanner::new("a = 2;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!("(a =  (Number(2.0)))", statements[0].to_string());

        Ok(())
    }

    #[test]
    fn parses_ast_correctly_block() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Scopes
            var outer = "outer variable";
            print outer; 
            {
                outer = "now inner";
                print outer;
                var inner = "new inner";
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
        assert_eq!(
            "Var outer = Some(Literal(Some(String(\"outer variable\")))), Print (Var outer), Block [(outer =  (String(\"now inner\"))), Print (Var outer), Var inner = Some(Literal(Some(String(\"new inner\")))), Print (Var inner)], Print (Var outer), Print (Var inner)",
            statements
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
        Ok(())
    }
    #[test]
    fn parses_ast_correctly_if() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Scopes
            var a = 2;
            var b = 3;
            if (a == 2 or b== 3) {
                print "one";
            }  else {
                print "";
            }
            if (a == 3 or b ==3){
                print "two";
            } else {
                print "";
            }

            if (a == 3 and b ==3){
                print "";
            } else {
                print "three";
            }

            if (a == 2 and b ==3){
                print "four";
            } else {
                print "";
            }
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!(
            "Var a = Some(Literal(Some(Number(2.0)))), Var b = Some(Literal(Some(Number(3.0)))), If <(or (== (Var a) (Number(2.0))) (== (Var b) (Number(3.0))))> [Block [Print (String(\"one\"))]] else [Some(Block([Print(Literal(Some(String(\"\"))))]))], If <(or (== (Var a) (Number(3.0))) (== (Var b) (Number(3.0))))> [Block [Print (String(\"two\"))]] else [Some(Block([Print(Literal(Some(String(\"\"))))]))], If <(and (== (Var a) (Number(3.0))) (== (Var b) (Number(3.0))))> [Block [Print (String(\"\"))]] else [Some(Block([Print(Literal(Some(String(\"three\"))))]))], If <(and (== (Var a) (Number(2.0))) (== (Var b) (Number(3.0))))> [Block [Print (String(\"four\"))]] else [Some(Block([Print(Literal(Some(String(\"\"))))]))]",
            statements
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );

        let mut scanner = Scanner::new("a = 2;".into());
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!("(a =  (Number(2.0)))", statements[0].to_string());

        Ok(())
    }
    #[test]
    fn parses_ast_correctly_loop() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Scopes
            var a = 1;
            while (a < 10) {
                print a;
                a = a +1;
            }
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!(
            "Var a = Some(Literal(Some(Number(1.0)))), While <(< (Var a) (Number(10.0)))> [Block [Print (Var a), (a =  (+ (Var a) (Number(1.0))))]] ",
            statements
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );

        let mut scanner = Scanner::new(
            r#"
            // Scopes
            var a = 0;
            var temp;
            for (var b = 1; a < 10000; b = temp + b) {
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
        assert_eq!(
            "Var a = Some(Literal(Some(Number(0.0)))), Var temp = None, Block [Var b = Some(Literal(Some(Number(1.0)))), While <(< (Var a) (Number(10000.0)))> [Block [Block [Print (Var a), (temp =  (Var a)), (a =  (Var b))], (b =  (+ (Var temp) (Var b)))]] ]",
            statements
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );

        Ok(())
    }

    #[test]
    fn parses_ast_correctly_call() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // function
            test_function(a, b);
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!(
            "(Fun call [(Var test_function)([Var(Token { token_type: Identifier, lexeme: \"a\", line: 3, literal: Some(Identifier(\"a\")) }), Var(Token { token_type: Identifier, lexeme: \"b\", line: 3, literal: Some(Identifier(\"b\")) })])])",
            statements
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
        let mut scanner = Scanner::new(
            r#"
            // function
            test_function();
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!(
            "(Fun call [(Var test_function)([])])",
            statements
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
        Ok(())
    }

    #[test]
    fn parses_ast_correctly_fun_declaration() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // function
            
            fun test_function(a, b) {
                print a + b;
            }

            test_function("hello", "world");
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!(
            "Fun declaration[test_function([Token { token_type: Identifier, lexeme: \"a\", line: 4, literal: Some(Identifier(\"a\")) }, Token { token_type: Identifier, lexeme: \"b\", line: 4, literal: Some(Identifier(\"b\")) }]) [[Print(Binary(Var(Token { token_type: Identifier, lexeme: \"a\", line: 5, literal: Some(Identifier(\"a\")) }), Token { token_type: Plus, lexeme: \"+\", line: 5, literal: None }, Var(Token { token_type: Identifier, lexeme: \"b\", line: 5, literal: Some(Identifier(\"b\")) })))]]], (Fun call [(Var test_function)([Literal(Some(String(\"hello\"))), Literal(Some(String(\"world\")))])])",
            statements
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
        Ok(())
    }

    #[test]
    fn parses_ast_correctly_class_declaration() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            class Bacon {
                eat() {
                  print "Crunch crunch crunch!";
                }
              }
              
            Bacon().eat();
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        assert_eq!(
            "Class declaration Token { token_type: Identifier, lexeme: \"Bacon\", line: 2, literal: Some(Identifier(\"Bacon\")) }[[Function(Token { token_type: Identifier, lexeme: \"eat\", line: 3, literal: Some(Identifier(\"eat\")) }, [], [Print(Literal(Some(String(\"Crunch crunch crunch!\"))))])]], (Fun call [(Get[(Fun call [(Var Bacon)([])]).type Identifier lexeme eat literal Some(Identifier(\"eat\"))])([])])",
            statements
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
        Ok(())
    }
}

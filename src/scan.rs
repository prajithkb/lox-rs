use std::collections::HashMap;

use log::debug;

use crate::{
    errors::*,
    lox::report_error,
    tokens::{Literal, Token, TokenType},
};

pub struct Scanner {
    source: String,
    source_len: usize,
    tokens: Vec<Token>,
    line: usize,
    start: usize,
    current: usize,
    key_words: HashMap<&'static str, TokenType>,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        debug!("Scanning source\n>>\n{}\n<<", source);
        let source_len = source.chars().count();
        Scanner {
            source,
            source_len,
            tokens: vec![],
            line: 1,
            start: 0,
            current: 0,
            key_words: HashMap::from([
                ("and", TokenType::And),
                ("class", TokenType::Class),
                ("else", TokenType::Else),
                ("false", TokenType::False),
                ("for", TokenType::For),
                ("fun", TokenType::Fun),
                ("if", TokenType::If),
                ("nil", TokenType::Nil),
                ("or", TokenType::OR),
                ("print", TokenType::Print),
                ("return", TokenType::Return),
                ("super", TokenType::Super),
                ("this", TokenType::This),
                ("true", TokenType::True),
                ("var", TokenType::Var),
                ("while", TokenType::While),
            ]),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&[Token]> {
        let mut error_found = false;
        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(_) => continue,
                Err(_) => error_found = true,
            }
        }
        self.tokens
            .push(Token::new(TokenType::Eof, "".into(), self.line, None));
        if error_found {
            Err("Scan error".into())
        } else {
            Ok(self.tokens.as_slice())
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source_len
    }

    fn scan_token(&mut self) -> Result<()> {
        let current_char = self.get_char_and_advance();
        match current_char {
            // Single character tokens
            '(' => self.add_token(TokenType::LeftParen, None),
            ')' => self.add_token(TokenType::RightParen, None),
            '{' => self.add_token(TokenType::LeftBrace, None),
            '}' => self.add_token(TokenType::RightBrace, None),
            ',' => self.add_token(TokenType::Comma, None),
            '.' => self.add_token(TokenType::Dot, None),
            '-' => self.add_token(TokenType::Minus, None),
            '+' => self.add_token(TokenType::Plus, None),
            ';' => self.add_token(TokenType::Semicolon, None),
            '*' => self.add_token(TokenType::Star, None),
            // Double character tokens
            '!' => self.match_char_and_add_token('=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.match_char_and_add_token('=', TokenType::EqualEqual, TokenType::Equal),
            '<' => self.match_char_and_add_token('=', TokenType::LessEqual, TokenType::Less),
            '>' => self.match_char_and_add_token('>', TokenType::GreaterEqual, TokenType::Greater),
            '/' => {
                // Comment
                if self.get_char() == '/' {
                    // Traverse until the end of this line
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash, None);
                }
            }
            ' ' | '\r' | '\t' => {
                // do nothing
            }
            '\n' => self.line += 1,
            // String literals
            '"' => self.add_string()?,
            _ => {
                // base 10
                if current_char.is_ascii_digit() {
                    self.add_number()?;
                    // identifier
                } else if current_char.is_ascii_alphabetic() || current_char == '_' {
                    self.add_identifier();
                } else {
                    report_error(self.line, format!("Unexpected character {}", current_char));
                    return Err("Unexpected character".into());
                }
            }
        }
        Ok(())
    }

    fn add_string(&mut self) -> Result<()> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            let l = &self.source[self.start..self.current];
            report_error(self.line, format!("Unterminated String literal {}", l));
            return Err("unterminated String literal".into());
        }
        // advance to conver the closing '"'
        self.advance();
        // get the value from "[...]", excluding the '"'
        let string = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token(TokenType::String, Literal::opt_string(string));
        Ok(())
    }

    fn add_number(&mut self) -> Result<()> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == '.' {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        let number_string = &self.source[self.start..self.current];
        if let Ok(number) = number_string.parse::<f64>() {
            self.add_token(TokenType::Number, Literal::opt_number(number))
        } else {
            report_error(self.line, format!("{} Not a valid number", number_string));
            return Err("Invalid number".into());
        }
        Ok(())
    }

    fn add_identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = self.source[self.start..self.current].to_string();
        if let Some(v) = self.key_words.get(text.as_str()) {
            let t = *v;
            self.add_token(t, None);
        } else {
            let id = text.to_string();
            self.add_token(TokenType::Identifier, Literal::opt_identifier(id));
        }
    }

    fn add_token(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(token_type, lexeme.into(), self.line, literal))
    }

    fn get_char_and_advance(&mut self) -> char {
        let c = self.get_char();
        self.advance();
        c
    }

    fn match_char_and_add_token(&mut self, c: char, if_matches: TokenType, if_not: TokenType) {
        if self.next_char_is(c) {
            self.add_token(if_matches, None)
        } else {
            self.add_token(if_not, None)
        }
    }

    fn next_char_is(&self, c: char) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.get_char() == c
        }
    }

    fn get_char(&self) -> char {
        let v = &self.source[self.current..(self.current + 1)];
        v.chars().next().expect("Character expected")
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.get_char()
        }
    }

    fn advance(&mut self) {
        self.current += 1
    }
}

#[cfg(test)]
mod tests {
    use crate::tokens::{Literal, Token, TokenType};

    use super::Scanner;

    use crate::errors::*;

    #[test]
    fn scanner_tests() -> Result<()> {
        // Single line test
        let mut source = r#"var language = "lox""#;
        let mut scanner = Scanner::new(source.into());
        let mut tokens = scanner.scan_tokens()?;
        let expected = &[
            Token::new(TokenType::Var, "var".into(), 1, None),
            Token::new(
                TokenType::Identifier,
                "language".into(),
                1,
                Some(Literal::Identifier("language".into())),
            ),
            Token::new(TokenType::Equal, "=".into(), 1, None),
            Token::new(
                TokenType::String,
                "\"lox\"".into(),
                1,
                Some(Literal::String("lox".into())),
            ),
            Token::new(TokenType::Eof, "".into(), 1, None),
        ];
        assert_eq!(expected, tokens);

        source = r#"
        var pi = 3.14 // pi
        // random comment
        var two_pi = (pi) * 2
        "#;
        scanner = Scanner::new(source.into());
        tokens = scanner.scan_tokens()?;
        let expected = &[
            Token::new(TokenType::Var, "var".into(), 2, None),
            Token::new(
                TokenType::Identifier,
                "pi".into(),
                2,
                Some(Literal::Identifier("pi".into())),
            ),
            Token::new(TokenType::Equal, "=".into(), 2, None),
            Token::new(
                TokenType::Number,
                "3.14".into(),
                2,
                #[allow(clippy::approx_constant)]
                Some(Literal::Number(3.14)),
            ),
            Token::new(TokenType::Var, "var".into(), 4, None),
            Token::new(
                TokenType::Identifier,
                "two_pi".into(),
                4,
                Some(Literal::Identifier("two_pi".into())),
            ),
            Token::new(TokenType::Equal, "=".into(), 4, None),
            Token::new(TokenType::LeftParen, "(".into(), 4, None),
            Token::new(
                TokenType::Identifier,
                "pi".into(),
                4,
                Some(Literal::Identifier("pi".into())),
            ),
            Token::new(TokenType::RightParen, ")".into(), 4, None),
            Token::new(TokenType::Star, "*".into(), 4, None),
            Token::new(TokenType::Number, "2".into(), 4, Some(Literal::Number(2.0))),
            Token::new(TokenType::Eof, "".into(), 5, None),
        ];
        assert_eq!(expected, tokens);
        Ok(())
    }
}

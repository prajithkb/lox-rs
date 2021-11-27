use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    OR,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}
#[derive(Debug, PartialEq)]
pub enum Literal {
    String(String),
    Identifier(String),
    Number(f64),
}

#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    line: usize,
    literal: Option<Literal>,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        line: usize,
        literal: Option<Literal>,
    ) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            literal,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "type {:?} lexeme {} literal {:?}",
            self.token_type, self.lexeme, self.literal
        ))
    }
}

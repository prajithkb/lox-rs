use log::log_enabled;
use num_enum::{FromPrimitive, IntoPrimitive};

use crate::{
    chunk::{Chunk, Opcode, Value},
    errors::*,
    tokens::TokenType,
};

use crate::tokens::Token;

fn parse_error(token: &Token, message: &str) -> ErrorKind {
    ErrorKind::ParseError(format!(
        "[line: {}] Error at <{}>: message: {}",
        token.line, token.lexeme, message
    ))
}
#[repr(usize)]
#[derive(Debug, FromPrimitive, IntoPrimitive, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    #[num_enum(default)]
    None = 0,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn higher_precedence(&self) -> Precedence {
        let current: usize = (*self).into();
        (current + 1).into()
    }
}

type ParseFn<'a> = fn(&mut Compiler<'a>) -> Result<()>;
struct ParseRule<'a> {
    token_type: TokenType,
    prefix_function: Option<ParseFn<'a>>,
    infix_function: Option<ParseFn<'a>>,
    precedence: Precedence,
}

impl<'r, 'a> std::fmt::Debug for ParseRule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseRule")
            .field("token_type", &self.token_type)
            .field("precedence", &self.precedence)
            .finish()
    }
}

impl<'r, 'a> ParseRule<'a> {
    fn new(
        token_type: TokenType,
        prefix_function: Option<ParseFn<'a>>,
        infix_function: Option<ParseFn<'a>>,
        precedence: Precedence,
    ) -> Self {
        ParseRule {
            token_type,
            prefix_function,
            infix_function,
            precedence,
        }
    }
}

#[derive(Debug)]
pub struct Compiler<'a> {
    tokens: &'a [Token],
    compiling_chunk: Chunk,
    current: usize,
    parse_rules: Vec<ParseRule<'a>>,
}
#[allow(dead_code)]
impl<'a> Compiler<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let mut c = Compiler {
            tokens,
            compiling_chunk: Chunk::default(),
            current: 0,
            parse_rules: Vec::new(),
        };
        c.init_parse_rules();
        c
    }

    fn init_parse_rules(&mut self) {
        self.parse_rules = vec![
            ParseRule::new(
                TokenType::LeftParen,
                Some(Compiler::grouping),
                None,
                Precedence::None,
            ),
            ParseRule::new(TokenType::RightParen, None, None, Precedence::None),
            ParseRule::new(TokenType::LeftBrace, None, None, Precedence::None),
            ParseRule::new(TokenType::RightBrace, None, None, Precedence::None),
            ParseRule::new(TokenType::Comma, None, None, Precedence::None),
            ParseRule::new(TokenType::Dot, None, None, Precedence::None),
            ParseRule::new(
                TokenType::Minus,
                Some(Compiler::unary),
                Some(Compiler::binary),
                Precedence::Term,
            ),
            ParseRule::new(
                TokenType::Plus,
                None,
                Some(Compiler::binary),
                Precedence::Term,
            ),
            ParseRule::new(TokenType::Semicolon, None, None, Precedence::None),
            ParseRule::new(
                TokenType::Slash,
                None,
                Some(Compiler::binary),
                Precedence::Factor,
            ),
            ParseRule::new(
                TokenType::Star,
                None,
                Some(Compiler::binary),
                Precedence::Factor,
            ),
            ParseRule::new(TokenType::Bang, None, None, Precedence::None),
            ParseRule::new(TokenType::BangEqual, None, None, Precedence::None),
            ParseRule::new(TokenType::Equal, None, None, Precedence::None),
            ParseRule::new(TokenType::EqualEqual, None, None, Precedence::None),
            ParseRule::new(TokenType::Greater, None, None, Precedence::None),
            ParseRule::new(TokenType::GreaterEqual, None, None, Precedence::None),
            ParseRule::new(TokenType::Less, None, None, Precedence::None),
            ParseRule::new(TokenType::LessEqual, None, None, Precedence::None),
            ParseRule::new(TokenType::Identifier, None, None, Precedence::None),
            ParseRule::new(TokenType::String, None, None, Precedence::None),
            ParseRule::new(
                TokenType::Number,
                Some(Compiler::number),
                None,
                Precedence::None,
            ),
            ParseRule::new(TokenType::And, None, None, Precedence::None),
            ParseRule::new(TokenType::Class, None, None, Precedence::None),
            ParseRule::new(TokenType::Else, None, None, Precedence::None),
            ParseRule::new(TokenType::False, None, None, Precedence::None),
            ParseRule::new(TokenType::For, None, None, Precedence::None),
            ParseRule::new(TokenType::Fun, None, None, Precedence::None),
            ParseRule::new(TokenType::If, None, None, Precedence::None),
            ParseRule::new(TokenType::Nil, None, None, Precedence::None),
            ParseRule::new(TokenType::Or, None, None, Precedence::None),
            ParseRule::new(TokenType::Print, None, None, Precedence::None),
            ParseRule::new(TokenType::Return, None, None, Precedence::None),
            ParseRule::new(TokenType::Super, None, None, Precedence::None),
            ParseRule::new(TokenType::This, None, None, Precedence::None),
            ParseRule::new(TokenType::True, None, None, Precedence::None),
            ParseRule::new(TokenType::Var, None, None, Precedence::None),
            ParseRule::new(TokenType::While, None, None, Precedence::None),
            ParseRule::new(TokenType::Eof, None, None, Precedence::None),
        ]
    }

    pub fn compile(mut self) -> Result<Chunk> {
        self.expression()?;
        self.consume_next_token(TokenType::Eof, "Expect end of expression")?;
        self.end_compiler();
        Ok(self.compiling_chunk)
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        self.advance();
        match self.get_rule(self.previous().token_type).prefix_function {
            Some(prefix_rule) => prefix_rule(self)?,
            None => bail!(parse_error(self.previous(), "Expect expression")),
        };
        while precedence <= self.get_rule(self.current().token_type).precedence {
            self.advance();
            match self.get_rule(self.previous().token_type).infix_function {
                Some(infix_rule) => infix_rule(self)?,
                None => bail!(parse_error(self.previous(), "Expect expression")),
            };
        }
        Ok(())
    }

    fn number(&mut self) -> Result<()> {
        let str = &self.previous().lexeme;
        let value: Value = str::parse::<Value>(str)
            .map_err(|_| ErrorKind::ParseError(format!("{} not a number", str)))?;
        self.emit_constant(value);
        Ok(())
    }

    fn grouping(&mut self) -> Result<()> {
        self.expression()?;
        self.consume_next_token(TokenType::RightParen, "Expect ')' after expression")?;
        Ok(())
    }

    fn unary(&mut self) -> Result<()> {
        let token_type = self.previous().token_type;
        self.parse_precedence(Precedence::Unary)?;
        match token_type {
            TokenType::Minus => self.emit_op_code(Opcode::Negate),
            _ => bail!(parse_error(
                self.previous(),
                "Cannot perform unary operation."
            )),
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<()> {
        let prev_token = self.previous().clone();
        let operator = prev_token.token_type;
        let rule = self.get_rule(operator);
        let next_precedence = rule.precedence.higher_precedence();
        self.parse_precedence(next_precedence)?;
        match operator {
            TokenType::Plus => self.emit_op_code(Opcode::Add),
            TokenType::Minus => self.emit_op_code(Opcode::Subtract),
            TokenType::Star => self.emit_op_code(Opcode::Multiply),
            TokenType::Slash => self.emit_op_code(Opcode::Divide),
            _ => bail!(parse_error(&prev_token, "Invalid operator")),
        }
        Ok(())
    }

    fn get_rule(&mut self, token_type: TokenType) -> &ParseRule<'a> {
        let index: usize = token_type.into();
        &self.parse_rules[index]
    }

    fn emit_constant(&mut self, value: Value) {
        let offset = self.current_chunk().add_constant(value);
        self.emit_opcode_and_bytes(Opcode::Constant, offset);
    }
    fn end_compiler(&mut self) {
        self.emit_return();
        if log_enabled!(log::Level::Trace) {
            self.current_chunk().disassemble_chunk("code");
        }
    }

    fn emit_return(&mut self) {
        self.emit_op_code(Opcode::Return)
    }

    #[inline]
    fn emit_op_code(&mut self, opcode: Opcode) {
        self.emit_byte(opcode.into())
    }

    #[inline]
    fn emit_byte(&mut self, byte: u8) {
        let line = self.previous().line;
        self.current_chunk().write_chunk(byte, line);
    }

    #[inline]
    fn emit_opcode_and_bytes(&mut self, op_code: Opcode, byte: u8) {
        self.emit_op_code(op_code);
        self.emit_byte(byte);
    }

    #[inline]
    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.compiling_chunk
    }

    fn consume_next_token(
        &mut self,
        token_type_to_match: TokenType,
        message: &str,
    ) -> Result<&Token> {
        let t = self.current();
        if t.token_type == token_type_to_match {
            self.advance();
            Ok(self.current())
        } else {
            let previous = self.previous();
            bail!(parse_error(previous, message))
        }
    }
    fn match_and_advance(&mut self, token_types_to_match: &[TokenType]) -> bool {
        if self.is_at_end() {
            return false;
        }
        for &token_type in token_types_to_match {
            if token_type == self.current().token_type {
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
            match self.current().token_type {
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
    fn current(&self) -> &Token {
        &self.tokens[self.current]
    }

    #[inline]
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    #[inline]
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.current().token_type == TokenType::Eof
    }
}

#[cfg(test)]
mod tests {
    use crate::{errors::*, scanner::Scanner, tokens::pretty_print};

    use super::Compiler;

    fn utf8_to_string(bytes: &[u8]) -> String {
        match String::from_utf8(bytes.to_vec()) {
            Ok(s) => s,
            Err(_) => String::new(),
        }
    }

    #[test]
    fn number() -> Result<()> {
        let source = r#"3.14"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            "== test ==\n0000 0001 OpCode[Constant]    0 '3.14'\n0002    | OpCode[Return]\n",
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn grouping() -> Result<()> {
        let source = r#"(3.14)"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            "== test ==\n0000 0001 OpCode[Constant]    0 '3.14'\n0002    | OpCode[Return]\n",
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn unary() -> Result<()> {
        let source = r#"-3.14"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            "== test ==\n0000 0001 OpCode[Constant]    0 '3.14'\n0002    | OpCode[Negate]\n0003    | OpCode[Return]\n",
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn binary() -> Result<()> {
        let source = r#"4-3*2+8/4"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            "== test ==\n0000 0001 OpCode[Constant]    0 '4.0'\n0002    | OpCode[Constant]    1 '3.0'\n0004    | OpCode[Constant]    2 '2.0'\n0006    | OpCode[Multiply]\n0007    | OpCode[Subtract]\n0008    | OpCode[Constant]    3 '8.0'\n0010    | OpCode[Constant]    4 '4.0'\n0012    | OpCode[Divide]\n0013    | OpCode[Add]\n0014    | OpCode[Return]\n",utf8_to_string(&buf)
        );
        Ok(())
    }
}

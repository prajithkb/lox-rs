use log::log_enabled;
use num_enum::{FromPrimitive, IntoPrimitive};

use crate::{
    chunk::{Chunk, Object, ObjectType, Value},
    errors::*,
    instructions::Opcode,
    tokens::{Literal, TokenType},
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

type ParseFn<'a> = fn(&mut Compiler<'a>, bool) -> Result<()>;
struct ParseRule<'a> {
    token_type: TokenType,
    prefix_function: Option<ParseFn<'a>>,
    infix_function: Option<ParseFn<'a>>,
    precedence: Precedence,
}

impl<'a> std::fmt::Debug for ParseRule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseRule")
            .field("token_type", &self.token_type)
            .field("precedence", &self.precedence)
            .finish()
    }
}

impl<'a> ParseRule<'a> {
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
struct Scope<'a> {
    locals: Vec<Local<'a>>,
    depth: usize,
}

impl<'a> Scope<'a> {
    #[allow(clippy::new_without_default)]
    fn new() -> Self {
        Scope {
            locals: Vec::new(),
            depth: 0,
        }
    }
}

const GLOBAL_SCOPE_DEPTH: usize = 0;

#[derive(Debug)]
struct Local<'a> {
    name: &'a Token,
    depth: Option<usize>,
}

impl<'a> Local<'a> {
    #[allow(clippy::new_without_default)]
    fn new(name: &'a Token, depth: Option<usize>) -> Self {
        Local { name, depth }
    }
}

#[derive(Debug)]
pub struct Compiler<'a> {
    tokens: &'a [Token],
    compiling_chunk: Chunk,
    current_index: usize,
    parse_rules: Vec<ParseRule<'a>>,
    current_scope: Scope<'a>,
}
#[allow(dead_code)]
impl<'a> Compiler<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let mut c = Compiler {
            tokens,
            compiling_chunk: Chunk::default(),
            current_index: 0,
            parse_rules: Vec::new(),
            current_scope: Scope::new(),
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
            ParseRule::new(
                TokenType::Bang,
                Some(Compiler::unary),
                None,
                Precedence::None,
            ),
            ParseRule::new(
                TokenType::BangEqual,
                None,
                Some(Compiler::binary),
                Precedence::Equality,
            ),
            ParseRule::new(TokenType::Equal, None, None, Precedence::None),
            ParseRule::new(
                TokenType::EqualEqual,
                None,
                Some(Compiler::binary),
                Precedence::Equality,
            ),
            ParseRule::new(
                TokenType::Greater,
                None,
                Some(Compiler::binary),
                Precedence::Comparison,
            ),
            ParseRule::new(
                TokenType::GreaterEqual,
                Some(Compiler::binary),
                None,
                Precedence::Comparison,
            ),
            ParseRule::new(
                TokenType::Less,
                None,
                Some(Compiler::binary),
                Precedence::Comparison,
            ),
            ParseRule::new(
                TokenType::LessEqual,
                None,
                Some(Compiler::binary),
                Precedence::Comparison,
            ),
            ParseRule::new(
                TokenType::Identifier,
                Some(Compiler::variable_usage),
                None,
                Precedence::None,
            ),
            ParseRule::new(
                TokenType::String,
                Some(Compiler::string),
                None,
                Precedence::None,
            ),
            ParseRule::new(
                TokenType::Number,
                Some(Compiler::number),
                None,
                Precedence::None,
            ),
            ParseRule::new(TokenType::And, None, None, Precedence::None),
            ParseRule::new(TokenType::Class, None, None, Precedence::None),
            ParseRule::new(TokenType::Else, None, None, Precedence::None),
            ParseRule::new(
                TokenType::False,
                Some(Compiler::literal),
                None,
                Precedence::None,
            ),
            ParseRule::new(TokenType::For, None, None, Precedence::None),
            ParseRule::new(TokenType::Fun, None, None, Precedence::None),
            ParseRule::new(TokenType::If, None, None, Precedence::None),
            ParseRule::new(
                TokenType::Nil,
                Some(Compiler::literal),
                None,
                Precedence::None,
            ),
            ParseRule::new(TokenType::Or, None, None, Precedence::None),
            ParseRule::new(TokenType::Print, None, None, Precedence::None),
            ParseRule::new(TokenType::Return, None, None, Precedence::None),
            ParseRule::new(TokenType::Super, None, None, Precedence::None),
            ParseRule::new(TokenType::This, None, None, Precedence::None),
            ParseRule::new(
                TokenType::True,
                Some(Compiler::literal),
                None,
                Precedence::None,
            ),
            ParseRule::new(TokenType::Var, None, None, Precedence::None),
            ParseRule::new(TokenType::While, None, None, Precedence::None),
            ParseRule::new(TokenType::Eof, None, None, Precedence::None),
        ]
    }

    pub fn compile(mut self) -> Result<Chunk> {
        while !self.is_at_end() {
            self.declaration()?;
        }
        self.end_compiler();
        Ok(self.compiling_chunk)
    }

    fn declaration(&mut self) -> Result<()> {
        if self.match_and_advance(&[TokenType::Var]) {
            self.var_declaration()?;
        } else {
            self.statement()?;
        }
        Ok(())
    }

    fn var_declaration(&mut self) -> Result<()> {
        let global = self.parse_variable("Expect variable name")?;
        if self.match_and_advance(&[TokenType::Equal]) {
            self.expression()?;
        } else {
            self.emit_op_code(Opcode::Nil);
        }
        self.consume_next_token(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        self.define_variable(global);
        Ok(())
    }

    fn variable_usage(&mut self, can_assign: bool) -> Result<()> {
        self.named_variable(self.previous().clone(), can_assign)
    }

    fn named_variable(&mut self, token: Token, can_assign: bool) -> Result<()> {
        let mut get_op = Opcode::GetGlobal;
        let mut set_op = Opcode::SetGlobal;
        let arg;
        if let Some(byte) = self.resolve_local(&token)? {
            get_op = Opcode::GetLocal;
            set_op = Opcode::SetLocal;
            arg = byte;
        } else {
            arg = self.identifier_constant(token)?;
        }
        if can_assign && self.match_and_advance(&[TokenType::Equal]) {
            self.expression()?;
            self.emit_opcode_and_bytes(set_op, arg)
        } else {
            self.emit_opcode_and_bytes(get_op, arg);
        }
        Ok(())
    }

    fn resolve_local(&self, name: &Token) -> Result<Option<u8>> {
        let mut i = self.current_scope.locals.len() as i32 - 1;
        while i >= 0 {
            let index = i as usize;
            if self.current_scope.locals[index].name.lexeme == name.lexeme {
                if self.current_scope.locals[index].depth.is_none() {
                    bail!(parse_error(
                        name,
                        "Can't read local variable in its own initializer"
                    ))
                }
                return Ok(Some(i as u8));
            }
            i -= 1;
        }
        Ok(None)
    }

    fn define_variable(&mut self, byte: u8) {
        if self.current_scope.depth == GLOBAL_SCOPE_DEPTH {
            self.emit_opcode_and_bytes(Opcode::DefineGlobal, byte);
        } else {
            self.mark_initialized();
        }
    }

    fn statement(&mut self) -> Result<()> {
        if self.match_and_advance(&[TokenType::Print]) {
            self.print_statement()?;
        } else if self.match_and_advance(&[TokenType::LeftBrace]) {
            self.begin_scope();
            self.block()?;
            self.end_scope();
        } else {
            self.expression_statement()?;
        }
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.current_scope.depth += 1;
    }

    fn block(&mut self) -> Result<()> {
        while self.current().token_type != TokenType::RightBrace
            && self.current().token_type != TokenType::Eof
        {
            self.declaration()?;
        }
        self.consume_next_token(TokenType::RightBrace, "Expect '}' after block")?;
        Ok(())
    }

    fn end_scope(&mut self) {
        self.current_scope.depth -= 1;
        let mut i: i32 = (self.current_scope.locals.len() - 1) as i32;
        while i >= 0 {
            if self.current_scope.locals[i as usize]
                .depth
                .expect("Expect depth")
                > self.current_scope.depth
            {
                self.current_scope.locals.pop();
                self.emit_op_code(Opcode::Pop);
            } else {
                break;
            }
            i -= 1;
        }
    }

    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume_next_token(TokenType::Semicolon, "Expect ';' after print statement")?;
        self.emit_op_code(Opcode::Print);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume_next_token(TokenType::Semicolon, "Expect ';' after expression")?;
        self.emit_op_code(Opcode::Pop);
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        self.advance();
        let previous = self.previous().token_type;
        let can_assign = precedence <= Precedence::Assignment;
        match self.get_rule(previous).prefix_function {
            Some(prefix_rule) => prefix_rule(self, can_assign)?,
            None => bail!(parse_error(self.previous(), "Expect expression")),
        };

        while precedence <= self.get_rule(self.current().token_type).precedence {
            self.advance();
            match self.get_rule(self.previous().token_type).infix_function {
                Some(infix_rule) => infix_rule(self, can_assign)?,
                None => bail!(parse_error(self.previous(), "Expect expression")),
            };
        }
        if can_assign && self.match_and_advance(&[TokenType::Equal]) {
            bail!(parse_error(self.previous(), "Invalid assignment target"))
        }
        Ok(())
    }

    fn number(&mut self, _can_assign: bool) -> Result<()> {
        if let Some(Literal::Number(n)) = &self.previous().literal {
            let value = Value::Number(*n);
            self.emit_constant(value);
            Ok(())
        } else {
            bail!(parse_error(self.previous(), "not a number"))
        }
    }

    fn string(&mut self, _can_assign: bool) -> Result<()> {
        if let Some(Literal::String(s)) = &self.previous().literal {
            let value = Value::Object(Object::new(1, ObjectType::String(s.to_string())));
            self.emit_constant(value);
            Ok(())
        } else {
            bail!(parse_error(self.previous(), "not a string"))
        }
    }

    fn grouping(&mut self, _can_assign: bool) -> Result<()> {
        self.expression()?;
        self.consume_next_token(TokenType::RightParen, "Expect ')' after expression")?;
        Ok(())
    }

    fn unary(&mut self, _can_assign: bool) -> Result<()> {
        let token_type = self.previous().token_type;
        self.parse_precedence(Precedence::Unary)?;
        match token_type {
            TokenType::Minus => self.emit_op_code(Opcode::Negate),
            TokenType::Bang => self.emit_op_code(Opcode::Not),
            _ => bail!(parse_error(
                self.previous(),
                "Cannot perform unary operation."
            )),
        }
        Ok(())
    }

    fn binary(&mut self, _can_assign: bool) -> Result<()> {
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
            TokenType::BangEqual => self.emit_op_code(Opcode::BangEqual),
            TokenType::EqualEqual => self.emit_op_code(Opcode::EqualEqual),
            TokenType::Greater => self.emit_op_code(Opcode::Greater),
            TokenType::GreaterEqual => self.emit_op_code(Opcode::GreaterEqual),
            TokenType::Less => self.emit_op_code(Opcode::Less),
            TokenType::LessEqual => self.emit_op_code(Opcode::LessEqual),
            _ => bail!(parse_error(&prev_token, "Invalid operator (to be impl?)")),
        }
        Ok(())
    }

    fn literal(&mut self, _can_assign: bool) -> Result<()> {
        match self.previous().token_type {
            TokenType::Nil => self.emit_op_code(Opcode::Nil),
            TokenType::True => self.emit_op_code(Opcode::True),
            TokenType::False => self.emit_op_code(Opcode::False),
            _ => bail!(parse_error(self.previous(), "Not a literal")),
        };
        Ok(())
    }

    #[inline]
    fn get_rule(&mut self, token_type: TokenType) -> &ParseRule<'a> {
        let index: usize = token_type.into();
        &self.parse_rules[index]
    }

    #[inline]
    fn emit_constant(&mut self, value: Value) {
        let offset = self.add_constant(value);
        self.emit_opcode_and_bytes(Opcode::Constant, offset);
    }

    #[inline]
    fn add_constant(&mut self, value: Value) -> u8 {
        self.current_chunk().add_constant(value)
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        if log_enabled!(log::Level::Trace) {
            self.current_chunk().disassemble_chunk("code");
            println!("==========")
        }
    }
    #[inline]
    fn emit_return(&mut self) {
        self.emit_op_code(Opcode::Return)
    }

    fn parse_variable(&mut self, message: &str) -> Result<u8> {
        self.consume_next_token(TokenType::Identifier, message)?;
        self.declare_local_variable()?;
        if self.current_scope.depth > GLOBAL_SCOPE_DEPTH {
            return Ok(0);
        }
        self.identifier_constant(self.previous().clone())
    }

    fn declare_local_variable(&mut self) -> Result<()> {
        if self.current_scope.depth > GLOBAL_SCOPE_DEPTH {
            let token = self.previous();
            for local in self.current_scope.locals.iter().rev() {
                if let Some(depth) = &local.depth {
                    if *depth < self.current_scope.depth {
                        break;
                    }
                    if local.name.lexeme == token.lexeme {
                        bail!(parse_error(
                            token,
                            "Already a variable with this name exists in this scope"
                        ))
                    }
                }
            }
            self.add_local(token);
        }
        Ok(())
    }

    fn add_local(&mut self, token: &'a Token) {
        let local = Local::new(token, None);
        self.current_scope.locals.push(local);
    }

    fn mark_initialized(&mut self) {
        let locals_count = self.current_scope.locals.len();
        self.current_scope.locals[locals_count - 1].depth = Some(self.current_scope.depth);
    }

    fn identifier_constant(&mut self, mut token: Token) -> Result<u8> {
        let literal = token.literal.take();
        if let Literal::Identifier(s) = literal.expect("Expect string") {
            let name = Value::Object(Object::new(1, ObjectType::String(s)));
            Ok(self.add_constant(name))
        } else {
            bail!(parse_error(&token, "Expect identifier"))
        }
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
    fn current(&self) -> &'a Token {
        &self.tokens[self.current_index]
    }

    #[inline]
    fn previous(&self) -> &'a Token {
        &self.tokens[self.current_index - 1]
    }

    #[inline]
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current_index += 1;
        }
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.current().token_type == TokenType::Eof
    }
}

#[cfg(test)]
mod tests {
    use crate::{errors::*, lox::utf8_to_string, scanner::Scanner};

    use super::Compiler;

    #[test]
    fn number() -> Result<()> {
        let source = r#"3.14;"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            "== test ==\n0000 0001 OpCode[Constant]    0 '3.14'\n0002    | OpCode[Pop]\n0003    | OpCode[Return]\n",
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn grouping() -> Result<()> {
        let source = r#"(3.14);"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            "== test ==\n0000 0001 OpCode[Constant]    0 '3.14'\n0002    | OpCode[Pop]\n0003    | OpCode[Return]\n",
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn unary() -> Result<()> {
        let source = r#"-3.14;"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            "== test ==\n0000 0001 OpCode[Constant]    0 '3.14'\n0002    | OpCode[Negate]\n0003    | OpCode[Pop]\n0004    | OpCode[Return]\n",
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn binary() -> Result<()> {
        let source = r#"4-3*2+8/4;"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]    0 '4'
0002    | OpCode[Constant]    1 '3'
0004    | OpCode[Constant]    2 '2'
0006    | OpCode[Multiply]
0007    | OpCode[Subtract]
0008    | OpCode[Constant]    3 '8'
0010    | OpCode[Constant]    4 '4'
0012    | OpCode[Divide]
0013    | OpCode[Add]
0014    | OpCode[Pop]
0015    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );

        let source = r#"!(5 - 4 > 3 * 2 == !nil);"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]    0 '5'
0002    | OpCode[Constant]    1 '4'
0004    | OpCode[Subtract]
0005    | OpCode[Constant]    2 '3'
0007    | OpCode[Constant]    3 '2'
0009    | OpCode[Multiply]
0010    | OpCode[Greater]
0011    | OpCode[Nil]
0012    | OpCode[Not]
0013    | OpCode[EqualEqual]
0014    | OpCode[Not]
0015    | OpCode[Pop]
0016    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn string() -> Result<()> {
        let source = r#""Hello " + " world"; "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]    0 'Hello '
0002    | OpCode[Constant]    1 ' world'
0004    | OpCode[Add]
0005    | OpCode[Pop]
0006    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn print() -> Result<()> {
        let source = r#"print 3 + 3;"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]    0 '3'
0002    | OpCode[Constant]    1 '3'
0004    | OpCode[Add]
0005    | OpCode[Print]
0006    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn var_declaration() -> Result<()> {
        let source = r#"var a =2;"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]    1 '2'
0002    | OpCode[DefineGlobal]    0 'a'
0004    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn block() -> Result<()> {
        let source = r#"
        var a = 2;
        {
            print a;
            var a = 3;
            print a;
            var b = a;
        }
        print a;
        print b;
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let chunk = compiler.compile()?;
        let mut buf = vec![];
        chunk.disassemble_chunk_with_writer("test", &mut buf);
        assert_eq!(
            r#"== test ==
0000 0002 OpCode[Constant]    1 '2'
0002    | OpCode[DefineGlobal]    0 'a'
0004 0004 OpCode[GetGlobal]    2 'a'
0006    | OpCode[Print]
0007 0005 OpCode[Constant]    3 '3'
0009 0006 OpCode[GetLocal]    0
0011    | OpCode[Print]
0012 0007 OpCode[GetLocal]    0
0014 0008 OpCode[Pop]
0015    | OpCode[Pop]
0016 0009 OpCode[GetGlobal]    4 'a'
0018    | OpCode[Print]
0019 0010 OpCode[GetGlobal]    5 'b'
0021    | OpCode[Print]
0022    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }
}

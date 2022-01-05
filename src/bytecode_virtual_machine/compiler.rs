use std::{
    collections::{linked_list::IterMut, LinkedList},
    io::stdout,
    iter::Rev,
    rc::Rc,
};

use log::log_enabled;
use num_enum::{FromPrimitive, IntoPrimitive};

use crate::{
    common::lox::Writer,
    common::tokens::{Literal, Token, TokenType},
    errors::*,
};

use super::{
    chunk::Chunk,
    instructions::Opcode,
    objects::{Function, Object, UserDefinedFunction, Value},
};

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
struct Upvalue {
    index: u8,
    is_local: bool,
}

impl Upvalue {
    pub fn new(index: u8, is_local: bool) -> Self {
        Upvalue { index, is_local }
    }
}

struct ClassCompiler {}

impl ClassCompiler {
    pub fn new() -> Self {
        ClassCompiler {}
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
    name: &'a str,
    depth: Option<usize>,
    is_captured: bool,
}

impl<'a> Local<'a> {
    #[allow(clippy::new_without_default)]
    fn new(name: &'a str, depth: Option<usize>) -> Self {
        Local {
            name,
            depth,
            is_captured: false,
        }
    }
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FunctionType {
    Script,
    Function,
    Method,
    Initializer,
}

#[derive(Debug)]
struct State<'a> {
    function: Function,
    scope: Scope<'a>,
    function_type: FunctionType,
    upvalues: Vec<Upvalue>,
}

impl<'a> State<'a> {
    fn new(function: Function, scope: Scope<'a>, function_type: FunctionType) -> Self {
        State {
            function,
            scope,
            function_type,
            upvalues: Vec::new(),
        }
    }
}

pub struct Compiler<'a> {
    tokens: &'a [Token],
    token_index: usize,
    parse_rules: Vec<ParseRule<'a>>,
    states: LinkedList<State<'a>>,
    state: State<'a>,
    custom_writer: Writer<'a>,
    current_class: Option<ClassCompiler>,
    class_compilers: LinkedList<ClassCompiler>,
}
#[allow(dead_code)]
impl<'a> Compiler<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Compiler::new_with_type_and_writer(tokens, FunctionType::Script, None)
    }

    pub fn new_with_type(tokens: &'a [Token], function_type: FunctionType) -> Self {
        Compiler::new_with_type_and_writer(tokens, function_type, None)
    }

    pub fn new_with_type_and_writer(
        tokens: &'a [Token],
        function_type: FunctionType,
        custom_writer: Writer<'a>,
    ) -> Self {
        let mut c = Compiler {
            tokens,
            token_index: 0,
            parse_rules: Vec::new(),
            state: State::new(
                Function::UserDefined(UserDefinedFunction::new(0, Chunk::new(), "".to_string())),
                Scope::new(),
                function_type,
            ),
            states: LinkedList::new(),
            custom_writer,
            current_class: None,
            class_compilers: LinkedList::new(),
        };
        c.current_scope_mut().locals.push(Local::new("", Some(0)));
        c.init_parse_rules();
        c
    }

    fn init_parse_rules(&mut self) {
        self.parse_rules = vec![
            ParseRule::new(
                TokenType::LeftParen,
                Some(Compiler::grouping),
                Some(Compiler::call),
                Precedence::Call,
            ),
            ParseRule::new(TokenType::RightParen, None, None, Precedence::None),
            ParseRule::new(TokenType::LeftBrace, None, None, Precedence::None),
            ParseRule::new(TokenType::RightBrace, None, None, Precedence::None),
            ParseRule::new(TokenType::Comma, None, None, Precedence::None),
            ParseRule::new(TokenType::Dot, None, Some(Compiler::dot), Precedence::Call),
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
                None,
                Some(Compiler::binary),
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
            ParseRule::new(
                TokenType::And,
                None,
                Some(Compiler::logical_and),
                Precedence::And,
            ),
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
            ParseRule::new(
                TokenType::Or,
                None,
                Some(Compiler::logical_or),
                Precedence::Or,
            ),
            ParseRule::new(TokenType::Print, None, None, Precedence::None),
            ParseRule::new(TokenType::Return, None, None, Precedence::None),
            ParseRule::new(TokenType::Super, None, None, Precedence::None),
            ParseRule::new(
                TokenType::This,
                Some(Compiler::this),
                None,
                Precedence::None,
            ),
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

    pub fn compile(mut self) -> Result<Function> {
        while !self.is_at_end() {
            self.declaration()?;
        }
        self.emit_return_and_log();
        Ok(self.state.function)
    }

    fn declaration(&mut self) -> Result<()> {
        if self.match_and_advance(&[TokenType::Class]) {
            self.class_declaration()?;
        } else if self.match_and_advance(&[TokenType::Fun]) {
            self.fun_declaration()?;
        } else if self.match_and_advance(&[TokenType::Var]) {
            self.var_declaration()?;
        } else {
            self.statement()?;
        }
        Ok(())
    }

    fn class_declaration(&mut self) -> Result<()> {
        self.consume_next_token(TokenType::Identifier, "Expect class name")?;
        let class_name = self.previous().clone();
        let name_constant = self.identifier_constant(class_name.clone())?;
        self.declare_local_variable()?;
        self.emit_opcode_and_bytes(Opcode::Class, name_constant);
        self.define_variable(name_constant);
        if let Some(current_compiler) = self.current_class.take() {
            self.class_compilers.push_back(current_compiler);
        }
        self.current_class = Some(ClassCompiler::new());
        // this will bring the variable back on top of the stack
        self.named_variable(class_name, false)?;
        self.consume_next_token(TokenType::LeftBrace, "Expect '{' before class body")?;
        while self.current().token_type != TokenType::RightBrace && !self.is_at_end() {
            self.method()?;
        }
        self.consume_next_token(TokenType::RightBrace, "Expect '}' after class body")?;
        self.emit_op_code(Opcode::Pop); // pop the class
        let prev_class_compiler = self.class_compilers.pop_back();
        self.current_class = prev_class_compiler;
        Ok(())
    }

    fn method(&mut self) -> Result<()> {
        self.consume_next_token(TokenType::Identifier, "Expect method name")?;
        let method_name = self.previous().clone();
        let mut function_type = FunctionType::Method;
        if let Some(Literal::Identifier(s)) = &method_name.literal {
            if s == "init" {
                function_type = FunctionType::Initializer;
            }
        }
        self.function(function_type)?;
        let constant = self.identifier_constant(method_name)?;
        self.emit_opcode_and_bytes(Opcode::Method, constant);
        Ok(())
    }

    fn start_new_function(&mut self, function_type: FunctionType) -> Result<()> {
        let new_function_name = self.function_name(function_type)?;
        let new_function =
            Function::UserDefined(UserDefinedFunction::new(0, Chunk::new(), new_function_name));
        let mut new_scope = Scope::new();
        if function_type != FunctionType::Function {
            new_scope.locals.push(Local::new("this", Some(0)));
        } else {
            new_scope.locals.push(Local::new("", Some(0)));
        }
        let current_state = std::mem::replace(
            &mut self.state,
            State::new(new_function, new_scope, function_type),
        );
        self.states.push_back(current_state);
        Ok(())
    }

    fn end_new_function(&mut self) -> State<'a> {
        let prev_state = self.states.pop_back().expect("State expected");
        std::mem::replace(&mut self.state, prev_state)
    }

    fn fun_declaration(&mut self) -> Result<()> {
        let global = self.parse_variable("Expect function name")?;
        self.mark_initialized();
        self.function(FunctionType::Function)?;
        self.define_variable(global);
        Ok(())
    }

    fn function(&mut self, function_type: FunctionType) -> Result<()> {
        self.start_new_function(function_type)?;
        self.begin_scope();
        self.consume_next_token(TokenType::LeftParen, "Expect '(' after function name")?;
        while self.current().token_type != TokenType::RightParen {
            match &mut self.state.function {
                Function::UserDefined(u) => u.arity += 1,
            }
            let constant = self.parse_variable("Expect parameter name")?;
            self.define_variable(constant);
            if self.current().token_type == TokenType::Comma {
                self.advance();
            } else {
                break;
            }
        }
        self.consume_next_token(TokenType::RightParen, "Expect ')' after parameters")?;
        self.consume_next_token(TokenType::LeftBrace, "Expect '{' before function body")?;
        self.block()?;
        self.emit_return_and_log();
        let state = self.end_new_function();
        let up_values = &state.upvalues;
        let closure = Value::Object(Object::Function(Rc::new(state.function)));
        let byte = self.add_constant(closure);
        self.emit_opcode_and_bytes(Opcode::Closure, byte);
        for u in up_values {
            self.emit_byte(if u.is_local { 1 } else { 0 });
            self.emit_byte(u.index);
        }
        Ok(())
    }

    fn function_name(&self, t: FunctionType) -> Result<String> {
        match t {
            FunctionType::Script => Ok("".to_string()),
            FunctionType::Function => self.function_name_from_token(),
            FunctionType::Method => self.function_name_from_token(),
            FunctionType::Initializer => self.function_name_from_token(),
        }
    }

    fn function_name_from_token(&self) -> Result<String> {
        let token = self.previous();
        let name = token
            .literal
            .as_ref()
            .expect("Expect function name literal");
        match name {
            Literal::Identifier(s) => Ok(s.clone()),
            _ => bail!(parse_error(self.previous(), "Expect function name")),
        }
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
        } else if let Some(byte) = self.resolve_upvalue(&token)? {
            get_op = Opcode::GetUpvalue;
            set_op = Opcode::SetUpvalue;
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

    fn resolve_upvalue(&mut self, name: &Token) -> Result<Option<u8>> {
        let state_iterator = self.states.iter_mut().rev();
        Compiler::resolve_upvalue_with_state(&mut self.state, state_iterator, name)
    }

    fn resolve_upvalue_with_state(
        current: &mut State,
        mut state_iter: Rev<IterMut<State>>,
        name: &Token,
    ) -> Result<Option<u8>> {
        if let Some(enclosing) = state_iter.next() {
            if let Some(index) = Compiler::resolve_local_with_state(name, enclosing)? {
                let local = &mut enclosing.scope.locals[index as usize];
                local.is_captured = true;
                Ok(Some(Compiler::add_upvalue(index, current, true)))
            } else if let Some(index) =
                Compiler::resolve_upvalue_with_state(enclosing, state_iter, name)?
            {
                Ok(Some(Compiler::add_upvalue(index, current, false)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn add_upvalue(index: u8, state: &mut State, is_local: bool) -> u8 {
        match &mut state.function {
            Function::UserDefined(u) => {
                if let Some((i, _)) = state
                    .upvalues
                    .iter()
                    .enumerate()
                    .find(|(_, v)| v.is_local == is_local && v.index == index)
                {
                    return i as u8;
                }
                state.upvalues.push(Upvalue::new(index, is_local));
                u.upvalue_count += 1;
                (u.upvalue_count - 1) as u8
            }
        }
    }

    fn resolve_local_with_state(name: &Token, state: &State) -> Result<Option<u8>> {
        let scope = &state.scope;
        let mut i = scope.locals.len() as i32 - 1;
        while i >= 0 {
            let index = i as usize;
            if scope.locals[index].name == name.lexeme {
                if scope.locals[index].depth.is_none() {
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

    fn resolve_local(&self, name: &Token) -> Result<Option<u8>> {
        Compiler::resolve_local_with_state(name, &self.state)
    }

    fn define_variable(&mut self, byte: u8) {
        if self.current_scope_mut().depth == GLOBAL_SCOPE_DEPTH {
            self.emit_opcode_and_bytes(Opcode::DefineGlobal, byte);
        } else {
            self.mark_initialized();
        }
    }

    fn statement(&mut self) -> Result<()> {
        if self.match_and_advance(&[TokenType::Print]) {
            self.print_statement()?;
        } else if self.match_and_advance(&[TokenType::If]) {
            self.if_statement()?;
        } else if self.match_and_advance(&[TokenType::While]) {
            self.while_statement()?;
        } else if self.match_and_advance(&[TokenType::LeftBrace]) {
            self.begin_scope();
            self.block()?;
            self.end_scope();
        } else if self.match_and_advance(&[TokenType::Return]) {
            self.return_statement()?;
        } else {
            self.expression_statement()?;
        }
        Ok(())
    }

    fn return_statement(&mut self) -> Result<()> {
        if self.state.function_type == FunctionType::Script {
            bail!(parse_error(
                self.current(),
                "Can't return from top level code"
            ))
        }
        if self.match_and_advance(&[TokenType::Semicolon]) {
            self.emit_return();
        } else {
            if self.state.function_type == FunctionType::Initializer {
                bail!(parse_error(
                    self.current(),
                    "Can't return a value from an initializer"
                ))
            }
            self.expression()?;
            self.consume_next_token(TokenType::Semicolon, "Expect ';' after return")?;
            self.emit_op_code(Opcode::Return);
        }
        Ok(())
    }

    fn while_statement(&mut self) -> Result<()> {
        let loop_start = self.current_chunk_mut().code.count;
        self.consume_next_token(TokenType::LeftParen, "Expect '(' after while")?;
        self.expression()?;
        self.consume_next_token(TokenType::RightParen, "Expect ')' after condition")?;
        let exit_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_op_code(Opcode::Pop);
        self.statement()?;
        self.emit_loop(loop_start);
        self.patch_jump(exit_jump)?;
        self.emit_op_code(Opcode::Pop);
        Ok(())
    }

    fn if_statement(&mut self) -> Result<()> {
        self.consume_next_token(TokenType::LeftParen, "Expect '(' after if")?;
        self.expression()?;
        self.consume_next_token(TokenType::RightParen, "Expect ')' after condition")?;
        let then_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_op_code(Opcode::Pop);
        self.statement()?;
        let else_jump = self.emit_jump(Opcode::Jump);
        self.patch_jump(then_jump)?;
        self.emit_op_code(Opcode::Pop);
        if self.match_and_advance(&[TokenType::Else]) {
            self.statement()?;
        }
        self.patch_jump(else_jump)?;
        Ok(())
    }

    fn current_scope_mut(&mut self) -> &mut Scope<'a> {
        &mut self.state.scope
    }

    fn current_scope(&self) -> &Scope<'a> {
        &self.state.scope
    }

    fn begin_scope(&mut self) {
        self.current_scope_mut().depth += 1;
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
        self.current_scope_mut().depth -= 1;
        let mut i: i32 = self.current_scope_mut().locals.len() as i32 - 1;
        while i >= 0 {
            if self.current_scope_mut().locals[i as usize]
                .depth
                .expect("Expect depth")
                > self.current_scope_mut().depth
            {
                let local = self
                    .current_scope_mut()
                    .locals
                    .pop()
                    .expect("local expected");
                if local.is_captured {
                    self.emit_op_code(Opcode::CloseUpvalue);
                } else {
                    self.emit_op_code(Opcode::Pop);
                }
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
            let prev_token = self.previous();
            match self.get_rule(prev_token.token_type).infix_function {
                Some(infix_rule) => infix_rule(self, can_assign)?,
                None => bail!(parse_error(prev_token, "Expect expression")),
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
            let value = Value::Object(Object::String(s.to_string()));
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

    fn logical_and(&mut self, _can_assign: bool) -> Result<()> {
        let if_left_is_false = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_op_code(Opcode::Pop);
        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(if_left_is_false)?;
        Ok(())
    }
    fn logical_or(&mut self, _can_assign: bool) -> Result<()> {
        let if_left_is_true = self.emit_jump(Opcode::JumpIfTrue);
        self.emit_op_code(Opcode::Pop);
        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(if_left_is_true)?;
        Ok(())
    }

    fn call(&mut self, _can_assign: bool) -> Result<()> {
        let arg_count = self.argument_list()?;
        self.emit_opcode_and_bytes(Opcode::Call, arg_count);
        Ok(())
    }

    fn dot(&mut self, can_assign: bool) -> Result<()> {
        self.consume_next_token(TokenType::Identifier, "Expect property name after '.'")?;
        let name = self.identifier_constant(self.previous().clone())?;
        if can_assign && self.match_and_advance(&[TokenType::Equal]) {
            self.expression()?;
            self.emit_opcode_and_bytes(Opcode::SetProperty, name);
        } else if self.match_and_advance(&[TokenType::LeftParen]) {
            let arg_count = self.argument_list()?;
            self.emit_opcode_and_bytes(Opcode::Invoke, name);
            self.emit_byte(arg_count);
        } else {
            self.emit_opcode_and_bytes(Opcode::GetProperty, name);
        }
        Ok(())
    }

    fn this(&mut self, _can_assign: bool) -> Result<()> {
        if self.current_class.is_none() {
            bail!(parse_error(
                self.previous(),
                "Can't use 'this' outside a class"
            ));
        }
        self.variable_usage(false)
    }

    fn argument_list(&mut self) -> Result<u8> {
        let mut count = 0;
        while self.current().token_type != TokenType::RightParen {
            self.expression()?;
            count += 1;
            self.match_and_advance(&[TokenType::Comma]);
        }
        self.consume_next_token(TokenType::RightParen, "Expect ')' after arguments")?;
        Ok(count)
    }

    #[inline]
    fn get_rule(&mut self, token_type: TokenType) -> &ParseRule<'a> {
        let index: usize = token_type.into();
        &self.parse_rules[index]
    }

    #[inline]
    fn emit_jump(&mut self, opcode: Opcode) -> usize {
        self.emit_op_code(opcode);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.current_chunk_mut().code.count - 2
    }

    #[inline]
    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_op_code(Opcode::Loop);
        let jump = self.current_chunk_mut().code.count - loop_start + 2;
        let (first, second) = as_two_bytes(jump);
        self.emit_byte(first);
        self.emit_byte(second);
    }

    #[inline]
    fn patch_jump(&mut self, offset: usize) -> Result<()> {
        let jump = self.current_chunk_mut().code.count - offset - 2;
        let (first, second) = as_two_bytes(jump);
        self.current_chunk_mut().code.insert_at(offset, first);
        self.current_chunk_mut().code.insert_at(offset + 1, second);
        Ok(())
    }

    #[inline]
    fn emit_constant(&mut self, value: Value) {
        let offset = self.add_constant(value);
        self.emit_opcode_and_bytes(Opcode::Constant, offset);
    }

    #[inline]
    fn add_constant(&mut self, value: Value) -> u8 {
        self.current_chunk_mut().add_constant(value)
    }

    fn emit_return_and_log(&mut self) {
        self.emit_return();
        let name = &self.state.function.to_string();
        if self.custom_writer.is_some() {
            let mut writer_opt = self.custom_writer.take();
            let writer = writer_opt.as_deref_mut().expect("Writer expected");
            self.current_chunk()
                .disassemble_chunk_with_writer(name, writer);
            self.custom_writer = writer_opt;
        } else if log_enabled!(log::Level::Trace) {
            self.current_chunk()
                .disassemble_chunk_with_writer(name, &mut stdout());
        }
    }

    #[inline]
    fn emit_return(&mut self) {
        if self.state.function_type == FunctionType::Initializer {
            self.emit_opcode_and_bytes(Opcode::GetLocal, 0);
        } else {
            self.emit_op_code(Opcode::Nil);
        }
        self.emit_op_code(Opcode::Return);
    }

    fn parse_variable(&mut self, message: &str) -> Result<u8> {
        self.consume_next_token(TokenType::Identifier, message)?;
        self.declare_local_variable()?;
        if self.current_scope_mut().depth > GLOBAL_SCOPE_DEPTH {
            return Ok(0);
        }
        self.identifier_constant(self.previous().clone())
    }

    fn declare_local_variable(&mut self) -> Result<()> {
        let current_scope_depth = self.current_scope().depth;
        if self.current_scope_mut().depth > GLOBAL_SCOPE_DEPTH {
            let token = self.previous();
            for local in self.current_scope_mut().locals.iter().rev() {
                if let Some(depth) = &local.depth {
                    if *depth < current_scope_depth {
                        break;
                    }
                    if local.name == token.lexeme {
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
        let local = Local::new(&token.lexeme, None);
        self.current_scope_mut().locals.push(local);
    }

    fn mark_initialized(&mut self) {
        if self.current_scope_mut().depth == 0 {
            return;
        }
        let locals_count = self.current_scope_mut().locals.len();
        self.current_scope_mut().locals[locals_count - 1].depth =
            Some(self.current_scope_mut().depth);
    }

    fn identifier_constant(&mut self, mut token: Token) -> Result<u8> {
        let literal = token.literal.take();
        if let Literal::Identifier(s) = literal.expect("Expect string") {
            let name = Value::Object(Object::String(s));
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
        let mut line = 0;
        if self.token_index != 0 {
            line = self.previous().line;
        }
        self.current_chunk_mut().write_chunk(byte, line);
    }

    #[inline]
    fn emit_opcode_and_bytes(&mut self, op_code: Opcode, byte: u8) {
        self.emit_op_code(op_code);
        self.emit_byte(byte);
    }

    #[inline]
    fn current_chunk_mut(&mut self) -> &mut Chunk {
        match &mut self.state.function {
            Function::UserDefined(u) => &mut u.chunk,
        }
    }

    #[inline]
    fn current_chunk(&self) -> &Chunk {
        match &self.state.function {
            Function::UserDefined(u) => &u.chunk,
        }
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
    #[inline]
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
        &self.tokens[self.token_index]
    }

    #[inline]
    fn previous(&self) -> &'a Token {
        &self.tokens[self.token_index - 1]
    }

    #[inline]
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.token_index += 1;
        }
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.current().token_type == TokenType::Eof
    }
}

#[inline]
fn as_two_bytes(jump: usize) -> (u8, u8) {
    let first = ((jump >> 8) & 0xff) as u8;
    let second = (jump & 0xff) as u8;
    (first, second)
}

#[cfg(test)]
mod tests {
    use crate::bytecode_virtual_machine::compiler::FunctionType;
    use crate::bytecode_virtual_machine::objects::Function::UserDefined;
    use crate::{common::lox::utf8_to_string, common::scanner::Scanner, errors::*};

    use super::Compiler;

    #[test]
    fn number() -> Result<()> {
        let source = r#"3.14;"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]                  0 '3.14'
0002    | OpCode[Pop]
0003    | OpCode[Nil]
0004    | OpCode[Return]
"#,
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
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]                  0 '3.14'
0002    | OpCode[Pop]
0003    | OpCode[Nil]
0004    | OpCode[Return]
"#,
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
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]                  0 '3.14'
0002    | OpCode[Negate]
0003    | OpCode[Pop]
0004    | OpCode[Nil]
0005    | OpCode[Return]
"#,
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
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]                  0 '4'
0002    | OpCode[Constant]                  1 '3'
0004    | OpCode[Constant]                  2 '2'
0006    | OpCode[Multiply]
0007    | OpCode[Subtract]
0008    | OpCode[Constant]                  3 '8'
0010    | OpCode[Constant]                  4 '4'
0012    | OpCode[Divide]
0013    | OpCode[Add]
0014    | OpCode[Pop]
0015    | OpCode[Nil]
0016    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );

        let source = r#"!(5 - 4 > 3 * 2 == !nil);"#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]                  0 '5'
0002    | OpCode[Constant]                  1 '4'
0004    | OpCode[Subtract]
0005    | OpCode[Constant]                  2 '3'
0007    | OpCode[Constant]                  3 '2'
0009    | OpCode[Multiply]
0010    | OpCode[Greater]
0011    | OpCode[Nil]
0012    | OpCode[Not]
0013    | OpCode[EqualEqual]
0014    | OpCode[Not]
0015    | OpCode[Pop]
0016    | OpCode[Nil]
0017    | OpCode[Return]
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
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]                  0 'Hello '
0002    | OpCode[Constant]                  1 ' world'
0004    | OpCode[Add]
0005    | OpCode[Pop]
0006    | OpCode[Nil]
0007    | OpCode[Return]
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
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]                  0 '3'
0002    | OpCode[Constant]                  1 '3'
0004    | OpCode[Add]
0005    | OpCode[Print]
0006    | OpCode[Nil]
0007    | OpCode[Return]
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
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0001 OpCode[Constant]                  1 '2'
0002    | OpCode[DefineGlobal]              0 'a'
0004    | OpCode[Nil]
0005    | OpCode[Return]
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
        let function = compiler.compile()?;
        let mut buf = vec![];

        match &function {
            UserDefined(u) => {
                u.chunk.disassemble_chunk_with_writer("test", &mut buf);
            }
        }
        assert_eq!(
            r#"== test ==
0000 0002 OpCode[Constant]                  1 '2'
0002    | OpCode[DefineGlobal]              0 'a'
0004 0004 OpCode[GetGlobal]                 2 'a'
0006    | OpCode[Print]
0007 0005 OpCode[Constant]                  3 '3'
0009 0006 OpCode[GetLocal]                  1
0011    | OpCode[Print]
0012 0007 OpCode[GetLocal]                  1
0014 0008 OpCode[Pop]
0015    | OpCode[Pop]
0016 0009 OpCode[GetGlobal]                 4 'a'
0018    | OpCode[Print]
0019 0010 OpCode[GetGlobal]                 5 'b'
0021    | OpCode[Print]
0022    | OpCode[Nil]
0023    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn if_condition() -> Result<()> {
        let source = r#"
        var a = "";
        var condition = true;
        if (condition) {
            a = "if";
        } else {
            a = "else";
        }
        print a;
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0002 OpCode[Constant]                  1 ''
0002    | OpCode[DefineGlobal]              0 'a'
0004 0003 OpCode[True]
0005    | OpCode[DefineGlobal]              2 'condition'
0007 0004 OpCode[GetGlobal]                 3 'condition'
0009    | OpCode[JumpIfFalse]               9 -> 21
0012    | OpCode[Pop]
0013 0005 OpCode[Constant]                  5 'if'
0015    | OpCode[SetGlobal]                 4 'a'
0017    | OpCode[Pop]
0018 0006 OpCode[Jump]                     18 -> 27
0021    | OpCode[Pop]
0022 0007 OpCode[Constant]                  7 'else'
0024    | OpCode[SetGlobal]                 6 'a'
0026    | OpCode[Pop]
0027 0009 OpCode[GetGlobal]                 8 'a'
0029    | OpCode[Print]
0030    | OpCode[Nil]
0031    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn logical_or_and_and_statements() -> Result<()> {
        let source = r#"
        print 2 or 3;
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0002 OpCode[Constant]                  0 '2'
0002    | OpCode[JumpIfTrue]                2 -> 8
0005    | OpCode[Pop]
0006    | OpCode[Constant]                  1 '3'
0008    | OpCode[Print]
0009    | OpCode[Nil]
0010    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );

        let source = r#"
        print 2 and 3;
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0002 OpCode[Constant]                  0 '2'
0002    | OpCode[JumpIfFalse]               2 -> 8
0005    | OpCode[Pop]
0006    | OpCode[Constant]                  1 '3'
0008    | OpCode[Print]
0009    | OpCode[Nil]
0010    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn while_loop() -> Result<()> {
        let source = r#"
        var a = 1;
        while (a <= 5) {
            print a;
            a = a +1;
        }
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => u.chunk.disassemble_chunk_with_writer("test", &mut buf),
        }
        assert_eq!(
            r#"== test ==
0000 0002 OpCode[Constant]                  1 '1'
0002    | OpCode[DefineGlobal]              0 'a'
0004 0003 OpCode[GetGlobal]                 2 'a'
0006    | OpCode[Constant]                  3 '5'
0008    | OpCode[LessEqual]
0009    | OpCode[JumpIfFalse]               9 -> 27
0012    | OpCode[Pop]
0013 0004 OpCode[GetGlobal]                 4 'a'
0015    | OpCode[Print]
0016 0005 OpCode[GetGlobal]                 6 'a'
0018    | OpCode[Constant]                  7 '1'
0020    | OpCode[Add]
0021    | OpCode[SetGlobal]                 5 'a'
0023    | OpCode[Pop]
0024 0006 OpCode[Loop]                     24 -> 4
0027    | OpCode[Pop]
0028    | OpCode[Nil]
0029    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn functions() -> Result<()> {
        let source = r#"
        var const = "You answered";
        fun areWeHavingItYet(answer) {
            print  const + " " + answer;
        }
          
        areWeHavingItYet("yes!");
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => {
                u.chunk.disassemble_chunk_with_writer("test", &mut buf);
            }
        }
        assert_eq!(
            r#"== test ==
0000 0002 OpCode[Constant]                  1 'You answered'
0002    | OpCode[DefineGlobal]              0 'const'
0004 0005 OpCode[Closure]                   3 '<fn areWeHavingItYet>'
0006    | OpCode[DefineGlobal]              2 'areWeHavingItYet'
0008 0007 OpCode[GetGlobal]                 4 'areWeHavingItYet'
0010    | OpCode[Constant]                  5 'yes!'
0012    | OpCode[Call]                      1
0014    | OpCode[Pop]
0015    | OpCode[Nil]
0016    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );

        let source = r#"
        var const = "You answered";
        fun areWeHavingItYet(answer) {
            return  const + " " + answer;
        }
          
        print areWeHavingItYet("yes!");
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let compiler = Compiler::new(tokens);
        let function = compiler.compile()?;
        let mut buf = vec![];
        match &function {
            UserDefined(u) => {
                u.chunk.disassemble_chunk_with_writer("test", &mut buf);
            }
        }
        assert_eq!(
            r#"== test ==
0000 0002 OpCode[Constant]                  1 'You answered'
0002    | OpCode[DefineGlobal]              0 'const'
0004 0005 OpCode[Closure]                   3 '<fn areWeHavingItYet>'
0006    | OpCode[DefineGlobal]              2 'areWeHavingItYet'
0008 0007 OpCode[GetGlobal]                 4 'areWeHavingItYet'
0010    | OpCode[Constant]                  5 'yes!'
0012    | OpCode[Call]                      1
0014    | OpCode[Print]
0015    | OpCode[Nil]
0016    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn closure() -> Result<()> {
        let source = r#"fun outer() {
            var a = 1;
            var b = 2;
            fun middle() {
              var c = 3;
              var d = 4;
              fun inner() {
                print a + c + b + d;
              }
            }
          }
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let mut buf = vec![];
        let compiler =
            Compiler::new_with_type_and_writer(tokens, FunctionType::Script, Some(&mut buf));
        let _ = compiler.compile()?;
        assert_eq!(
            r#"== <fn inner> ==
0000 0008 OpCode[GetUpvalue]                0
0002    | OpCode[GetUpvalue]                1
0004    | OpCode[Add]
0005    | OpCode[GetUpvalue]                2
0007    | OpCode[Add]
0008    | OpCode[GetUpvalue]                3
0010    | OpCode[Add]
0011    | OpCode[Print]
0012 0009 OpCode[Nil]
0013    | OpCode[Return]
== <fn middle> ==
0000 0005 OpCode[Constant]                  0 '3'
0002 0006 OpCode[Constant]                  1 '4'
0004 0009 OpCode[Closure]                   2 '<fn inner>'
0006    |                                      upvalue 0
0008    |                                      local 1
0010    |                                      upvalue 1
0012    |                                      local 2
0014 0010 OpCode[Nil]
0015    | OpCode[Return]
== <fn outer> ==
0000 0002 OpCode[Constant]                  0 '1'
0002 0003 OpCode[Constant]                  1 '2'
0004 0010 OpCode[Closure]                   2 '<fn middle>'
0006    |                                      local 1
0008    |                                      local 2
0010 0011 OpCode[Nil]
0011    | OpCode[Return]
== <fn script> ==
0000 0011 OpCode[Closure]                   1 '<fn outer>'
0002    | OpCode[DefineGlobal]              0 'outer'
0004    | OpCode[Nil]
0005    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn classes_fields() -> Result<()> {
        let source = r#"
            class Pair {}
            var pair = Pair();
            pair.first = 1;
            pair.second = 2;
            print pair.first + pair.second; // 3.
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let mut buf = vec![];
        let compiler =
            Compiler::new_with_type_and_writer(tokens, FunctionType::Script, Some(&mut buf));
        let _ = compiler.compile()?;
        assert_eq!(
            r#"== <fn script> ==
0000 0002 OpCode[Class]                     0 'Pair'
0002    | OpCode[DefineGlobal]              0 'Pair'
0004    | OpCode[GetGlobal]                 1 'Pair'
0006    | OpCode[Pop]
0007 0003 OpCode[GetGlobal]                 3 'Pair'
0009    | OpCode[Call]                      0
0011    | OpCode[DefineGlobal]              2 'pair'
0013 0004 OpCode[GetGlobal]                 4 'pair'
0015    | OpCode[Constant]                  6 '1'
0017    | OpCode[SetProperty]               5 'first'
0019    | OpCode[Pop]
0020 0005 OpCode[GetGlobal]                 7 'pair'
0022    | OpCode[Constant]                  9 '2'
0024    | OpCode[SetProperty]               8 'second'
0026    | OpCode[Pop]
0027 0006 OpCode[GetGlobal]                10 'pair'
0029    | OpCode[GetProperty]              11 'first'
0031    | OpCode[GetGlobal]                12 'pair'
0033    | OpCode[GetProperty]              13 'second'
0035    | OpCode[Add]
0036    | OpCode[Print]
0037    | OpCode[Nil]
0038    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn classes_methods() -> Result<()> {
        let source = r#"
        class Scone {
            topping(first, second) {
              print "scone with " + first + " and " + second;
            }
          }
          
          var scone = Scone();
          scone.topping("berries", "cream");
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let mut buf = vec![];
        let compiler =
            Compiler::new_with_type_and_writer(tokens, FunctionType::Script, Some(&mut buf));
        let _ = compiler.compile()?;
        assert_eq!(
            r#"== <fn topping> ==
0000 0004 OpCode[Constant]                  0 'scone with '
0002    | OpCode[GetLocal]                  1
0004    | OpCode[Add]
0005    | OpCode[Constant]                  1 ' and '
0007    | OpCode[Add]
0008    | OpCode[GetLocal]                  2
0010    | OpCode[Add]
0011    | OpCode[Print]
0012 0005 OpCode[Nil]
0013    | OpCode[Return]
== <fn script> ==
0000 0002 OpCode[Class]                     0 'Scone'
0002    | OpCode[DefineGlobal]              0 'Scone'
0004    | OpCode[GetGlobal]                 1 'Scone'
0006 0005 OpCode[Closure]                   2 '<fn topping>'
0008    | OpCode[Method]                    3 'topping'
0010 0006 OpCode[Pop]
0011 0008 OpCode[GetGlobal]                 5 'Scone'
0013    | OpCode[Call]                      0
0015    | OpCode[DefineGlobal]              4 'scone'
0017 0009 OpCode[GetGlobal]                 6 'scone'
0019    | OpCode[Constant]                  8 'berries'
0021    | OpCode[Constant]                  9 'cream'
0023    | OpCode[Invoke]                   (2 args)   7 'topping'
0026    | OpCode[Pop]
0027    | OpCode[Nil]
0028    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }

    #[test]
    fn class_initializer_with_this() -> Result<()> {
        let source = r#"
        class Scone {
            topping(first, second) {
              print "scone with " + first + " and " + second;
            }
          }
          
          var scone = Scone();
          scone.topping("berries", "cream");
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let mut buf = vec![];
        let compiler =
            Compiler::new_with_type_and_writer(tokens, FunctionType::Script, Some(&mut buf));
        let _ = compiler.compile()?;
        assert_eq!(
            r#"== <fn topping> ==
0000 0004 OpCode[Constant]                  0 'scone with '
0002    | OpCode[GetLocal]                  1
0004    | OpCode[Add]
0005    | OpCode[Constant]                  1 ' and '
0007    | OpCode[Add]
0008    | OpCode[GetLocal]                  2
0010    | OpCode[Add]
0011    | OpCode[Print]
0012 0005 OpCode[Nil]
0013    | OpCode[Return]
== <fn script> ==
0000 0002 OpCode[Class]                     0 'Scone'
0002    | OpCode[DefineGlobal]              0 'Scone'
0004    | OpCode[GetGlobal]                 1 'Scone'
0006 0005 OpCode[Closure]                   2 '<fn topping>'
0008    | OpCode[Method]                    3 'topping'
0010 0006 OpCode[Pop]
0011 0008 OpCode[GetGlobal]                 5 'Scone'
0013    | OpCode[Call]                      0
0015    | OpCode[DefineGlobal]              4 'scone'
0017 0009 OpCode[GetGlobal]                 6 'scone'
0019    | OpCode[Constant]                  8 'berries'
0021    | OpCode[Constant]                  9 'cream'
0023    | OpCode[Invoke]                   (2 args)   7 'topping'
0026    | OpCode[Pop]
0027    | OpCode[Nil]
0028    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        let source = r#"
        class Brunch {
            init(food, drinks) {
                this.food = food;
                this.drinks = drinks;
            }

            set_dessert(item) {
                this.dessert = item;
                return this;
            }
        }
                  
        var brunch = Brunch("eggs", "coffee");

        var brunch_with_dessert = brunch.set_dessert("cake");
        
        print brunch_with_dessert.food + " and " + brunch_with_dessert.drinks + " with " + brunch_with_dessert.dessert + " as dessert";
        "#;
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens()?;
        let mut buf = vec![];
        let compiler =
            Compiler::new_with_type_and_writer(tokens, FunctionType::Script, Some(&mut buf));
        let _ = compiler.compile()?;
        assert_eq!(
            r#"== <fn init> ==
0000 0004 OpCode[GetLocal]                  0
0002    | OpCode[GetLocal]                  1
0004    | OpCode[SetProperty]               0 'food'
0006    | OpCode[Pop]
0007 0005 OpCode[GetLocal]                  0
0009    | OpCode[GetLocal]                  2
0011    | OpCode[SetProperty]               1 'drinks'
0013    | OpCode[Pop]
0014 0006 OpCode[GetLocal]                  0
0016    | OpCode[Return]
== <fn set_dessert> ==
0000 0009 OpCode[GetLocal]                  0
0002    | OpCode[GetLocal]                  1
0004    | OpCode[SetProperty]               0 'dessert'
0006    | OpCode[Pop]
0007 0010 OpCode[GetLocal]                  0
0009    | OpCode[Return]
0010 0011 OpCode[Nil]
0011    | OpCode[Return]
== <fn script> ==
0000 0002 OpCode[Class]                     0 'Brunch'
0002    | OpCode[DefineGlobal]              0 'Brunch'
0004    | OpCode[GetGlobal]                 1 'Brunch'
0006 0006 OpCode[Closure]                   2 '<fn init>'
0008    | OpCode[Method]                    3 'init'
0010 0011 OpCode[Closure]                   4 '<fn set_dessert>'
0012    | OpCode[Method]                    5 'set_dessert'
0014 0012 OpCode[Pop]
0015 0014 OpCode[GetGlobal]                 7 'Brunch'
0017    | OpCode[Constant]                  8 'eggs'
0019    | OpCode[Constant]                  9 'coffee'
0021    | OpCode[Call]                      2
0023    | OpCode[DefineGlobal]              6 'brunch'
0025 0016 OpCode[GetGlobal]                11 'brunch'
0027    | OpCode[Constant]                 13 'cake'
0029    | OpCode[Invoke]                   (1 args)  12 'set_dessert'
0032    | OpCode[DefineGlobal]             10 'brunch_with_dessert'
0034 0018 OpCode[GetGlobal]                14 'brunch_with_dessert'
0036    | OpCode[GetProperty]              15 'food'
0038    | OpCode[Constant]                 16 ' and '
0040    | OpCode[Add]
0041    | OpCode[GetGlobal]                17 'brunch_with_dessert'
0043    | OpCode[GetProperty]              18 'drinks'
0045    | OpCode[Add]
0046    | OpCode[Constant]                 19 ' with '
0048    | OpCode[Add]
0049    | OpCode[GetGlobal]                20 'brunch_with_dessert'
0051    | OpCode[GetProperty]              21 'dessert'
0053    | OpCode[Add]
0054    | OpCode[Constant]                 22 ' as dessert'
0056    | OpCode[Add]
0057    | OpCode[Print]
0058    | OpCode[Nil]
0059    | OpCode[Return]
"#,
            utf8_to_string(&buf)
        );
        Ok(())
    }
}

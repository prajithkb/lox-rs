use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;

use crate::errors::*;
use crate::parser::{Expr, Stmt};
use crate::tokens::Token;

#[derive(Default)]
pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    variable_depths: HashMap<String, usize>,
    current_function: FunctionType,
    current_class: ClassType,
}
#[derive(Debug, Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
}

impl Default for FunctionType {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ClassType {
    None,
    Class,
}

impl Default for ClassType {
    fn default() -> Self {
        Self::None
    }
}
fn resolution_error(token: Option<&Token>, message: &str) -> ErrorKind {
    match token {
        Some(t) => ErrorKind::ResolutionError(format!(
            "[line: {}] Error at <{}>: message: {}",
            t.line, t.lexeme, message
        )),
        None => ErrorKind::ResolutionError(format!("message: {}", message)),
    }
}

impl Resolver {
    pub fn new() -> Self {
        Resolver::default()
    }

    pub fn resolve(&mut self, statements: &[Stmt]) -> Result<HashMap<String, usize>> {
        for stmt in statements {
            self.resolve_statement(stmt)?;
        }
        Ok(self.variable_depths.clone())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn peek(&mut self) -> Option<&mut HashMap<String, bool>> {
        if !self.scopes.is_empty() {
            let len = self.scopes.len();
            Some(&mut self.scopes[len - 1])
        } else {
            None
        }
    }

    fn resolve_statement(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(e) => self.resolve_expression(e),
            Stmt::Print(e) => self.resolve_expression(e),
            Stmt::If(condition, if_branch, else_branch) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(if_branch)?;
                if let Some(e) = else_branch {
                    self.resolve_statement(e)?;
                }
                Ok(())
            }
            Stmt::While(condition, body) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;
                Ok(())
            }
            Stmt::Return(t, expr) => {
                match self.current_function {
                    FunctionType::None => bail!(resolution_error(
                        Some(t),
                        "Return statements can only be present in functions"
                    )),
                    FunctionType::Initializer => bail!(resolution_error(
                        Some(t),
                        "Return statements cannot be present in init (constructors)"
                    )),
                    _ => {
                        if let Some(e) = expr {
                            self.resolve_expression(e)?;
                        }
                    }
                }
                if let Some(e) = expr {
                    self.resolve_expression(e)?;
                }
                Ok(())
            }
            // The ones to be impl
            Stmt::Function(name, params, body) => {
                self.resolve_function_statement(name, params, body)
            }
            Stmt::Var(name, initializer) => self.resolve_var_declaration(name, initializer),
            Stmt::Block(stmts) => self.block(stmts),
            Stmt::Class(name, methods) => self.class(name, methods),
        }
    }

    fn class(&mut self, name: &Token, methods: &[Stmt]) -> Result<()> {
        let enclosing_class = self.current_class;
        self.current_class = ClassType::Class;
        self.define(name);
        self.begin_scope();
        self.peek().map(|s| s.insert("this".to_string(), true));
        for method in methods {
            if let Stmt::Function(name, params, body) = method {
                let mut function_type = FunctionType::Method;
                if name.lexeme == "init" {
                    function_type = FunctionType::Initializer;
                }
                self.declare(name)?;
                self.define(name);
                self.resolve_function_params_body(params, body, function_type)?;
            } else {
                bail!(resolution_error(
                    Some(name),
                    "Class declaration can have only methods"
                ));
            }
        }
        self.declare(name)?;
        self.end_scope();
        self.current_class = enclosing_class;
        Ok(())
    }

    fn resolve_expression(&mut self, e: &Expr) -> Result<()> {
        match e {
            Expr::Binary(left, _, right) => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
                Ok(())
            }
            Expr::Grouping(e) => self.resolve_expression(e),
            Expr::Literal(_) => Ok(()),
            Expr::Unary(_, e) => self.resolve_expression(e),

            Expr::Logical(left, _, right) => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
                Ok(())
            }
            Expr::Call(name, _, arguments) => {
                self.resolve_expression(name)?;
                for a in arguments {
                    self.resolve_expression(a)?;
                }
                Ok(())
            }
            // The ones to be impl
            Expr::Var(t) => self.resolve_var_expression(t, e),
            Expr::Assign(name, e) => self.resolve_var_assign(name, e),
            Expr::Get(object_expr, _) => self.resolve_expression(object_expr),
            Expr::Set(object_expr, _, value) => self.set_expression(object_expr, value),
            Expr::This(keyword) => {
                if self.current_class == ClassType::None {
                    bail!(resolution_error(
                        Some(keyword),
                        "Can't use 'this' outside of a class"
                    ))
                }
                self.resolve_local(keyword, e);
                Ok(())
            }
        }
    }

    fn set_expression(&mut self, object_expr: &Expr, value: &Expr) -> Result<()> {
        self.resolve_expression(object_expr)?;
        self.resolve_expression(value)?;
        Ok(())
    }
    fn block(&mut self, stmts: &[Stmt]) -> Result<()> {
        self.begin_scope();
        for stmt in stmts {
            self.resolve_statement(stmt)?;
        }
        self.end_scope();
        Ok(())
    }

    fn resolve_function_statement(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<()> {
        self.declare(name)?;
        self.define(name);
        self.resolve_function_params_body(params, body, FunctionType::Function)
    }

    fn resolve_function_params_body(
        &mut self,
        params: &[Token],
        body: &[Stmt],
        function_type: FunctionType,
    ) -> Result<()> {
        let enclosing_type = self.current_function;
        self.current_function = function_type;
        self.begin_scope();
        for t in params {
            self.declare(t)?;
            self.define(t);
        }
        self.resolve(body)?;
        self.end_scope();
        self.current_function = enclosing_type;
        Ok(())
    }

    fn resolve_var_declaration(&mut self, name: &Token, initializer: &Option<Expr>) -> Result<()> {
        self.declare(name)?;
        if let Some(init) = initializer {
            self.resolve_expression(init)?;
        }
        self.define(name);
        Ok(())
    }

    fn resolve_var_expression(&mut self, name: &Token, e: &Expr) -> Result<()> {
        if let Some(map) = self.peek() {
            if let Some(bool) = map.get(&name.lexeme) {
                // prevent edge case var a = a;
                if !bool {
                    bail!(resolution_error(
                        Some(name),
                        "Cannot read local variable in its own initializer"
                    ))
                }
            }
        }
        self.resolve_local(name, e);
        Ok(())
    }

    fn resolve_var_assign(&mut self, name: &Token, expr: &Expr) -> Result<()> {
        self.resolve_expression(expr)?;
        self.resolve_local(name, expr);
        Ok(())
    }

    fn resolve_local(&mut self, name: &Token, e: &Expr) {
        let size = self.scopes.len();
        for i in (0..size).rev() {
            if self.scopes[i].contains_key(&name.lexeme) {
                self.variable_depths.insert(e.to_string(), size - i - 1);
            }
        }
    }

    fn declare(&mut self, name: &Token) -> Result<()> {
        if let Some(map) = self.peek() {
            match map.entry(name.lexeme.clone()) {
                Occupied(_) => bail!(resolution_error(
                    Some(name),
                    "Already a variable with this name in this scope"
                )),
                Vacant(e) => {
                    e.insert(false);
                }
            }
        }
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(map) = self.peek() {
            map.insert(name.lexeme.clone(), true);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{errors::*, parser::Parser, scanner::Scanner};

    use super::Resolver;

    #[test]
    fn test_resolver() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // function
            var a = "global";
            {
                fun showA() {
                    print a;
                }
                showA();
                var a = "block";
                var b ="something";
                showA();
                print a;
                print b;
            }
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        let v = resolver.resolve(&statements)?;
        let expected = HashMap::from([
            ("(Var showA)".to_string(), 0),
            ("(Var a)".to_string(), 0),
            ("(Var b)".to_string(), 0),
        ]);
        assert_eq!(expected, v);
        Ok(())
    }

    #[test]
    fn resolver_fails() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // function
            var a = "global";
            {
                var a = "block";
                print a;
                var a = "something else";
            }
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        match resolver.resolve(&statements) {
            Ok(_) => panic!("Expected to fail"),
            Err(e) => assert_eq!("Resolution Error: [line: 7] Error at <a>: message: Already a variable with this name in this scope", e.to_string()),
        };

        let mut scanner = Scanner::new(
            r#"
            // function
            var a = "global";
            {
                fun hello() {
                    return "hi";
                }
                print hello();
                return "hi";
            }
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        match resolver.resolve(&statements) {
            Ok(_) => panic!("Expected to fail"),
            Err(e) => assert_eq!("Resolution Error: [line: 9] Error at <return>: message: Return statements can only be present in functions", e.to_string()),
        };
        Ok(())
    }
}

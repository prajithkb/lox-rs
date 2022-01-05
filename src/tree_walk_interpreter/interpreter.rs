use std::cell::{RefCell, RefMut};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::io::Write;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::common::lox::Shared;
use crate::common::tokens::{Literal, Token, TokenType};
use crate::errors::*;

use super::parser::{Expr, Stmt};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Function(Rc<LoxFunction>),
    Instance(Rc<LoxFunction>, Shared<HashMap<String, Value>>),
    // Special value for returning, to unwind the stack
    __Return(Box<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => f.write_str("nil"),
            Value::Boolean(b) => f.write_str(&b.to_string()),
            Value::Number(n) => f.write_str(&n.to_string()),
            Value::String(s) => f.write_str(s),
            Value::Function(callable) => f.write_fmt(format_args!("fn {:?}", callable)),
            Value::Instance(class, _) => f.write_fmt(format_args!("Class {:?}", class)),
            Value::__Return(v) => f.write_fmt(format_args!("Return {:?}", v)),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum LoxFunction {
    Native(
        String,
        Vec<String>,
        Shared<NativeFunction>,
        Shared<Environment>,
    ),
    UserDefined(String, Vec<String>, Rc<Vec<Stmt>>, Shared<Environment>),
    Class(
        String,
        Vec<String>,
        HashMap<String, Rc<LoxFunction>>,
        Shared<Environment>,
    ),
}

impl Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native(arg0, _, _, _) => f.debug_tuple("Native").field(arg0).finish(),
            Self::UserDefined(arg0, _, _, _) => f.debug_tuple("UserDefined").field(arg0).finish(),
            Self::Class(arg0, _, _, _) => f.debug_tuple("Class").field(arg0).finish(),
        }
    }
}

type InnerNativeFunction = Box<dyn FnMut(Vec<Value>, Shared<Environment>) -> Value>;

pub struct NativeFunction {
    name: String,
    inner: InnerNativeFunction,
}

impl NativeFunction {
    fn new(name: String, inner: InnerNativeFunction) -> Self {
        NativeFunction { name, inner }
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction")
            .field("name", &self.name)
            .finish()
    }
}

#[derive(Debug, PartialEq)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Shared<Environment>>,
}

impl Environment {
    pub fn new(enclosing: Option<Shared<Environment>>) -> Self {
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
                if let Some(e) = self.enclosing.as_mut() {
                    let v = &**e;
                    v.borrow_mut().assign(name, value)
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
                Some(e) => e.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn get_at(&mut self, name: &str, hops: usize) -> Option<Value> {
        if hops == 0 {
            self.get(name)
        } else {
            self.get_at(name, hops - 1)
        }
    }

    pub fn assign_at(&mut self, name: &str, value: Value, hops: usize) -> Option<Value> {
        if hops == 0 {
            self.assign(name, value)
        } else {
            self.assign_at(name, value, hops - 1)
        }
    }
}

pub fn define_native_fn(
    name: String,
    interpreter: &mut Interpreter,
    native_fn: InnerNativeFunction,
) {
    let v = interpreter.globals.clone();
    interpreter.environment().define(
        name.clone(),
        Value::Function(Rc::new(LoxFunction::Native(
            name.clone(),
            vec![],
            Rc::new(RefCell::new(NativeFunction::new(name, native_fn))),
            v,
        ))),
    );
}

pub struct Interpreter<'a> {
    #[allow(unused)]
    globals: Shared<Environment>,
    environment: Shared<Environment>,
    locals: HashMap<String, usize>,
    output_writer: Option<&'a mut dyn Write>,
}

impl<'a> Interpreter<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Interpreter::new_with_writer(None)
    }

    pub fn clock() -> InnerNativeFunction {
        Box::new(|_, _| {
            let start = SystemTime::now();
            let since_the_epoch = start
                .duration_since(UNIX_EPOCH)
                .expect("Time went backwards")
                .as_secs_f64();
            Value::Number(since_the_epoch)
        })
    }

    fn new_with_writer(output_writer: Option<&'a mut dyn Write>) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));
        Interpreter {
            globals: globals.clone(),
            environment: globals,
            output_writer,
            locals: HashMap::new(),
        }
    }

    fn environment(&mut self) -> RefMut<Environment> {
        (*self.environment).borrow_mut()
    }

    pub fn interpret(
        &mut self,
        statements: &[Stmt],
        resolved_variables: HashMap<String, usize>,
    ) -> Result<()> {
        self.locals = resolved_variables;
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&mut self, statement: &Stmt) -> Result<Value> {
        match statement {
            Stmt::Expression(e) => self.evaluate(e).map(|_| Ok(Value::Nil))?,
            Stmt::Print(e) => self.print_statement(e).map(|_| Ok(Value::Nil))?,
            Stmt::Var(token, expr) => self.var_statement(token, expr).map(|_| Ok(Value::Nil))?,
            Stmt::Block(statements) => self.block(statements),
            Stmt::If(condition, if_branch, else_branch) => {
                self.if_statement(condition, if_branch, else_branch)
            }
            Stmt::While(condition, body) => self
                .while_statement(condition, body)
                .map(|_| Ok(Value::Nil))?,
            Stmt::Function(name, parameters, body) => self
                .function_declaration(name, parameters, body.clone())
                .map(|_| Ok(Value::Nil))?,
            Stmt::Return(_, expr) => self.return_statement(expr),
            Stmt::Class(name, methods) => self.class_declaration(name, methods.clone()),
        }
    }

    fn class_declaration(&mut self, name: &Token, methods: Rc<Vec<Stmt>>) -> Result<Value> {
        self.environment().define(name.lexeme.clone(), Value::Nil);
        let closure = Rc::new(RefCell::new(Environment::new(Some(
            self.environment.clone(),
        ))));
        let mut functions = HashMap::new();
        let mut constructor_params = vec![];
        for method in methods.iter() {
            if let Stmt::Function(name, params, body) = method {
                let parameter_names: Vec<String> =
                    params.iter().map(|p| p.lexeme.clone()).collect();
                if name.lexeme == "init" {
                    constructor_params = parameter_names.clone();
                }
                let function = LoxFunction::UserDefined(
                    name.lexeme.clone(),
                    parameter_names,
                    body.clone(),
                    closure.clone(),
                );
                functions.insert(name.lexeme.clone(), Rc::new(function));
            } else {
                bail!(runtime_error_with_token("Invalid expression/statement in class declaration. Should have been caught in resolver!".into(), name))
            }
        }
        let class = LoxFunction::Class(name.lexeme.clone(), constructor_params, functions, closure);
        self.environment()
            .assign(&name.lexeme, Value::Function(Rc::new(class)));
        Ok(Value::Nil)
    }

    fn return_statement(&mut self, expr: &Option<Expr>) -> Result<Value> {
        match expr {
            Some(expr) => Ok(Value::__Return(Box::new(self.evaluate(expr)?))),
            None => Ok(Value::Nil),
        }
    }

    fn function_declaration(
        &mut self,
        name: &Token,
        parameters: &[Token],
        body: Rc<Vec<Stmt>>,
    ) -> Result<()> {
        let function_name = name.lexeme.clone();
        let closure = Rc::new(RefCell::new(Environment::new(Some(
            self.environment.clone(),
        ))));
        let parameter_names: Vec<String> = parameters.iter().map(|p| p.lexeme.clone()).collect();
        self.environment().define(
            function_name,
            Value::Function(Rc::new(LoxFunction::UserDefined(
                name.lexeme.clone(),
                parameter_names,
                body,
                closure,
            ))),
        );
        Ok(())
    }

    fn print_statement(&mut self, expr: &Expr) -> Result<()> {
        let v = self.evaluate(expr)?;
        if let Some(writer) = self.output_writer.as_deref_mut() {
            writeln!(writer, "{}", v)?;
        }
        println!("{}", v);
        Ok(())
    }

    fn var_statement(&mut self, token: &Token, expr: &Option<Expr>) -> Result<()> {
        let name = token.lexeme.clone();
        let value = match expr {
            Some(e) => self.evaluate(e)?,
            None => Value::Nil,
        };
        self.environment().define(name, value);
        Ok(())
    }

    fn block(&mut self, statements: &[Stmt]) -> Result<Value> {
        self.block_with_env(
            statements,
            Rc::new(RefCell::new(Environment::new(Some(
                self.environment.clone(),
            )))),
        )
    }

    fn block_with_env(
        &mut self,
        statements: &[Stmt],
        environment: Shared<Environment>,
    ) -> Result<Value> {
        let previous = self.environment.clone();
        self.environment = environment;
        let mut value = Value::Nil;
        for statement in statements {
            let result = self.execute(statement);
            match result {
                Ok(output) => {
                    if let Value::__Return(v) = output {
                        value = Value::__Return(v);
                        break;
                    }
                }
                Err(e) => {
                    self.environment = previous;
                    return Err(e);
                }
            }
        }
        self.environment = previous;
        Ok(value)
    }

    fn if_statement(
        &mut self,
        condition: &Expr,
        if_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<Value> {
        let evaluated_condition = self.evaluate(condition);
        if let Value::Boolean(is_true) = evaluated_condition? {
            if is_true {
                return self.execute(if_branch);
            } else if let Some(else_branch) = else_branch {
                return self.execute(else_branch);
            } else {
                return Ok(Value::Nil);
            }
        }
        bail!(runtime_error(format!(
            "Condition {} did not evaluate to a boolean",
            condition.to_string()
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
            Expr::Var(name) => self.evaluate_var(name, expr),
            Expr::Assign(name, expr) => self.evaluate_assignment(name, expr),
            Expr::Logical(left, operator, right) => self.evaluate_logical(left, operator, right),
            Expr::Call(expr, call_paren, arguments) => self.call(expr, call_paren, arguments),
            Expr::Get(object, property) => self.evaluate_get(object, property),
            Expr::Set(object, property, value) => self.evaluate_set(object, property, value),
            Expr::This(keyword) => self.evaluate_this(keyword, expr),
        }
    }

    fn evaluate_this(&mut self, keyword: &Token, expr: &Expr) -> Result<Value> {
        self.evaluate_var(keyword, expr)
    }

    fn evaluate_set(
        &mut self,
        object_expr: &Expr,
        property: &Token,
        value_expr: &Expr,
    ) -> Result<Value> {
        let object = self.evaluate(object_expr)?;
        if let Value::Instance(_, properties) = object {
            let v = self.evaluate(value_expr)?;
            (&*properties)
                .borrow_mut()
                .insert(property.lexeme.clone(), v);
            Ok(Value::Nil)
        } else {
            bail!(runtime_error_with_token(
                "Cannot call '.' operator non class instances".to_string(),
                property
            ))
        }
    }

    fn evaluate_get(&mut self, expr: &Expr, property: &Token) -> Result<Value> {
        let object = self.evaluate(expr)?;
        if let Value::Instance(class, properties) = object.clone() {
            if let Some(v) = (&*properties).borrow_mut().get(&property.lexeme) {
                Ok(v.clone())
            } else {
                let method = match &*class {
                    LoxFunction::Class(_, _params, methods, _) => methods.get(&property.lexeme),
                    _ => None,
                };
                if let Some(m) = method {
                    Ok(Value::Function(Rc::new(
                        self.bind_method(m, object, property, &class)?,
                    )))
                } else {
                    bail!(runtime_error_with_token(
                        format!(
                            "Undefined property {} for class {:?}",
                            property.lexeme, class
                        ),
                        property
                    ))
                }
            }
        } else {
            bail!(runtime_error_with_token(
                "Cannot call '.' operator non class instances".to_string(),
                property
            ))
        }
    }

    fn bind_method(
        &mut self,
        method: &Rc<LoxFunction>,
        object_to_bind_to: Value,
        token: &Token,
        class: &LoxFunction,
    ) -> Result<LoxFunction> {
        match &**method {
            LoxFunction::UserDefined(name, arguments, body, closure) => {
                let environment = Rc::new(RefCell::new(Environment::new(Some(closure.clone()))));
                environment
                    .borrow_mut()
                    .define("this".to_string(), object_to_bind_to);
                Ok(LoxFunction::UserDefined(
                    name.clone(),
                    arguments.clone(),
                    body.clone(),
                    environment,
                ))
            }
            _ => bail!(runtime_error_with_token(
                format!(
                    "Invalid function type {} for class {:?}",
                    token.lexeme, class
                ),
                token
            )),
        }
    }

    fn evaluate_assignment(&mut self, name: &Token, expr: &Expr) -> Result<Value> {
        let value = self.evaluate(expr)?;
        let distance = self.lookup_variable(expr);
        let result = match distance {
            Some(hops) => self.environment().assign_at(&name.lexeme, value, hops),
            None => self.globals.borrow_mut().assign(&name.lexeme, value),
        };
        match result {
            Some(value) => Ok(value),
            None => bail!(runtime_error_with_token(
                format!("var {} is not defined", &name.lexeme),
                name
            )),
        }
    }

    fn evaluate_var(&mut self, name: &Token, expr: &Expr) -> Result<Value> {
        let distance = self.lookup_variable(expr);
        let result = match distance {
            Some(hops) => self.environment().get_at(&name.lexeme, hops),
            None => {
                let globals = self.globals.borrow();
                globals.get(&name.lexeme)
            }
        };
        match result {
            Some(v) => Ok(v),
            None => bail!(runtime_error_with_token(
                format!("var {} is not defined", &name.lexeme),
                name
            )),
        }
    }

    fn lookup_variable(&self, expr: &Expr) -> Option<usize> {
        // Some(0)
        // This is expensive! Need to find an alternative
        self.locals.get(&expr.to_string()).copied()
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
        if operator.token_type == TokenType::Or {
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

    fn call(&mut self, expr: &Expr, left_paren: &Token, arguments: &[Expr]) -> Result<Value> {
        if let Value::Function(lox_function) = self.evaluate(expr)? {
            match &*lox_function {
                LoxFunction::UserDefined(name, parameters, body, closure) => self.user_defined_fn(
                    name,
                    parameters,
                    body,
                    closure.clone(),
                    arguments,
                    left_paren,
                ),
                LoxFunction::Native(name, parameters, native_function, closure) => self.native_fn(
                    name,
                    parameters,
                    closure.clone(),
                    arguments,
                    native_function,
                    left_paren,
                ),
                LoxFunction::Class(name, parameters, methods, _closure) => self.instance_creation(
                    name,
                    parameters,
                    methods,
                    arguments,
                    left_paren,
                    lox_function.clone(),
                ),
            }
        } else {
            bail!(runtime_error_with_token(
                "Can only call functions and classes.".into(),
                left_paren
            ))
        }
    }

    fn user_defined_fn(
        &mut self,
        name: &str,
        parameters: &[String],
        body: &[Stmt],
        closure: Shared<Environment>,
        arguments: &[Expr],
        left_paren: &Token,
    ) -> Result<Value> {
        if parameters.len() != arguments.len() {
            bail!(runtime_error_with_token(
                format!(
                    "function {} expects {} arguments but got {}",
                    name,
                    parameters.len(),
                    arguments.len()
                ),
                left_paren
            ))
        }
        let mut evaluated_arguments = vec![];
        for argument in arguments {
            let evaluated_argument = self.evaluate(argument)?;
            evaluated_arguments.push(evaluated_argument);
        }
        let mut env = Environment::new(Some(closure));
        for (i, arg) in evaluated_arguments.into_iter().enumerate() {
            env.define(parameters[i].clone(), arg);
        }
        let function_env = Rc::new(RefCell::new(env));
        if let Value::__Return(v) = self.block_with_env(body, function_env)? {
            Ok(*v)
        } else {
            Ok(Value::Nil)
        }
    }

    fn native_fn(
        &mut self,
        name: &str,
        parameters: &[String],
        closure: Shared<Environment>,
        arguments: &[Expr],
        native_function: &Shared<NativeFunction>,
        left_paren: &Token,
    ) -> Result<Value> {
        if parameters.len() != arguments.len() {
            bail!(runtime_error_with_token(
                format!(
                    "function {} expects {} arguments but got {}",
                    name,
                    parameters.len(),
                    arguments.len()
                ),
                left_paren
            ))
        }
        let mut evaluated_arguments = vec![];
        for argument in arguments {
            let evaluated_argument = self.evaluate(argument)?;
            evaluated_arguments.push(evaluated_argument);
        }
        let function_env = Rc::new(RefCell::new(Environment::new(Some(closure))));
        for (i, arg) in evaluated_arguments.clone().into_iter().enumerate() {
            let mut e = (*function_env).borrow_mut();
            e.define(parameters[i].clone(), arg);
        }
        let native_call = &mut (*native_function).borrow_mut().inner;
        Ok(native_call(evaluated_arguments, function_env))
    }

    fn instance_creation(
        &mut self,
        name: &str,
        parameters: &[String],
        methods: &HashMap<String, Rc<LoxFunction>>,
        arguments: &[Expr],
        left_paren: &Token,
        class: Rc<LoxFunction>,
    ) -> Result<Value> {
        if parameters.len() != arguments.len() {
            bail!(runtime_error_with_token(
                format!(
                    "Class constructor for {} expects {} arguments but got {}",
                    name,
                    parameters.len(),
                    arguments.len()
                ),
                left_paren
            ))
        }
        let instance = Value::Instance(class.clone(), Rc::new(RefCell::new(HashMap::new())));
        if let Some(init) = methods.get("init") {
            if let LoxFunction::UserDefined(name, parameters, body, closure) =
                self.bind_method(init, instance.clone(), left_paren, &class)?
            {
                self.user_defined_fn(&name, &parameters, &body, closure, arguments, left_paren)?;
            } else {
                bail!(runtime_error_with_token(
                    format!("Invalid constructor method {:?} for class {}", init, name),
                    left_paren
                ))
            }
        }
        Ok(instance)
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
        common::scanner::Scanner,
        errors::*,
        tree_walk_interpreter::{
            parser::{Parser, Stmt},
            resolver::Resolver,
        },
    };
    use std::{cell::RefCell, f64::EPSILON, rc::Rc};

    use super::{define_native_fn, Environment, Interpreter, Value};

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
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        let expected = "hello world!\n".to_string();
        interpreter.interpret(&statements, locals)?;
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
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        let expected = "true\n".to_string();
        interpreter.interpret(&statements, locals)?;
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
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        let expected = r#"outer variable
outer variable set to inner variable
created a new inner variable
outer variable set to inner variable
"#
        .to_string();
        match interpreter.interpret(&statements, locals) {
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
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        let expected = "if\nsecond else\n".to_string();
        interpreter.interpret(&statements, locals)?;
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
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        let expected = "one\ntwo\nthree\nfour\n".to_string();
        interpreter.interpret(&statements, locals)?;
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
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        let expected = "1\n2\n3\n4\n".to_string();
        interpreter.interpret(&statements, locals)?;
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
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        let expected = "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n".to_string();
        interpreter.interpret(&statements, locals)?;
        assert_eq!(expected, utf8_to_string(&buf));
        Ok(())
    }

    #[test]
    fn native_function_clock() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Scopes
            print clock();
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        define_native_fn("clock".to_string(), &mut interpreter, Interpreter::clock());
        interpreter.interpret(&statements, locals)?;
        let output = utf8_to_string(&buf);
        // This will fail if it is not f64
        let _ = output.trim().parse::<f64>().unwrap();
        Ok(())
    }

    #[test]
    fn user_defined_fun() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // function
            
            fun test_function(a, b) {
                print a + b;
            }
            var a = "hello";
            test_function(a, " world");
            var b = 4;
            test_function(1,b);
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        interpreter.interpret(&statements, locals)?;
        let output = utf8_to_string(&buf);
        assert_eq!("hello world\n5\n", output);
        Ok(())
    }
    #[test]
    fn closure() -> Result<()> {
        // Closure
        let mut scanner = Scanner::new(
            r#"
            // function
            
            fun makeCounter() {
                var i = 0;
                fun count() {
                  i = i + 1;
                  print i;
                }
              
                return count;
              }
              
              var counter = makeCounter();
              counter(); // "1".
              counter(); // "2"
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        interpreter.interpret(&statements, locals)?;
        let output = utf8_to_string(&buf);
        assert_eq!("1\n2\n", output);
        Ok(())
    }

    #[test]
    fn fibonacci() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Fibonacci
            fun fib(n) {
              if (n <= 1) 
                  return n;
              return fib(n - 2) + fib(n - 1);
            }
            
            for (var i = 0; i < 20; i = i + 1) {
              print fib(i);
            }
            
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        interpreter.interpret(&statements, locals)?;
        let output = utf8_to_string(&buf);
        assert_eq!(
            "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n",
            output
        );
        Ok(())
    }

    #[test]
    fn class_with_behaviour() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            // Fibonacci
            class Bacon {
                eat() {
                  print "Crunch crunch crunch!";
                }
                eat_quietly() {
                    print "mm mmm mmm!";
                }
              }
              Bacon().eat();
              var eater = Bacon();
              eater.eat();
              eater.eat_quietly();
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        interpreter.interpret(&statements, locals)?;
        let output = utf8_to_string(&buf);
        assert_eq!(
            "Crunch crunch crunch!\nCrunch crunch crunch!\nmm mmm mmm!\n",
            output
        );
        Ok(())
    }

    #[test]
    fn class_with_state() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            class Cake {
              }
              
              var cake = Cake();
              cake.flavor = "German chocolate";
              print cake.flavor;
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        interpreter.interpret(&statements, locals)?;
        let output = utf8_to_string(&buf);
        assert_eq!("German chocolate\n", output);
        Ok(())
    }

    #[test]
    fn class_with_state_and_behaviour() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            class Cake {
                taste() {
                  var adjective = "delicious";
                  print "The " + this.flavor + " cake is " + adjective + "!";
                }
              }
              
              var cake = Cake();
              cake.flavor = "German chocolate";
              cake.taste(); 
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        interpreter.interpret(&statements, locals)?;
        let output = utf8_to_string(&buf);
        assert_eq!("The German chocolate cake is delicious!\n", output);
        Ok(())
    }

    #[test]
    fn class_with_state_and_behaviour_and_constructor() -> Result<()> {
        let mut scanner = Scanner::new(
            r#"
            class Cake {
    
                init(type) {
                    this.type = type;
                }
            
                taste() {
                    this.inner_taste();
                    this.flavor = "Belgian chocolate";
                }
            
                 taste_again() {
                    this.inner_taste();
                }
            
                inner_taste() {
                    var adjective = "delicious";
                    print "The " + this.flavor + " " + this.type + " is " + adjective + "!";
                }
            }
            
            var cake = Cake("cake");
            cake.flavor = "German chocolate";
            cake.taste();
            cake.taste_again(); 
            
            
            var cake = Cake("cookie");
            cake.flavor = "German chocolate";
            cake.taste();
            cake.taste_again();  
        "#
            .into(),
        );
        let tokens = scanner.scan_tokens()?;
        let mut parser = Parser::new(tokens);
        let statements = parser.parse()?;
        let mut resolver = Resolver::new();
        let locals = resolver.resolve(&statements)?;
        let mut buf = vec![];
        let mut interpreter = Interpreter::new_with_writer(Some(&mut buf));
        interpreter.interpret(&statements, locals)?;
        let output = utf8_to_string(&buf);
        assert_eq!("The German chocolate cake is delicious!\nThe Belgian chocolate cake is delicious!\nThe German chocolate cookie is delicious!\nThe Belgian chocolate cookie is delicious!\n", output);
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
        let mut env = Environment::new(Some(Rc::new(RefCell::new(env))));
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
        let v = env.enclosing.unwrap();
        assert_eq!(None, v.as_ref().borrow().get("inner"));
    }
}

use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{chunk::Chunk, lox::Shared};

#[derive(Debug, Clone)]
pub enum Value {
    Unitialized,
    Bool(bool),
    Nil,
    Number(f64),
    Object(Object),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => f.write_str(&b.to_string()),
            Value::Nil => f.write_str("nil"),
            Value::Number(n) => f.write_str(&n.to_string()),
            Value::Object(o) => f.write_str(&o.to_string()),
            Value::Unitialized => f.write_str("Uninitialized"),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Unitialized
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Closure(Shared<Closure>),
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Function,
    pub upvalues: Vec<Shared<Upvalue>>,
}

impl Closure {
    pub fn new(function: Function) -> Self {
        Closure {
            function,
            upvalues: Vec::new(),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fun = &self.function;
        f.write_str(&fun.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    pub location: *mut Value,
    pub closed: Option<Value>,
}

impl Upvalue {
    pub fn new(value: *mut Value) -> Self {
        Upvalue {
            location: value,
            closed: None,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::String(s) => f.write_str(s),
            Object::Closure(c) => f.write_str(&(*c.borrow()).to_string()),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

pub fn shared<T>(v: T) -> Shared<T> {
    Rc::new(RefCell::new(v))
}

#[derive(Debug, Clone)]
pub enum Function {
    UserDefined(UserDefinedFunction),
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Function::UserDefined(u) => {
                let mut name = &*u.name;
                if name.is_empty() {
                    name = "script";
                }
                f.write_str(&format!("<fn {}>", name))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserDefinedFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
    pub upvalue_count: usize,
}

impl UserDefinedFunction {
    pub fn new(arity: usize, chunk: Chunk, name: String) -> Self {
        UserDefinedFunction {
            arity,
            chunk,
            name,
            upvalue_count: 0,
        }
    }
}
#[allow(dead_code)]
#[derive(Debug)]
pub struct Values {
    objects: HashMap<String, Value>,
}

#[allow(dead_code)]
impl Values {
    pub fn new() -> Self {
        Values {
            objects: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: Value) {
        self.objects.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.objects.get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        self.objects.get_mut(key)
    }

    pub fn remove(&mut self, key: &str) {
        self.objects.remove(key);
    }
}

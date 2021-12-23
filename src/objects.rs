use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{chunk::Chunk, lox::Shared};

#[derive(Debug, Clone)]
pub enum Value {
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    String(Shared<String>),
    Function(Shared<Function>),
    Nil,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::String(s) => {
                let s = (&*s).borrow();
                f.write_str(&s)
            }
            Object::Function(fun) => f.write_str(&((**fun).borrow()).to_string()),
            Object::Nil => f.write_str("Nil"),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Function(_), Self::Function(_)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Object {
    #[inline]
    pub fn string(string: String) -> Object {
        Object::String(Rc::new(RefCell::new(string)))
    }

    #[inline]
    pub fn function(function: Function) -> Object {
        Object::Function(Object::shared_function(function))
    }

    #[inline]
    pub fn shared_function(function: Function) -> Shared<Function> {
        Rc::new(RefCell::new(function))
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
                let name = &*u.name.borrow();
                let mut name = name.as_str();
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
    pub object: Option<Object>,
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Shared<String>,
}

impl UserDefinedFunction {
    pub fn new(object: Option<Object>, arity: usize, chunk: Chunk, name: Shared<String>) -> Self {
        UserDefinedFunction {
            object,
            arity,
            chunk,
            name,
        }
    }
}
#[derive(PartialOrd, Debug, Eq)]
struct Key(Shared<String>);

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        *(&self.0).borrow() == *(&other.0).borrow()
    }
}

impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (&self.0).borrow().hash(state);
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Values {
    objects: HashMap<Key, Value>,
}

#[allow(dead_code)]
impl Values {
    pub fn new() -> Self {
        Values {
            objects: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: Shared<String>, value: Value) {
        self.objects.insert(Key(key), value);
    }

    pub fn get(&self, key: &Shared<String>) -> Option<&Value> {
        self.objects.get(&Key(key.clone()))
    }

    pub fn get_mut(&mut self, key: &Shared<String>) -> Option<&mut Value> {
        self.objects.get_mut(&Key(key.clone()))
    }

    pub fn remove(&mut self, key: &Shared<String>) {
        self.objects.remove(&Key(key.clone()));
    }
}

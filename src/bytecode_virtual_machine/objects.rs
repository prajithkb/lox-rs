use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::common::lox::Shared;

use super::chunk::Chunk;

pub type Byte = u8;

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
    Function(Rc<Function>),
    Class(Rc<Class>),
    Instance(Instance),
    Closure(Closure),
    Receiver(Rc<Value>, Rc<Closure>),
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub methods: Shared<HashMap<String, Rc<Closure>>>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Class {
            name,
            methods: shared(HashMap::new()),
        }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("<class {}>", &self.name))
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Rc<Class>,
    pub fields: Shared<HashMap<String, Value>>,
}

impl Instance {
    pub fn new(class: Rc<Class>) -> Self {
        Instance {
            class,
            fields: shared(HashMap::new()),
        }
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("<instance of {}>", &self.class.name))
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Shared<Upvalue>>,
}

impl Closure {
    pub fn new(function: Rc<Function>) -> Self {
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
    pub location: Location,
}

#[derive(Debug, Clone)]
pub enum Location {
    Stack(usize),
    Heap(Value),
}

impl Upvalue {
    pub fn new_with_location(location: Location) -> Self {
        Upvalue { location }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::String(s) => f.write_str(s),
            Object::Closure(c) => f.write_str(&c.to_string()),
            Object::Function(fun) => f.write_str(&fun.to_string()),
            Object::Class(class) => f.write_str(&class.to_string()),
            Object::Instance(instance) => f.write_str(&instance.to_string()),
            Object::Receiver(value, method) => {
                f.write_str(&format!("{} as receiver for {}", value, method))
            }
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
    Native(NativeFunction),
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
            Function::Native(n) => f.write_str(&format!("<native fn {}>", n.name)),
        }
    }
}

pub type NativeFn = Rc<dyn Fn(Vec<Value>) -> Value>;

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub function: NativeFn,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction")
            .field("arg_count", &self.arity)
            .finish()
    }
}

impl NativeFunction {
    pub fn new(name: String, arity: usize, function: NativeFn) -> Self {
        NativeFunction {
            name,
            arity,
            function,
        }
    }

    pub fn call(&self, arguments: Vec<Value>) -> Value {
        let function = self.function.clone();
        function(arguments)
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

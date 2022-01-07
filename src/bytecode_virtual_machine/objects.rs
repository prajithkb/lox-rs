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
#[derive(Debug, Clone, Eq)]
pub struct SharedString(pub Rc<String>);

impl SharedString {
    pub fn from_str(s: &str) -> Self {
        SharedString::from_string(s.into())
    }

    pub fn from_string(s: String) -> Self {
        SharedString(Rc::new(s))
    }
}

impl PartialEq for SharedString {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(Rc::as_ptr(&self.0), Rc::as_ptr(&other.0)) || self.0 == other.0
    }
}

impl std::hash::Hash for SharedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // std::ptr::hash(Rc::as_ptr(&self.0), state);
        self.0.hash(state)
    }
}
impl Display for SharedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    SharedString(SharedString),
    Function(Rc<Function>),
    Class(Rc<Class>),
    Instance(Instance),
    Closure(Closure),
    Receiver(Rc<Value>, Rc<Closure>),
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: SharedString,
    pub methods: Shared<HashMap<SharedString, Rc<Closure>>>,
}

impl Class {
    pub fn new(name: SharedString) -> Self {
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
    pub fields: Shared<HashMap<SharedString, Value>>,
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
            Object::SharedString(s) => f.write_str(&s.to_string()),
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
                let mut name = &**u.name.0;
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
    pub name: SharedString,
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
    pub fn new(name: SharedString, arity: usize, function: NativeFn) -> Self {
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
    pub name: SharedString,
    pub upvalue_count: usize,
}

impl UserDefinedFunction {
    pub fn new(arity: usize, chunk: Chunk, name: SharedString) -> Self {
        UserDefinedFunction {
            arity,
            chunk,
            name,
            upvalue_count: 0,
        }
    }
}

// pub type Strings = Objects<String, SharedString>;

pub type Values = Objects<SharedString, Value>;

#[allow(dead_code)]
#[derive(Debug)]
pub struct Objects<K, V>
where
    K: std::hash::Hash + std::cmp::Eq,
{
    objects: HashMap<K, V>,
}

#[allow(dead_code)]
impl<K: std::hash::Hash + std::cmp::Eq, V> Objects<K, V> {
    pub fn new() -> Self {
        Objects {
            objects: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.objects.insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.objects.get(key)
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.objects.get_mut(key)
    }

    pub fn remove(&mut self, key: &K) {
        self.objects.remove(key);
    }
}

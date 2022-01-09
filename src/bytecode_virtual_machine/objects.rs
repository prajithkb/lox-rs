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

#[derive(Debug, Clone, Copy)]
pub enum StackValue {
    Unitialized,
    Bool(bool),
    Nil,
    Number(f64),
    Object(ObjectPtr),
}

impl Default for StackValue {
    fn default() -> Self {
        StackValue::Unitialized
    }
}

impl Display for StackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackValue::Bool(b) => f.write_str(&b.to_string()),
            StackValue::Nil => f.write_str("nil"),
            StackValue::Number(n) => f.write_str(&n.to_string()),
            StackValue::Object(o) => f.write_str(&o.to_string()),
            StackValue::Unitialized => f.write_str("Uninitialized"),
        }
    }
}

impl From<&Value> for StackValue {
    #[inline(always)]
    fn from(v: &Value) -> Self {
        match v {
            Value::Unitialized => StackValue::Unitialized,
            Value::Bool(b) => StackValue::Bool(*b),
            Value::Nil => StackValue::Nil,
            Value::Number(n) => StackValue::Number(*n),
            Value::Object(o) => StackValue::Object(ObjectPtr::new(o)),
        }
    }
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

#[derive(Debug, Clone, Copy)]
pub struct ObjectPtr {
    pub ptr: *const Object,
}

impl ObjectPtr {
    pub fn new(ptr: *const Object) -> Self {
        ObjectPtr { ptr }
    }

    pub fn object_ref(&self) -> &Object {
        unsafe { self.ptr.as_ref().expect("Null pointer") }
    }
}

impl Display for ObjectPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let o = self.object_ref();
        f.write_str(&o.to_string())
    }
}

#[derive(Debug, Clone, Eq)]
pub struct SharedString(pub Rc<String>);

impl SharedString {
    #[allow(clippy::should_implement_trait)]
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
    Instance(Rc<Instance>),
    Closure(Rc<Closure>),
    Receiver(Rc<StackValue>, Rc<Closure>),
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
    pub fields: Shared<HashMap<SharedString, StackValue>>,
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
    Heap(StackValue),
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

pub type NativeFn = Rc<dyn Fn(Vec<StackValue>) -> StackValue>;

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

    pub fn call(&self, arguments: Vec<StackValue>) -> StackValue {
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

pub type Strings = Objects<String, SharedString>;

pub type Values = Objects<SharedString, StackValue>;

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
    #[allow(clippy::new_without_default)]
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

#[cfg(test)]
mod tests {
    use std::{f64::EPSILON, ptr::NonNull, time::Instant};

    use super::SharedString;

    // use super::{Object, SharedString, Value};

    #[derive(Debug, Clone)]
    pub enum Value {
        Unitialized,
        Bool(bool),
        Nil,
        Number(f64),
        // Object(ObjectPtr),
        Object(Object),
    }

    #[derive(Debug, Clone, Copy)]
    pub struct ObjectPtr {
        ptr: NonNull<Object>,
    }

    impl ObjectPtr {
        // pub fn new(ptr: *mut Object) -> Self {
        // let ptr = NonNull::new(ptr).expect("Null pointer");
        // ObjectPtr { ptr }
        // }
    }

    #[derive(Debug, Clone)]
    pub enum Object {
        SharedString(SharedString),
    }
    impl Default for Value {
        fn default() -> Self {
            Value::Unitialized
        }
    }

    #[test]
    fn primitives_read_write() {
        let constants = vec![
            Value::Nil,
            Value::Number(1.0),
            Value::Bool(true),
            Value::Bool(false),
            // Value::Object(ObjectPtr::new(&mut str as *mut Object)),
            // Value::Object(ObjectPtr::new(&mut stru as *mut Object)),
            Value::Object(Object::SharedString(SharedString::from_str("str"))),
            Value::Object(Object::SharedString(SharedString::from_str("stru"))),
        ];
        println!("{}", std::mem::size_of::<Value>());

        let mut stack = [
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ];

        let start = Instant::now();
        let mut count = 0;
        let operations = 10000000;
        while count < operations {
            for i in 0..constants.len() {
                for j in 0..constants.len() {
                    let a = constants[i].clone();
                    let b = constants[j].clone();
                    // let a = constants[i];
                    // let b = constants[j];
                    stack[0] = a;
                    stack[1] = b;
                    let a = std::mem::take(&mut stack[0]);
                    let b = std::mem::take(&mut stack[1]);
                    stack[0] = Value::Bool(value_equals(a, b));
                    count += 1;
                }
            }
        }
        println!(
            "Time for {} operations ={} ms",
            operations,
            start.elapsed().as_millis()
        )
    }
    fn value_equals(l: Value, r: Value) -> bool {
        match (l, r) {
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            (Value::Number(l), Value::Number(r)) => num_equals(l, r),
            (Value::Object(Object::SharedString(l)), Value::Object(Object::SharedString(r))) => {
                l == r
            }
            // (Value::Object(l), Value::Object(r)) => {
            //     let l: &Object = unsafe { l.ptr.as_ref() };
            //     let r: &Object = unsafe { r.ptr.as_ref() };
            //     match (l, r) {
            //         (Object::String(l), Object::String(r)) => l == r,
            //     }
            // }
            _ => false,
        }
    }
    fn num_equals(l: f64, r: f64) -> bool {
        (l - r).abs() < EPSILON
    }
}

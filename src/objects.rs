use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use crate::{chunk::Value, lox::Shared, vm::StackValue};
#[allow(dead_code)]
#[derive(Debug)]
pub struct Values {
    objects: HashMap<String, Shared<Value>>,
}
#[allow(dead_code)]
impl Values {
    pub fn new() -> Self {
        Values {
            objects: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: StackValue) -> Shared<Value> {
        let v = Values::as_shared(value);
        self.objects.insert(key, v.clone());
        v
    }
    #[inline]
    pub fn as_shared(v: StackValue) -> Shared<Value> {
        match v {
            StackValue::Owned(v) => Rc::new(RefCell::new(v)),
            StackValue::Shared(v) => v,
        }
    }

    pub fn get(&self, key: &str) -> Option<&Shared<Value>> {
        self.objects.get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Shared<Value>> {
        self.objects.get_mut(key)
    }

    pub fn entry(&mut self, key: String) -> Entry<String, Shared<Value>> {
        self.objects.entry(key)
    }

    pub fn remove(&mut self, key: &str) {
        self.objects.remove(key);
    }
}

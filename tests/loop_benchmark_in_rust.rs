// use lox_rs::bytecode_virtual_machine::objects::Value;

#[derive(Debug, Clone)]
pub enum Value {
    Unitialized,
    Bool(bool),
    Nil,
    Number(f64),
}

#[test]
fn random_test() {
    println!("Size of value {}", std::mem::size_of::<Value>());
}

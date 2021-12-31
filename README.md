## Implementation of LOX in Rust from [craftinginterpretors.com](craftinginterpreters.com)

My attempt to implement `jlox` (tree walk interpreter) and `clox` (stack machine based byte code interpreter) in Rust.

`clox` is ten times faster than `jlox`. Both are implemented in completely safe Rust.



## To run

```
cargo run <vm|interpreter> <path to .lox file>
```



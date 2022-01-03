## Implementation of LOX in Rust from [craftinginterpretors.com](craftinginterpreters.com)

My attempt to implement `jlox` (I call it interpreter) (tree walk interpreter) and `clox` (I call it vm) (stack machine based byte code interpreter) in Rust.

`clox` is ten times faster than `jlox`. Both are implemented in completely safe Rust.



## To run

```
cargo run <vm|interpreter> <path to .lox file>
```



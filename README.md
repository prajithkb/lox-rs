## Implementation of LOX in Rust from [craftinginterpretors.com](craftinginterpreters.com)

The [book](craftinginterpreters.com) has two implementations of the Lox programming language which the author called as Jlox and Clox. I have implemented both of them in `safe` Rust in an attempt to learn the concepts and as a hobby project to learn Rust.

The complete Lox grammer is available [here](https://craftinginterpreters.com/appendix-i.html).

TODO: Inheritance, for both Jlox and Clox, I stopped just before implementing inheritance. Mainly because I was out of time and felt that there was nothing new to learn. 

### Jlox
A Java based tree walk interpreter to help understand the programming language and it's concepts.  I have implemented the same in rust and called it an "interpreter"

To run that use

```
cargo run interpreter <optional path to lox file >

```

If you run the above command without the path to file it will be in repl mode. The source code for that is present in the `tree_walk_interpreter` folder.

### Clox
A C based, stack based byte code virtual machine, that is performant and helps you understand implementation details of a stack based virtual machine. I call it the "vm". 

TODO: Garbage collection. Currently the implementation does not have garbage collection.

To run that use

```
cargo run vm  <optional path to lox file >

```
Just like Clox, if you run the above command without the path to file it will be in repl mode. The source code for that is present in the `byte_code_interpreter` folder.



My benchmark shows that `vm` (my implementation of Clox)is ten times faster than `interpreter` (my implementation of Jlox).


Additionally, a set of lox scripts that I used to test can be found in the `lox-scripts` folder.



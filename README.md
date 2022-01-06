## Implementation of LOX in Rust from [craftinginterpretors.com](craftinginterpreters.com)

The [book](craftinginterpreters.com) has two implementations of the Lox programming language which the author called as Jlox and Clox. I have implemented both of them in `safe` Rust in an attempt to learn the concepts and as a hobby project to learn Rust.

The complete Lox grammer is available [here](https://craftinginterpreters.com/appendix-i.html).

### TODOs
* Inheritance, for both Jlox and Clox, I stopped just before implementing inheritance. Mainly because I was out of time and felt that there was nothing new to learn. 

* Garbage collection. Currently the Clox implementation does not have garbage collection.

* Performance optimization. Need to profile and tweak it.


### Jlox
A Java based tree walk interpreter to help understand the programming language and it's concepts.  I have implemented the same in rust and called it an "interpreter"

To run that use

```
cargo run interpreter <optional path to lox file >

```

If you run the above command without the path to file it will be in repl mode. The source code for that is present in the `tree_walk_interpreter` folder.

### Clox
A C based, stack based byte code virtual machine, that is performant and helps you understand implementation details of a stack based virtual machine. I call it the "vm". 


To run that use

```
cargo run vm  <optional path to lox file >

```
Just like Clox, if you run the above command without the path to file it will be in repl mode. The source code for that is present in the `byte_code_interpreter` folder.



My benchmark shows that `vm` (my implementation of Clox)is ten times faster than `interpreter` (my implementation of Jlox).


Additionally, a set of lox scripts that I used to test can be found in the `lox-scripts` folder.

### Benchmarks
The `test` folder contains the benchmarking scripts that I copied from https://github.com/munificent/craftinginterpreters/tree/master/test/benchmark. Currently my implementation lags behind (massively) the Clox implementation on all the benchmark tests. 

To run the benchmark comparison run.

```
 cargo test -- --nocapture perf_timings

```
This will output a neat table of comparison results that looks like this


```
+---------------------+----------------------+--------------------+-----------------------+
| Test                | Clox time in seconds | Vm time in seconds | Percentage difference |
+---------------------+----------------------+--------------------+-----------------------+
| a.lox               | 0.011161363          | 0.011533829        | 3.3371013916490284    |
+---------------------+----------------------+--------------------+-----------------------+
| binary_trees.lox    | 3.229505029          | 24.049177315       | 644.6706878932066     |
+---------------------+----------------------+--------------------+-----------------------+
| equality.lox        | 3.001326203          | 65.008122904       | 2065.979920443856     |
+---------------------+----------------------+--------------------+-----------------------+
| fib.lox             | 0.952522808          | 13.163639983       | 1281.9763550480777    |
+---------------------+----------------------+--------------------+-----------------------+
| instantiation.lox   | 1.821285462          | 6.254518575        | 243.4123153946308     |
+---------------------+----------------------+--------------------+-----------------------+
| invocation.lox      | 0.24067549           | 5.87811554         | 2342.3407385604573    |
+---------------------+----------------------+--------------------+-----------------------+
| properties.lox      | 0.387031488          | 5.646424429        | 1358.9056973576269    |
+---------------------+----------------------+--------------------+-----------------------+

```




# llvm_of_jvm

**Ahead of time compliation of java bytecode to llvm ir.**  

See [devlog.md](devlog.md) (reverse chronological order) if interested in far too many details on how it was made. 

> - This is a toy project. Use GraalVM for anything real.
> - This is also my first project learning OCaml so the code probably sucks.

The only garbage collection strategy so far is a reimplementation of the JDK's [Epsilon GC](https://blogs.oracle.com/javamagazine/post/epsilon-the-jdks-do-nothing-garbage-collector) ...what I'm saying is we just never free anything. That's good enough for short lived programs. 

## Implemented 

- Math on all primitive types (+, -, *, =, %)
- Bitwise operations (>>, <<, |, &)
- Comparison on all primitive types (==, !=, >=, <=, <, >)
- Jumps (If, IfCmp): ifs, while/for loops, ternary operator
- Casts between primitive types
- Static methods, fields, initializer blocks, and native methods
- Arrays and multidimensional arrays
- Class fields
- Inheritance
- Virtual method calls
- Instanceof checks and object casts
- Interfaces, abstract methods, and default methods
- String concatenation, lambdas

### Incomplete

- tests for all casts and bitwise op
- tests for math on mixed primitive types and with overflow
- cmp of long/float/double that uses the value produced instead of jumping
- divide by zero trap
- deterministic name mangling for overloaded functions 
- automaticlly call `_clinit_` on every class that needs it
- garbage collection 
- exceptions instead of assertions for bounds checks 
- interface method calls require traversing a linked list
- actually handle locks for syncronised blocks
- try/catch blocks


## Libraries Used 

- [Official Llvm Ocaml Bindings](https://github.com/llvm/llvm-project/tree/main/llvm/bindings/ocaml): emit llvm ir
- [Javalib](https://github.com/javalib-team/javalib): parse jvm classfiles 

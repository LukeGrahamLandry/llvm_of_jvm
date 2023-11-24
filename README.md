# llvm_of_jvm

**Ahead of time compliation of java bytecode to llvm ir.**  

See [devlog.md](devlog.md) (reverse chronological order) if interested in far too many details on how it was made. 

> - This is a toy project. Use GraalVM for anything real.
> - This is also my first project learning OCaml so the code probably sucks.


## Implemented 

- Math on all primitive types (+, -, *, =, %)
- Bitwise operations (>>, <<, |, &)
- Comparison on all primitive types (==, !=, >=, <=, <, >)
- Jumps (If, IfCmp): ifs, while/for loops
- Casts between primitive types
- Static methods, fields, initializer blocks, and native methods. 

### Incomplete

- tests for all casts and bitwise op
- tests for math on mixed primitive types and with overflow
- cmp of long/float/double that uses the value produced instead of jumping
- ternary operator phi node
- divide by zero trap
- overloaded functions name mangling 
- automaticlly call `_clinit_` on every class that needs it

## Libraries Used 

- [Official Llvm Ocaml Bindings](https://github.com/llvm/llvm-project/tree/main/llvm/bindings/ocaml): emit llvm ir
- [Javalib](https://github.com/javalib-team/javalib): parse jvm classfiles 

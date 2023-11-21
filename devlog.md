## Whats in java bytecode

- https://en.wikipedia.org/wiki/List_of_Java_bytecode_instructions

There are two things, local variables and ~the~ stack. But ~the~ stack is not the call stack. 
So every frame of the call stack has some local variables and a new ~the~ stack. Where opcodes load/store things between local variables and ~the~ stack and always perform operations on ~the~ stack. So `(c = a + b)` becomes `(load a; load b; add; store c;)`. Locals refered to by index (do I get thier types up front?) but no random access to the stack I think. So locals can just be alloc but then do I have to actually do all the stack manipulation at runtime? Maybe start with that but seems there should be some trivial data flow analysis that can be done at compile time for many sequences like the example above. But I'm afraid because what happens when you start jumping around. 

All the if* (cmp branch) instructions refer to target opcode index with a branchoffset. So I need to reconstruct the basic blocks to give to llvm. 

Once again arrived at the age old question of do i directly translate bytecode to llvm ir or go through my own representation that makes more sense to me in the middle. Having more layers always feels kinda verbose but it really seems like the trick that makes writing compilers much easier. 

Maybe the answer to my printing problem is using the repl but i can't figure out how to make it find library implementations. Other than that I don't really mind it. You kinda just try haskell syntax and if that doesn't work try rust syntax. 

To allocate space for the variables, need to know thier types, its in the local_variable_table. Each entry contains and index, which is weird are they not in order? Its the doubles take two slots thing again. 
start_pc and length are the lifetimes of the variable. Kinda afraid cause the spec says its optional and for debuggers. Maybe I'm supposed to be using the stack_map_table instead? sad day, its not there in my test so yeah was optional.
- https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.13

## Reading java bytecode (Nov 20)

library confusing choice: jopcode is like a nice ocaml type that has the data you want already parsed out but if you iterate over the opcodes in a method, you get a bunch of OpInvalid for every argument that was in the bytecode and already parsed out, so i guess just skip those? is there a method to only iterate real things instead of eveery byte? 
compent implies at least that its putting those in for arguments on purpose and not just blindly parsing where indexes could collide. yeah, if you have a constant that compiles to bipush of something i know is a valid opcode, it still shows up as opinvalid which is good. so that's just how they tell me how many arguments it takes i guess. 

There's a confusing path of |jmethod -> concrete_method -> method_signature -> list (value_type -> java_basic_type -> other_num -> float) -> array value_type| which i can finally put call the llvm method i want with. That data structure feels convoluted but maybe dealing with bytecode is just a pain. 

Why does jmethod have a generic parameter? 

- Is ocaml order dependent?! Can I only use functions after i declare them?! I will cry.
- Have to be very careful about putting brackets around arguments. `f add 1 2` means `f(add, 1, 2)` not `f(add(1, 2))` and I find the error messages a bit confusing sometimes cause O'm not used to it. 
- How do you just print something? Are there no traits? Seems like I need to find the specific string_of_whatever function for each type. 
- Syntax for generics is backwards which takes some getting used to. 
- I feel like vs code is very confused by javalib. Everything's in many files and once you click into one, it can't find the types that are defined in other files so I have to click in to find the type I'm interested in and then copy the name and use it as an annotation for a variable in my own code and then click into that to actually go to the new file. Can't even search everywhere for the name if the file isn't open cause its gitignored maybe? idk, can't say its worse than zig's language server tho so who am I to complain. 
- Once you've carved out a little corner you can just write code in it feels pretty normal. Much more imparitive than I expected. I'm just calling C functions and chilling. 


## Getting Started (Nov 19)

- https://github.com/llvm/llvm-project/tree/main/llvm/bindings/ocaml
- https://github.com/javalib-team/javalib
- https://ocamlverse.net/content/quickstart_ocaml_project_dune.html

For some reason ocaml is like first class citizen with in-tree llvm bindings and i want to practise a functional-y language so might as well learn ocaml. 


- Build and run: `dune exec llvm_of_jvm`
- Make ide show errors: `dune build --watch`
    - that locks the build directory but builds continuously so can run it directly `./_build/install/default/bin/llvm_of_jvm`
- Add dependency: 
    - in `dune-project` `depends` section (`name=version`)
    - in individual lib/bin `dune` `libraries` section
    - open `Whatever` in .ml code (can be same or not as library name)
    - `opam install libname` (why doesnt build system do that? what am i missing? seems to be just in the local switch thingy tho which is good but means you need a new compiler install for every project?)

opam switches are like rustup toolchains? 

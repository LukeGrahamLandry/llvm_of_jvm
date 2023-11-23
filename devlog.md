## other primitive types (Nov 23)

Math is easy, just use the right llvm builder depending on the type. 

Unfortunate that bytecode doesn't have ifcmp on floats in the same way. 
fcmp returns a value that i assume it will generally jump on immediately after. 
So either 
    - I produce that value and do a double jump OR 
    - i have to notice that pair of instructions and emit them as one. 
But if I try to be clever and do the second thing it makes jumps a bit more confusing. 
Because jumping to that would be to the fcmp but jumping away is the ifcmp so my current 
way of spliting up blocks would make two. So treating two ops as one instruction isn't elegant. 
I wonder if an fcmp is ~always~ followed by an ifcmp. Also wonder if you ~never~ jump to 
an ifcmp that's after an fcmp (maybe that's implied by stack_comptime_safe). 
Then you could just see any jump that targets an fcmp as actually targeting the next instruction 
and treat ifcmp as having its type modified by the previous instruction if its an fcmp. 


LLvm docs say "Ordered means that neither operand is a QNAN while unordered means that either operand may be a QNAN". 
Does that mean 
    - those are preconditions the instruction assumes OR
    - ordered alwasy returns false if there's a NAN and unordered always returns true if there's a NAN?
I hope the second one because then it matches with the bytecode fcmpg vs fcmpl. 

Also a little intimidating is it looks like the ternary operator pushes different things to the stack on each branch and then rejoins. Which it seems like my stack_comptime_safe isn't checking so have to do further invenstigation there 
once this works. Actually problem was previously assuming that Cmp was stack-2 but its -1 cause returns so now
at least stack_comptime_safe works and catches the problem early.  

## Function calls (Nov 22)

Functions receive arguments as locals (already handled) and they pass arguments by putting them on the stack before the call instruction. 
For llvm to call a function it needs the type (signeture) and the function llvalue reference.
Don't think you can get a function by name but when you define it you get the llvalue. 
So need to forward declare everything I might want to call. (that's what i did in seesea anyway). 
When you `append_block` you give it the function value to add to so seems its fine to not do everything in order,  
there's no global current function that's the last you declared. 
What's the difference between `declare_function` and `define_function`? Maybe internal defined later vs external to be imported later by the linker? 

Short circuiting for `or` is fine because it just becomes normal operations but somehow `and` isnt fine and llvm complains about an empty block. Giving the blocks names based on thier opcode index so its easy to compare to the bytecode from javap. Two for index 12? Just needed to remove duplicates from the list of instructions to create blocks for. I was making a new block for each time an instruction was seen as a jump target. 

## Jumping around (Nov 22)

Helpfully, bytecode and llvm ir have overlap in the int cmp types. Just ignoring stuff about references for now but would probably be easy to just cast to an int (or do they have a ptr_diff instruction?). 
Previous splitting into blocks seems to work fine. Just treat the implicit fallthrough on a failed comparison as an explicit jump to the very next instruction. 

Loops: 

Goto opcode can just go to a random place so we mark that as the start of a new block. 
But the instruction before needs to fall through into that new block. 
So as we're emitting, if the next instruction should be the start of a block (we saw it as a jump target) 
but this instruction isn't a terminator, need to add the explicit terminator. 
Which almost sounds elegant except for the variable number of OpInvalid (for args) in the bytecode stream. 
So it's not trivial to know if the next real instruction is the start of a block. 
Could look ahead to the next but that's annoying.

Actually, I can just do it at the start of the next instruction instead of the end of the previous instruction. 
I'm already checking if we're entering a new block so it can move the builder so when that happens, 
first check if the current block is terminated and if not, do the explicit fallthrough. 

So far so good on the stack_comptime_safe assumption. Suppose it would make sense if javac never generated code 
that didn't have that property because they have very similar goals as me for static data flow analysis by the jvm.


## final locals (Nov 22)

The next obvious fix is noticing when arguments are final so don't need to copy to locals. 
So go over the ops and count how many times you store to each local slot. 
If it's zero, it must be a function argument and you never mutate it so can use the llvm 
ssa register directly instead of allocating a stack slot. 

Kinda awquard information passing becuase alloc_locals counts so it knows which are final args but it 
doesn't know the argument registers so it just leaves a None in the localmap. Then when store_args sees a None, it trusts that must be final and replaces it with a FinalArg. 
Should maybe combine so first look at the function signeture and figure out the mapping of arguments to variable indexes (which is weird cause doubles take 2 slots) and then pass that to alloc locals so it can inline store_args there. 

Yeah that does feel better tho `alloc_locals` is a bit chonky now. 

Now the `add` function look reasonable. Doing the same thing with mutating the arg looks silly but I don't want to mess with it because ssa form gets scary once you add branching. 
Also should do the same thing for final locals that aren't arguments (1 store) but haven't yet. 

## comptime stack (Nov 21)

So currently it seems to work but I'm just emitting code for all the stack management stuff at runtime. 
So my 1 line add function becomes 40 lines of llvm ir (instead of the 2 it should be). 
LLVM can probably just fix it on its own but that's boring and the insanity makes it hard to debug my code. 
In this case, its obvious the stack book-keeping can be done at compile time to figure out how the data moves between instructions and then just generate code without a bunch of reduntant stuff. 
The scary thing is what happens when the code branches? Are different basic blocks allowed to leave different things 
on the stack and then converge and read it like a phi node? 
I guess I need to check if a basic block changes the size of the stack or ever goes below where it started and fall back to doing it at runtime. 
I'm hoping the stack promises to be the same height before and after each statement like in Crafting Interpreters. 
Then as I iterate over the instructions, keep a stack of llvalue and use that to emit instructions. 

That got it down to 7 lines for add. 

I've been agressivly writing type annotations cause i find it hard to tell what's going on but 
I figured out how to turn on the inlay type hints in vscode. It does seem to struggle with The Data Type Formerly Known As Enums sometimes tho.

operand ordering: (load_a load_b op_div) means (a divided by b)

that feels pretty good for day 3 ocaml. 

## Whats in java bytecode (Nov 21)

- https://en.wikipedia.org/wiki/List_of_Java_bytecode_instructions

There are two things, local variables and ~the~ stack. But ~the~ stack is not the call stack. 
So every frame of the call stack has some local variables and a new ~the~ stack. Where opcodes load/store things between local variables and ~the~ stack and always perform operations on ~the~ stack. So `(c = a + b)` becomes `(load a; load b; add; store c;)`. Locals refered to by index (do I get thier types up front?) but no random access to the stack I think. So locals can just be alloc but then do I have to actually do all the stack manipulation at runtime? Maybe start with that but seems there should be some trivial data flow analysis that can be done at compile time for many sequences like the example above. But I'm afraid because what happens when you start jumping around. 

All the if* (cmp branch) instructions refer to target opcode index with a branchoffset. So I need to reconstruct the basic blocks to give to llvm. 

Once again arrived at the age old question of do i directly translate bytecode to llvm ir or go through my own representation that makes more sense to me in the middle. Having more layers always feels kinda verbose but it really seems like the trick that makes writing compilers much easier. 

Maybe the answer to my printing problem is using the repl but i can't figure out how to make it find library implementations. Other than that I don't really mind it. You kinda just try haskell syntax and if that doesn't work try rust syntax. 

To allocate space for the variables, need to know thier types, its in the local_variable_table. Each entry contains and index, which is weird are they not in order? Its the doubles take two slots thing again. 
start_pc and length are the lifetimes of the variable. Kinda afraid cause the spec says its optional and for debuggers. Maybe I'm supposed to be using the stack_map_table instead? sad day, its not there in my test so yeah was optional. Even stack_map seems to be optional, can see with javap only some have it and it doesnt seem to have the info i want. Do i really have to infer types of everything? I guess its easy to just look at the types of all the loads and stores. Primitives have different opcodes but how do i know classes beyond just reference? 
- https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.13

Hashtbls are weird, `add` lets you have deuplicates? `replace` is the normal thing. 

For testing, can use llc to compile the text ir to asm and link that to a c program. 
`llc -O0 demo.ll && gcc runtime/callit.c demo.S && ./a.out`
(commands say gcc but its aliased to the clang that knows where apple hides shit in xcode, idk man)

My current derranged stack manipulation does not work when optimised but does work in O0 so I'm doing undefined behaviour somewhere I guess but idk where. BUT, if i run clang on it directly instead of llc, it works, even on O2.
So idk whats with that but im just gonna move on cause i wasted so much time trying to find a logic error in my crazy generated llvm ir. 

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

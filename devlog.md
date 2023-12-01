## todo: cleanup 

- redo stack_comptime_safe check 
- cleanup emitting blocks & phi nodes, statemachine is a bit weird 
- handle exception catch blocks properly 
- do a lock instead of no-op for monitors (just atomic set a bit in the object header?)
- make interfaces more efficient 
- clean up emiting vtable, seems redundant things could be factored out 
- write something that looks at the binary and breaks down space of functions and vtable data 
- multidimensional arrays are done in a silly way 
- im specifal case not emiting the clinit for Throwable because i want to be able to compile exceptions because lots of interesting things use them but dont use them as control flow so its fine if they just abort the program. 
- replace stub for java_lang_System_arraycopy

## todo: real jdk tests

https://github.com/openjdk/jdk/tree/master/test/jdk/java

the ones in `lang` at least seem orgnaised into little programs with a main method which is very helpful. 

- init exceptions (dont need to catch)
- string concatenation 
- standalone program from main(String...) 

## interfaces 

next in the quest to compile String.charAt, is handling `instanceof java.util.RandomAccess` in `java.util.List java.util.Collections.unmodifiableList(java.util.List)`

so i need to decide how i want to represent interfaces. the best seems like using a wide pointer like rust trait objects. but its awquard because you need it to be an instance of object. there is a special invokeinterface opcode so at least you always know when you're trying to call one. i guess make a new __Interface class or whatever that has a v-pointer and a data-pointer. and then you get a new vtable for each class that implements each interface. 

i guess interface inheritance is represented as the child implementing the parrent? 

when you're calling the method on the object directly it already works because it just shows up as the object adding that method and its in its vtable. 

the problem is that there's no explicit translation from normal class object to interface object. you can create an object, store it in a variable, load it back and do an invokeinterface on it. the load/store doesnt tell you any more than its an Object. So every instance must be a thing you can invokeinterface on. So no matter what class it actually is, it needs to be the same runtime operation, can't just put it in the vtable because they can't be at the same place for every possible implimenter because its not a direct hiarchy anymore. maybes it has to be the same as instanceof checks and you get a new field in your root vtable that's a linked list of all the interfaces you implement and a vtable for them. tho that sounds really slow, good start. 

so need to see every interface type that's referenced and create a vtable type for it. 



## no-op monitor (Nov 30)

```
static void use_syncronized_block();
    Code:
       0: new           #2                  // class java/lang/Object
       3: dup
       4: invokespecial #1                  // Method java/lang/Object."<init>":()V
       7: dup
       8: astore_0
       9: monitorenter
      10: aload_0
      11: monitorexit
      12: goto          20
      15: astore_1
      16: aload_0
      17: monitorexit
      18: aload_1
      19: athrow
      20: return
    Exception table:
       from    to  target type
          10    12    15   any
          15    18    15   any
```

something in the depths of throwable wants to have a syncronised block (java.lang.Class.desiredAssertionStatus()). 
I want to just ignore the locking and treat it as normal code for now.. but it does weird things with the exception table for how to handle if you fail to take the lock which confuses my basic block finding. Even adding those as jump targets so it splits the blocks correctly, the handler tries to use things from the stack. The exception handler expects to have the exception object on the stack. Ok so again, temporary soulition, when emitting a block, if the start index is a handler in the exception table, push a null pointer to the initial stack. That represents the thrown exception and should be a valid object but for now since I just termninate the program on real exceptions and don't even check the lock in this case, it will never actually try to dereference the pointer. 

so now i can compile the sync blocks (assuming its single threaded so you never fail to take the lock) and maybe even very close to at least compiling a catch block. 


## instanceof (Nov 30)

similar to fcmp, it produces a value but you tend to always jump on it right after so seems convient to do a similar optimisation to avoid an extra branch. not because i think llvm wouldn't fix it but because adding basic blocks in my current system seems a bit annoying. will need to revisit because im sure you're allowed to actually use the value. 

actually never mind easiest starting way to do inheritance i think is have root vtable have a super vptr slot so you can use it like a linked list and see if you hit the one you're looking for. so ill make it an intrinsic function. 

## abstract methods (Nov 29)

need to emit `<clinit>` for static blocks even if never referenced (since it will never be) but alas now its actually trying to set all the random fields on Throwable which uses new constant types. For now try just doing them as null pointers. Then it tries to call abstractcollection.size which i can't do yet. Leave that test for now, need to do abstract methods soon anyway. 

trying to do abstract methods: 
out/test.ll:1416:41: error: base of getelementptr must be a pointer
  %vptr65 = load ptr, ptr addrspace(32) getelementptr inbounds (%TestObjs_A, i32 5, i32 0, i32 0), align 8

if you just use the obj ptr as the vtable field ptr it tries to read.... the number 5.... more accuritely it tries to read the argument im trying to pass to the function. i thought i was just confused trying to read the gep ir but yeah it was just doing something dumb. god dman it. abstract methods were in fact trivial i wasnt crazy just dumb. i guess i never tried to call a virtual method with an argument befoer and i was looking at the stack backwards as i often do. i remember writing a comment next to that to check if i had the order right and then deleting it cause hey it works probably fine. thats deeply frustraiting, should really not do this while tired. 
just to reasure myself im not out of a job, chatgpt doesnt catch that until i point out the problem to it `https://chat.openai.com/share/b59bd1af-ff19-4100-843a-64a41e354cc5`, still kinda impressive tho. 

## ternary operator (Nov 29)

seems to always have the form 

```
14: if_icmple     21
17: iload_0
18: goto          22
21: iload_1
```

actually not quite that simple, its allowed to do extra work in each branch. 
but the structure of branching to different blocks and each block leaving one thing on the stack seems to persist. 

## string constants & throw (Nov 29) 

String constants need to be an instance of the String class so I need to do a kinda weird switchero where I make an object for any strings you need. In llvm I can emit a string constant that's just a byte array and then need to put that in a global so i have a `&[u8]` instead of a `[u8]`. Then need to make a char array out of that. So write a little rt_intrinsic that loops over an empty char array and fills in bytes from a `char*`. The whole thing where java chars are officially a u16 but they maybe do string compaction and store strings as ascii when possible is gonna be scary. Now I can't actually call the string constructor because they use an empty string literal in there which is kinda funny (even the one that takes a char array has a special case if length is zero). 

And I don't have exceptions yet so I can't call most methods. They also use `?` everywhere and many methods take a CharSequence but I don't have interfaces yet. So I technically have strings but it seems `length()` is the only method you can actually call. 

Right now you get a new object each time a string constant is evaluated. Need to move that into a static block or something so strings with the same value reuse the objects since they're immutable. 

Implimented very primitive fatal exceptions so i can at least compile `throw` (no try/catch yet), just call an intrinsic to log a message and exit the program. That's enough to call into libraries that use exceptions as panics (not as control flow). When it throws the bytecode constructs an instance of Throwable and then does an opthrow on it. I don't have runtime typeinfo yet so my message just logs the address of the struct which is not super useful. Alas that means now i reference throwable which is a whole can of worms it turns out. 

## Dynamic Dispatch (Nov 28)

For real dynamic dispatch, every object needs to start with a vtable. Struct of function pointers for function should be used for each method on this specific instance. 

Problematic: you want to making the vtable at the beginning but you don't know which are going to be called yet. 
There are some methods I choke on but don't call. So need another level of queuing where its not referenced yet but has reserved an llvalue so if it is referenced, the function will be emitted into the place already referenced by the vtable. 

llvm type checking is very sad day. was just getting bus error for a while because trying to read from vtable as object type? actually maybe that didnt fix. AAAASSDASDF. value first! `build_store vptr_base vptr_field ctx.builder` NOT `build_store vptr_field vptr_base ctx.builder`

There's a speed/space trade off: do you make a chain of vtables that point to each other or does each subclass get a whole new one? Its such tiny space tho.


for inheriting vtable, there's no const_load so need to keep around both the value and the pointer to the global containing the value. 

long time spent on i was emitting unreachable for a method called virtually because it got seen by the vtable thing first so it declared it but it never saw it actually get called. TODO: make it an assertion not evil llvm magic whatever it wants unreachable. which.... means it is infact calling the right method. so i do infact have working vtables, i just also have dead code elimination that fucks them up. 


## Fields & Inheritance (Nov 28)

llvm tracks struct fields by index so just keep track of the (arbitrary) order I asked for. 
Then can gep to trade a struct pointer and index for a field pointer. 

For inheritance, make a fake __parent field that's directly the super class instead of just a pointer to one. 
The classfile's field list doesn't include inherited ones so that works out, each struct in the chain just adds the new stuff. To look up a field, you try to find it in your class. If its there, great, you're the first in the chain just do the thing. If its not there, access your magic parent field and then try to lookup the field on that object instead. Javac gives well typed class files so it should always work out. 

Calling a method that IS NOT final but the class IS final, is also easy. You know the object you're looking at must be exactly that class (with nothing further down the chain) so can do static dispatch. Do have to carefully check up the chain since this lets you call a nonfinal on someone above you if you're final. 
When a class overrides a method, it shows up in both class files, so the first one you see as you walk up is the most overriden one that should be used. 
An obvious step would be not just relying on the final marker and just look at the classes to see if its actually used as final in that program but that sounds like a painful graph search thing where now every method call requires looking at all other code. Need to prescan everything and record for each method at what level its treated as final. 

## Objects (Nov 27)

The natural trick to representing inheritance is just having the first field be the super class, same as how im doing arrays. 
But having the objptr type be some real type instead of a void* means you get `warning: incompatible pointer types` for upcasting 

Think this is the point where it makes sense to do a little name mangling to avoid conflicts. Just prefix the package and class name on every function. Its a little sad tho cause it makes the test program more verbose. 
I'm using underscores instead of dots in package because I want to be able to refer to them from c but it means they could still technically overlap like `a.b.c.java` and `a.b_c.java` both use `a_b_c` as thier prefix. 

Feels important to only try to do the methods you actually call because there's probably a lot of stuff you'd pull in by looking at the whole tree that I'd choke on. Like Object has syncronisation stuff that would pull in threads maybe.

The GenericMapSig in javalib would be so convient but it doesnt have an optional get method? 

Seems like there's an awkwardness where a class method signeture doesn't include the this pointer for non-static methods. 
Current solution is carefully adding it everywhere I was converting a java method signeture to an llvm function signeture. 
Just treat an instance method as a static method taking `this` as the first argument for now. 
Soon instance methods will need to go in a vtable for dynamic dispatch but I want to start with OpInvokeSpecial. 
That's what's used to call the constructor, super constructor, etc, where you want to specify a specific level of the inheritance hiarchy to target. I don't even have fields yet so maybe just pretend an object is a heap allocated integer as a first step. 

struct_type doesn't take a name and each reference seems to appear as a seperate thing in the ir. 
Instead can use named_struct_type which takes a string but no type array and then call struct_set_body on it with the field types. 
It also takes a boolean but no idea what it does. Difference in ir is, 
true: `%TestObjs = type <{ i32 }>` OR false: `%TestObjs = type { i32 }`
Apparently its ispacked. Why I need to google that instead of it being in llvm.ml who can say. 
- https://llvm.moe/ocaml/Llvm.html
Also it seems llvm cares not for the field order which is gonna get a bit scary trying to have it interface with my c stuff. 

Could have a compiler flag for release builds that treat `@NonNull` as an assertion and emit as a poison value? 
There's also https://llvm.org/docs/FaultMaps.html which looks interesting. 

After a thousand years of trying to make link time optimisation work im reminded that llvm is hateful. 
There's some weird fusion of clang and xcode that makes it very hard to make a newer version work maybe? 
or im just dumb idk. 

## Thinking about tests

stack_comptime_safe: 
(* TODO: Should go in test exe instead but that commits to splitting most into lib. 
         Also this is the kind of tiny test I dislike (tightly coupled to implementation). *)
(* TODO: actually these aren't even testing what I mean to, just the `&& stack == 0` at the very end.
         since you look at each block individually, the first one that leaves info on the stack to pass to others is illegal 
         so the check is doing a bunch of extra work for no reason.
         my rule's not just that forks can't change the stack, its that the entry can't either.
         the compiler doesn't save the stack before branching away so the real limitation is they can't modify it
         but ive implemented that as forcing it to be empty when you jump away *)

inplace_stack_change:
(* TODO: This is still wrong. An op that pops 2 then pushes 3 mutates the stack. Maybe no instruction does that? 
         Should split op_stack_delta into op_count_pops and op_count_pushes so this becomes obvious.
         More importantly, I have a stricter restriction so this is unnecessary *)


Kinda cool that you can use `(func_name [@tailcall]) args` to assert that a recursive function uses constant stack space. 

## Why C

- clang knows it so can just run it on both runtime and generated ir together and not depend on yet another compiler. 
- pretty much anything else would at the very least make me deal with someone else's name mangling
    - c++ templates don't have a stable ABI (and c++ doesn't spark joy either)
    - zig comptime doesn't have a stable ABI 
        - tho maybe use it to generate @export c functions
    - rust generics don't have a stable ABI

## More Arrays (Nov 25)

Something fucked up where it works if you declare the accumulator at the top but not between loops. 
It's trying to read var2_acc for the for loop counter instead of var4_i?
Maybe I messed up the indexing of locals? But if I don't do doubles, same problem with floats which only take one slot. 
Maybe its not stack_comptime_safe and my check is only detecting stack height changes but some instructions swap so if 
you swap the top of the stack and then jump away you've perceived it even if it doesn't think you did. 
Nope not that. 
Looking in javap, its not making enough locals. L0 is the argument, L1 is the array, L2 is the for loop counter and then also L2 is acc.  
It works if you declare the loop counter as a variable before the loop initializer, then it gets its own slot. 
It must be reusing the slot because thier lifetimes don't overlap. The loop counter ends when at the end of the loop's scope. 
So then do I have to key local variables by index and type? OpIInc doesn't have a type, hopefully only used on ints? 

Now instead of tracking (local_index -> llvalue), have ((local_index, local_type) -> llvalue) and hope that javac made everything work out so they don't overlap and it can't tell they're actually in different places and then that llvm puts them back together to the same place in the end if that's actually the right choice. 

Did some cleaning up of how the intrinsic calls are represented in the compiler to make it easier to add new ones. 
Discovered it works if you init the type of array, need to add a test that fails. I guess initing too big is fine and initing too small it just writes off the end and im not running with address sanitizer so it just happened to work out. 

Multidimensional arrays have thier own opcode instead of just generating a loop in bytecode (or even a function call to some builtin thing). Seems a little weird to me, like why is that such a core operation you'll be doing so often that it needs its own opcode.

## Arrays (Nov 24)

I can't decide if its better to just generate ir directly for array operations OR write some functions in c and just call them as needed. 
And then I don't know if its better to have one array type that just has a `char*` and then dynamically do the right thing since you know the size of the type when you init/load/store OR use a macro as a template to generate new types and functions for each primitive type. 
Doing the macro thing would probably make it less of a pain to consume from other native code if you want that to interact with your newly compiled java program which seems like an interesting usecase. 

Really annoying like 40 lines of setting up the function forward declarations in llvm for all the types of array. 
Now a bunch of tedius calling the functions. There's just so many stupid conversions. 
OpNewArray's arg is a value_type (which I thought they'd all be so used to key my map) but 
OpArrayStore's arg is a jvm_array_type which is the same but all object types are uniform and bool=byte. 
That's really a pattern with the library I'm using for the parsing bytecode. 
They want to be all type safe and correct in reflecting the jvm spec but it means I have to do so many inane type conversions for slightly different representations. 
Pleasingly the argument order on the stack is the intuitive one I assumed for my functions before I looked. 
Should lazily forward declare the array functions only if they're actually called cause its annoying to see them all at the top of the ir. 

## More Tests (Nov 24)

It seems more reasonable to write my test runner thing in ocaml than in c but its strangely awkward. 
Running `dune test` puts you in a weird working directory so have to `Unix.chdir "../../../";`. 
It also locks the `_build` directory so can't `dune build` from the test program. That's fair enough, it expects the tested lib to be a dependency of the test runner not invoked as a seperate executable. 
It also doesn't run if the test file hasn't changed so would need to figure out how to mark the java files as like resource dependencies or whatever they call it. 
Some libraries seem to come with the default installation (?) but you still need to ask for them in the dune file `(libraries unix str)`, fair enough. 
For now I'm just gonna use it as another binary target instead of the c one I had before. But you need to `dune build` and then run the exe directly. Need to revisit so it works with `dune test` eventually. 

Just to make sure nothing's going terribly wrong, I want to report the time for each command. `Sys.time` gives numbers that seem too low compared to what my c runner was saying. I wonder if "Return the processor time, in seconds, used by the program since the beginning of execution." means it doesn't include the time you're sleeping waiting for the command to finish. That seems a little weird, like how does it know, I guess the OS must know and probably lets you ask. On second thought, `time` command gives you the split so clearly a plausable task. `Unix.gettimeofday` seems to give more reasonable values. 

I want to be able to test the simple operations on all the primitive types without needing to write the same function 6 times. 
The only thing I can think of really it just java a template with placeholders and string replace each type name. 
Can have a macro in c to call them. Probably also need to let them know the size of the integer so it can check that overflows work properly. 
Then to make that actually useful need to have my compiler load multiple classes and prefix function names so they don't conflict. 

## Static fields (Nov 24)

Native method calls are trivial but need to revisit when I do name mangling. 

There's a FieldMap just like for methods. Can easily get the ones that are static. 
Trying to ive a static field a default value `not stack_comptime_safe in <clinit>`
Maybe that's a static block trying to set the field? Javap shows: 
```
static {};
    Code:
       0: iconst_0
       1: putstatic     #20                 // Field acc:I
       4: return
```
Oh, its just a problem with returning void because stack_delta doesn't check the type. 

Seems like they don't use the optional cf_value for the inititial value of the field. 
Instead it goes in a static block which shows up as a clinit function (class init?).
Segfault trying to set_initializer on the global. Use define_global instead of declare_global. 

This seems to work but now I need to call `@"<cl_init>"` which is awkward because c can't refer to that name. 
objdump does show it in the executable tho. 
For now just replace special characters in the function names with underscores but you loose the nice property of not being able to conflict with user defined methods. 
Now need to remember to call `_clinit_` manually for every class that has static fields which is a bit annoying. 
But convient that now I get `static { ... }` blocks working too for free. 

TODO: is there a way to indicate that a function argument is only an immutable view into a Hashtbl? 

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
Even writing a function that returns exactly the value of an fcmp doesn't just generate one fcmp so maybe its true. 
Maybe it even works out without messing with stack_delta because you just have it think OpIf and fcmp both consume 1 when really its OpIf consuming 2 when the previous was an fcmp. 

LLvm docs say "Ordered means that neither operand is a QNAN while unordered means that either operand may be a QNAN". 
Does that mean 
    - those are preconditions the instruction assumes OR
    - ordered alwasy returns false if there's a NAN and unordered always returns true if there's a NAN?
I hope the second one because then it matches with the bytecode fcmpg vs fcmpl. 

Also a little intimidating is it looks like the ternary operator pushes different things to the stack on each branch and then rejoins. Which it seems like my stack_comptime_safe isn't checking so have to do further invenstigation there 
once this works. Actually problem was previously assuming that Cmp was stack-2 but its -1 cause returns so now
at least stack_comptime_safe works and catches the problem early.  
Maybe tho it's still very limited and its only used as exactly a phi instruction so could 
recognise the pattern and do that. That's scary tho. 

Some of that casting is a bit weird, it seems the operand stack can't hold anything smaller than an int? 
So the down casting instructions say they truncate to the smaller size and then sign extend back up to an int. 
By extension, when you return a smaller type, the stack has it as an int. There are different return opcodes for int/float/double/long/void but all the smaller types are converted based on the function signeture "as though by the cast opcode". Which is weird because the java compiler doesn't let you return something that isn't already the right type so it will already be in that range. So idk if they're just being overly specific for no reason or there's some edge case i haven't thought of. 
- https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-6.html#jvms-6.5.i2b
- https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-6.html#jvms-6.5.ireturn

Problem with OpIf on smaller int types, it doesn't know the type to produce the zero constant for, just assumes 32 bits. Could do it like the spec where every value on the operand stack has to be a 32 bit integer. 
But that feels silly when you're doing math on the same type. But then there won't be explicit casts when working with different types because it expects them to just be integers. 
So maybe inheriting that restriction is easiest since the ISA has that anyway kinda. 
But really math stuff is probably specified to work as though it was all cast to int and then back only at the very end of the expression. 
Further thought required when I add tests for overflowing stuff. 

Had a problem with comparisons that i assumed was about sizes somehow but it was actually just that my 
int OpIf had the operands in the wrong order. Why didn't I notice before, was I just never comparing to zero? 
Yeah I did longs and doubles but those both use cmp then If so I was wrong about which argument the const zero replaces in the normal case I guess, idk. 

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

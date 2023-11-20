open Llvm
(* open Javalib_pack
open Javalib
open JBasics
open JCode *)

type codegen = {
    _context: llcontext;
    the_module: llmodule;
    _builder: llbuilder;
}

let () = 
    let c = create_context () in
    let ctx = { _context = c; the_module = create_module c "javatest"; _builder = builder c } in
    let code = string_of_llmodule ctx.the_module in
    print_endline "Hello, World!";
    print_endline code

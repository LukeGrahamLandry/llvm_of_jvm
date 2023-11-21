open Llvm
open Javalib_pack
open Javalib
open JBasics
open JCode
open Printf

exception Fail of string

type codegen = {
    context: llcontext;
    the_module: llmodule;
    builder: llbuilder;
}

let lltype_of_basic (ctx: codegen) (basic: java_basic_type) =
    (match basic with
        | `Int -> i32_type
        | `Short | `Char -> i16_type
        | `Byte -> i8_type
        | `Bool -> i1_type
        | `Long -> i64_type
        | `Float -> float_type
        | `Double -> double_type
        ) ctx.context

let lltype_of_jtype (ctx: codegen) (ty: value_type): lltype = 
    match ty with
    | TObject _ -> raise (Fail "TODO: TObject")
    | TBasic basic -> lltype_of_basic ctx basic


let llfunc_type (ctx: codegen) (sign: method_signature): lltype = 
    let arg_types = Array.of_list (List.map (lltype_of_jtype ctx) (ms_args sign)) in
    let ret = match ms_rtype sign with 
        | Some ty -> lltype_of_jtype ctx ty 
        | None -> void_type ctx.context 
    in
    function_type ret arg_types

(* ctx must already be pointing to the right basic block *)
let _stack_alloca (ctx: codegen) (ty: value_type): llvalue = 
    build_alloca (lltype_of_jtype ctx ty) "" ctx.builder

(* Find the indexes of opcodes that begin basic blocks. So which instructions are jump targets. *)
let find_basic_blocks (code: jopcode array): int list =
    fst (Array.fold_left
        (fun (blocks, index) op ->
            let new_blocks = match op with
            | OpIf (_, i) 
            | OpIfCmp (_, i) 
            | OpGoto i 
                -> index + i :: blocks
            | _ -> blocks in
            (new_blocks, index + 1)
        ) ([0], 0) code)

type blockmap = (int, llbasicblock) Hashtbl.t
let _init_basic_blocks (ctx: codegen) (func: llvalue) (blocks: int list): blockmap = 
    let pairs = List.map (fun index -> (index, (append_block ctx.context "" func))) blocks in
    Hashtbl.of_seq (List.to_seq pairs)


type localtypemap = (int, jvm_type) Hashtbl.t
let find_local_types (code: jopcode array): localtypemap =
    let locals = Hashtbl.create 0 in
    Array.iter (fun op -> 
        match op with 
        | OpLoad (ty, index) -> (Hashtbl.replace locals index ty)
        | OpStore (ty, index) -> (Hashtbl.replace locals index ty)
        | _ -> ()
    ) code;
    locals

let tblmap f m = Hashtbl.of_seq (Seq.map f (Hashtbl.to_seq m))


let lltype_of_jvmtype (ctx: codegen) (ty: jvm_type): lltype = 
    match ty with
    | `Object -> pointer_type ctx.context
    | `Int2Bool -> i32_type ctx.context  (* no destinction? *)
    | `Long -> i64_type ctx.context
    | `Float -> float_type ctx.context
    | `Double -> double_type ctx.context

(* Create the entry block before block 0 and emit alloca instructions for each local variable in the method *)
type localmap = (int, llvalue) Hashtbl.t (* local index -> stack pointer value *)
let alloc_locals (ctx: codegen) (func: llvalue) (code: jcode): localmap = 
    let types = find_local_types code.c_code in
    let entry = append_block ctx.context "entry" func in 
    position_at_end entry ctx.builder; 
    tblmap (fun (i, jvmtype) -> 
        (* let name = "var" ^ string_of_int i in *)
        let ptr = build_alloca (lltype_of_jvmtype ctx jvmtype) "" ctx.builder in
        (i, ptr)
    ) types

(* builder must be pointing at entry block already *)
type rtstack = { arr: llvalue; count: llvalue; arr_ty: lltype }

let stack_init (ctx: codegen) (code: jcode): rtstack = 
    let intty = i32_type ctx.context in
    let arr_ty = array_type intty code.c_max_stack in 
    let zero = const_int intty 0 in 
    let arr = build_array_alloca arr_ty zero "" ctx.builder in  (* idk what the 0 is *)
    let count = build_alloca intty "" ctx.builder in
    let _ = build_store zero count ctx.builder in 
    { arr; count; arr_ty }

let stack_push (ctx: codegen) (stack: rtstack) (v: llvalue) = 
    let intty = i32_type ctx.context in
    let old_count = build_load intty stack.count "" ctx.builder in
    let indices = Array.of_list [old_count] in
    let next_ptr = build_gep stack.arr_ty stack.arr indices "" ctx.builder in
    let _ = build_store v next_ptr ctx.builder in 
    let one = const_int intty 1 in
    let new_count = build_add old_count one "" ctx.builder in
    let _ = build_store new_count stack.count ctx.builder in 
    ()

let stack_pop (ctx: codegen) (stack: rtstack): llvalue = 
    let intty = i32_type ctx.context in
    let old_count = build_load intty stack.count "" ctx.builder in
    let indices = Array.of_list [old_count] in
    let next_ptr = build_gep stack.arr_ty stack.arr indices "" ctx.builder in
    let v = build_load intty next_ptr "" ctx.builder in 
    let one = const_int intty 1 in
    let new_count = build_sub old_count one "" ctx.builder in
    let _ = build_store new_count stack.count ctx.builder in 
    v

(* push to the stack *)
let push_load_local (ctx: codegen) (stack: rtstack) (i: int) (locals: localmap) = 
    let intty = i32_type ctx.context in
    let local_ptr = Hashtbl.find locals i in
    let local_val = build_load intty local_ptr "" ctx.builder in 
    stack_push ctx stack local_val

let pop_store_local (ctx: codegen) (stack: rtstack) (i: int) (locals: localmap) = 
    let local_ptr = Hashtbl.find locals i in
    let v = stack_pop ctx stack in
    let _ = build_store v local_ptr ctx.builder in 
    ()

let stack_add ctx stack = 
    let a = stack_pop ctx stack in
    let b = stack_pop ctx stack in
    let c = build_add a b "" ctx.builder in
    stack_push ctx stack c

let convert_method (ctx: codegen) (code: jcode) (func: llvalue) = 
    printf "max stack size: %d. max locals: %d\n"  code.c_max_stack code.c_max_locals;
    
    let block_positions = find_basic_blocks code.c_code in
    print_string "Basic block indices: ";
    List.iter (printf "%d, ") block_positions;
    (* let _blocks = init_basic_blocks ctx func block_positions in *)
    let locals = alloc_locals ctx func code in
    let working_stack = stack_init ctx code in
    let _ = Array.fold_left
    (fun index op ->
        let _ = match op with
        | OpLoad (_, i) -> push_load_local ctx working_stack i locals
        | OpStore (_, i) -> pop_store_local ctx working_stack i locals
        | OpAdd _ty -> stack_add ctx working_stack
        | OpReturn _ty -> let _ = build_ret (stack_pop ctx working_stack) ctx.builder in ()
        | _ -> () in

        printf "%d.  " index;
        print_endline (JPrint.jopcode op);
        index + 1
    ) 0 code.c_code in

    printf "\nlocal count: %d\n" (Hashtbl.length locals);
    print_endline "\n================";
    ()

let emit_method (ctx: codegen) (m: jcode jmethod) = 
    match m with
    | AbstractMethod _ -> raise (Fail "TODO: AbstractMethod")
    | ConcreteMethod func ->
        let llfunc = declare_function (ms_name func.cm_signature) (llfunc_type ctx func.cm_signature) ctx.the_module in
        (match func.cm_implementation with
            | Native -> ()
            | Java code ->
                let jcode = Lazy.force code in
                    convert_method ctx jcode llfunc
        ) 

let () = 
    let c = create_context () in
    let ctx = { context = c; the_module = create_module c "javatest"; builder = builder c } in
    
    print_endline "Hello, World!";
    let path = class_path "./java" in
    let cls = get_class path (make_cn "Hello") in
    let methods = get_methods cls in
    let _ = MethodMap.map (fun m ->
        print_endline "\n";
        print_endline (JPrint.method_signature (get_method_signature m));
        let _ = emit_method ctx m in
        m
        ) methods in
    print_endline "hi";
    let code = string_of_llmodule ctx.the_module in
    print_endline code;

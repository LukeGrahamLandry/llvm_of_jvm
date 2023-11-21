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
let init_basic_blocks (ctx: codegen) (func: llvalue) (blocks: int list): blockmap = 
    let pairs = List.map (fun index -> (index, (append_block ctx.context "" func))) blocks in
    Hashtbl.of_seq (List.to_seq pairs)

(* Create the entry block before block 0 and emit alloca instructions for each local variable in the method *)
type localmap = (int, llvalue) Hashtbl.t (* local index -> stack pointer value *)
let alloc_locals (ctx: codegen) (func: llvalue) (code: jcode): localmap = 
    let entry = append_block ctx.context "" func in 
    position_at_end entry ctx.builder; 
    let pairs = List.map (fun (_start_pc, _length, name, ty, index) ->
        let val_ty = ty in
        let ptr = build_alloca (lltype_of_jtype ctx val_ty) name ctx.builder in
        (index, ptr)
    ) (Option.get code.c_local_variable_table) in
    Hashtbl.of_seq (List.to_seq pairs)





    

let convert_method (ctx: codegen) (code: jcode) (func: llvalue) = 
    printf "max stack size: %d. max locals: %d\n"  code.c_max_stack code.c_max_locals;
    let _ = Array.fold_left
        (fun index op ->
            printf "%d.  " index;
            print_endline (JPrint.jopcode op);
            index + 1
        ) 0 code.c_code in
    let block_positions = find_basic_blocks code.c_code in
    print_string "Basic block indices: ";
    List.iter (printf "%d, ") block_positions;
    let _blocks = init_basic_blocks ctx func block_positions in
    let _locals = alloc_locals ctx func code in
    
    (* Hashtbl.iter (fun index bb -> printf "%d %d" index (block_address func bb)) blocks; *)
    print_endline "\n================";
    ()

let emit_method (ctx: codegen) (m: jcode jmethod) = 
    match m with
    | AbstractMethod _ -> raise (Fail "TODO: AbstractMethod")
    | ConcreteMethod func ->
        let llfunc = define_function (ms_name func.cm_signature) (llfunc_type ctx func.cm_signature) ctx.the_module in
        let bb = append_block ctx.context "" llfunc in
        position_at_end bb ctx.builder;
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

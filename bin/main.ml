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

let lltype_of_jtype (ctx: codegen) (ty: value_type): lltype = 
    match ty with
    | TObject _ -> raise (Fail "TODO: TObject")
    | TBasic basic -> 
        match basic with
        | `Int -> i32_type ctx.context
        | _ -> raise (Fail "non-int TBasic")

let llfunc_type (ctx: codegen) (sign: method_signature): lltype = 
    let arg_types = Array.of_list (List.map (lltype_of_jtype ctx) (ms_args sign)) in
    let ret = match ms_rtype sign with 
        | Some ty -> lltype_of_jtype ctx ty 
        | None -> void_type ctx.context 
    in
    function_type ret arg_types

let _stack_alloca (ctx: codegen) (ty: value_type): llvalue = 
    build_alloca (lltype_of_jtype ctx ty) "" ctx.builder

let convert_method (_ctx: codegen) (code: jcode) = 
    printf "max stack size: %d. max locals: %d\n "  code.c_max_stack code.c_max_locals;
    let _ = Array.fold_left
        (fun _s op ->
            print_endline (JPrint.jopcode op);
            ()
        ) () code.c_code in
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
                    convert_method ctx jcode
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

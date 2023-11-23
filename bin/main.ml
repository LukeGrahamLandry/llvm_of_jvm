open Llvm
open Javalib_pack
open Javalib
open JBasics
open JCode
(* open Printf *)

exception Fail of string

type codegen = {
    context: llcontext;
    the_module: llmodule;
    builder: llbuilder;
}

let lltype_of_basic ctx (basic: java_basic_type) =
    (match basic with
        | `Int -> i32_type
        | `Short | `Char -> i16_type
        | `Byte -> i8_type
        | `Bool -> i1_type
        | `Long -> i64_type
        | `Float -> float_type
        | `Double -> double_type
        ) ctx.context

let lltype_of_valuetype ctx ty = 
    match ty with
    | TObject _ -> raise (Fail "TODO: TObject")
    | TBasic basic -> lltype_of_basic ctx basic

let slotcount_for ty = 
    match ty with
    | TObject _ -> 1
    | TBasic basic -> match basic with
        | `Int | `Short | `Char| `Byte | `Bool | `Float -> 1
        | `Long | `Double -> 2

let llfunc_type ctx sign = 
    let arg_types = Array.of_list (List.map (lltype_of_valuetype ctx) (ms_args sign)) in
    let ret = match ms_rtype sign with 
        | Some ty -> lltype_of_valuetype ctx ty 
        | None -> void_type ctx.context 
    in
    function_type ret arg_types

let remove_duplicates l = 
    List.fold_left (fun ll next -> 
        if List.exists ((=) next) ll then ll else next :: ll
    ) [] l
    
(* Find the indexes of opcodes that begin basic blocks. So which instructions are jump targets. *)
let find_basic_blocks (code: jopcode array): int list =
    remove_duplicates (fst (Array.fold_left
        (fun (blocks, index) op ->
            let new_blocks = match op with
            | OpIf (_, i) 
            | OpIfCmp (_, i) (* conditional jump can fall through. represent that as jumping to the next instruction *)
                -> (index + 1) :: (index + i) :: blocks
            | OpGoto i 
                -> index + i :: blocks
            | _ -> blocks in
            (new_blocks, index + 1)
        ) ([0], 0) code))

type blockmap = (int, llbasicblock) Hashtbl.t

let init_basic_blocks ctx func (blocks: int list): blockmap = 
    let pairs = List.map (fun index -> (index, (append_block ctx.context ("op" ^ string_of_int index) func))) blocks in
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

let count_local_stores (code: jopcode array) =
    let locals = Hashtbl.create 0 in
    Array.iter (fun op -> 
        match op with 
        | OpStore (_, index) | OpIInc (index, _) -> 
            let prev = Option.default 0 (Hashtbl.find_opt locals index) in
            (Hashtbl.replace locals index (prev + 1))
        | _ -> ()
    ) code;
    locals

let tblmap f m = Hashtbl.of_seq (Seq.map f (Hashtbl.to_seq m))

let lltype_of_jvmtype ctx (ty: jvm_type) = 
    match ty with
    | `Object -> pointer_type ctx.context
    | `Int2Bool -> i32_type ctx.context  (* no destinction? *)
    | `Long -> i64_type ctx.context
    | `Float -> float_type ctx.context
    | `Double -> double_type ctx.context

(* A function argument can be reassigned like a local but if it isn't, don't ask for a stack slot *)
type argplace = 
    FinalArg of llvalue (* direct value. must never be stored to. *) 
    | Mut of llvalue (* pointer to stack slot*)
type localinfo = { place: argplace; ty: lltype; }
type localmap = (int, localinfo) Hashtbl.t

let args_slotcount sign = 
    List.fold_left (fun acc ty -> acc + (slotcount_for ty)) 0 (ms_args sign)
    
(* Returns map of local indexes for the function arguments to thier llvm ssa. 
   ie. f(int, long, int) -> {0=%0, 1=%1, 3=%2} because longs take two slots *)
let find_arg_locals func sign = 
    let largs = Array.to_list (params func) in
    let jargs = ms_args sign in
    let pairs = List.rev (fst (List.fold_left2 (fun (l, off) jty ssa -> 
        ((off, ssa) :: l, off + (slotcount_for jty))
    ) ([], 0) jargs largs)) in
    Hashtbl.of_seq (List.to_seq pairs)

(* Create the entry block before block 0 and emit alloca instructions for each local variable in the method 
   Also copy mutable arguments to thier stack slot. *)
let alloc_locals ctx func code sign: localmap = 
    let arg_locals = find_arg_locals func sign in
    let types = find_local_types code.c_code in
    let stores = count_local_stores code.c_code in
    let entry = append_block ctx.context "entry" func in 
    position_at_end entry ctx.builder; 
    tblmap (fun (i, jvmtype) -> 
        let ty = lltype_of_jvmtype ctx jvmtype in
        let stores = Option.default 0 (Hashtbl.find_opt stores i) in
        let place = if stores > 0 then 
            let name = "var" ^ string_of_int i in
            let ptr = build_alloca ty name ctx.builder in
            (match Hashtbl.find_opt arg_locals i with
            | Some ssa -> (* its an argument. copy it to its stack slot*)
                let _ = build_store ssa ptr ctx.builder in 
                ()
            | None -> () (* it's not an argument. no initial value. (TODO: zero?)*)
            );
            Mut ptr
        else (* if it's never stored, it must be a function argument *)
            let ssa = Hashtbl.find arg_locals i in
            FinalArg ssa 
        in
        (i, { place; ty; })
    ) types


let op_stack_delta op = 
    match op with
    | (OpAdd _) | (OpSub _) | (OpMult _) | (OpDiv _) -> -1
    | OpStore _ -> -1
    | (OpLoad _) | (OpConst _) -> 1
    | OpReturn _ -> -1
    | (OpCmp _) -> -1 (* fcmp takes two but returns a value *)
    | (OpIfCmp _) -> -2
    | OpIf _ -> -1 (* comparing to zero *)
    | (OpIInc _) -> 0
    | OpInvoke ((`Static _), sign) -> 
        let ret = if (ms_rtype sign) == None then 0 else 1 in
        ret - args_slotcount sign
    | OpInvalid -> 0
    | _ -> raise (Fail ("TODO: stack_delta " ^ (JPrint.jopcode op)))

(* [a; b; c; d] -> [(a, b); (b, c); (c; d)]*)
let rec sliding_pairs (l: 'a list) =
    match l with
    | a :: [b] -> [(a, b)]
    | a :: (b :: rest) -> (a, b) :: (sliding_pairs rest)
    | _ -> []

(* It seems javac doesn't generate code that would return false here which is very convient. *)
let stack_comptime_safe (block_starts: int list) (code: jopcode array) =
    let starts = List.sort compare block_starts in
    let block_ranges = sliding_pairs (List.concat [starts; [Array.length code]]) in
    let block_comptime_safe start last =
        let (_, res, stack) = Array.fold_left (fun (index, safe, stack) op -> 
            if ((index < start) || (index >= last)) then (index, safe, stack) else 
            let new_stack = stack + op_stack_delta op in
            (index + 1, (safe && new_stack >= 0), new_stack)
        ) (0, true, 0) code in
        res && stack == 0
    in
    List.fold_left (fun prev (start, last) -> prev && (block_comptime_safe start last)) (true) block_ranges 

let load_local ctx i (locals: localmap) = 
    let local = Hashtbl.find locals i in
    match local.place with
    | Mut ptr -> build_load local.ty ptr "" ctx.builder
    | FinalArg v -> v

let store_local ctx i (locals: localmap) v = 
    let local_ptr = match (Hashtbl.find locals i).place with
        | Mut ptr -> ptr
        | (FinalArg _) -> raise (Fail "try store FinalArg local") in
    let _ = build_store v local_ptr ctx.builder in 
    ()

let drop_fst l = 
    match l with 
    | [] -> raise (Fail "drop_fst empty")
    | _ :: rest -> rest
    
let bin_op (f: llvalue -> llvalue -> llvalue) compstack = 
    let a = List.nth compstack 0 in
    let b = List.nth compstack 1 in
    (* Argument order matters for subtraction and division *)
    (f b a) :: (drop_fst (drop_fst compstack))

let intcmp kind: Llvm.Icmp.t = 
    match kind with
    | `IEq | `Eq -> Eq
    | `INe | `Ne -> Ne
    | `ILt | `Lt -> Slt
    | `IGe | `Ge -> Sge
    | `IGt | `Gt -> Sgt
    | `ILe | `Le -> Sle
    | `AEq | `ANe | `Null | `NonNull -> raise (Fail "TODO: cmp ptr")

let emit_const ctx (v: jconst): llvalue = 
    match v with
    | `Int n -> const_int (i32_type ctx.context) (Int32.to_int n)
    | (`Byte n) | (`Short n) -> const_int (i32_type ctx.context) n
    | _ -> raise (Fail "TODO: emit_const non-int")

let assert_empty l = if not (List.length l == 0) then raise (Fail "Expected list to be empty") else l

let is_float ty = 
    match ty with
    | `Int2Bool | `Long -> false
    | `Float | `Double -> true

let int_or_float ty if_int if_float = 
    if is_float ty then if_float else if_int

let convert_method ctx code current_sign funcmap = 
    let func = MethodMap.find current_sign funcmap in (* bro why did you make it (key, map) instead of (map, key)*)
    
    let block_positions = find_basic_blocks code.c_code in
    if not (stack_comptime_safe block_positions code.c_code)
        then raise (Fail ("TODO: use rt stack. not stack_comptime_safe in " ^ (ms_name current_sign)));

    let locals = alloc_locals ctx func code current_sign in
    let basic_blocks = init_basic_blocks ctx func block_positions in
    let first = Hashtbl.find basic_blocks 0 in
    let _ = build_br first ctx.builder in (* jump from stack setup to first instruction *)

    let emit_op ctx (compstack: llvalue list) op index: llvalue list = 
        (match Hashtbl.find_opt basic_blocks index with
        | Some bb -> 
            let current = insertion_block ctx.builder in
            (match block_terminator current with 
            | Some _ -> () (* last instruction was a jump *)
            | None -> 
                let _ = assert_empty compstack in
                let _ = build_br bb ctx.builder in ()); (* this is the head of a loop so fallthrough *)
            position_at_end bb ctx.builder (* either way start writing the new block*)
        | None -> () (* continue the previous block*)
        );

        let do_bin f = bin_op (fun a b -> f a b "" ctx.builder) compstack in 
        match op with
        | OpLoad (_, i) -> load_local ctx i locals :: compstack
        | OpStore (_, i) -> 
            store_local ctx i locals (List.hd compstack);
            drop_fst compstack
        | OpAdd ty -> do_bin (int_or_float ty build_add build_fadd)
        | OpSub ty -> do_bin (int_or_float ty build_sub build_fsub) 
        | OpMult ty -> do_bin (int_or_float ty build_mul build_fmul) 
        | OpDiv ty -> do_bin (int_or_float ty build_sdiv build_fdiv) 
        | OpIfCmp (kind, offset) -> 
            let true_block = Hashtbl.find basic_blocks (index + offset) in
            let false_block = Hashtbl.find basic_blocks (index + 1) in
            let a = List.nth compstack 0 in
            let b = List.nth compstack 1 in
            let c = build_icmp (intcmp kind) b a "" ctx.builder in
            let _ = build_cond_br c true_block false_block ctx.builder in
            assert_empty (drop_fst (drop_fst compstack))
        | OpIf (kind, offset) -> 
            let true_block = Hashtbl.find basic_blocks (index + offset) in
            let false_block = Hashtbl.find basic_blocks (index + 1) in
            let a = List.nth compstack 0 in
            let b = const_int (i32_type ctx.context) 0 in
            let c = build_icmp (intcmp kind) b a "" ctx.builder in
            let _ = build_cond_br c true_block false_block ctx.builder in
            assert_empty (drop_fst compstack)
        | OpGoto offset -> 
            let bb = Hashtbl.find basic_blocks (index + offset) in
            let _ = build_br bb ctx.builder in
            assert_empty compstack
        | OpReturn _ty -> 
            let _ = build_ret (List.hd compstack) ctx.builder in
            (* This will always work because of stack_comptime_safe*)
            assert_empty (drop_fst compstack)
        | OpConst v -> (emit_const ctx v) :: compstack
        | OpIInc (local_index, v) -> 
            let old_val = load_local ctx local_index locals in
            let inc = const_int (i32_type ctx.context) v in
            let new_val = build_add old_val inc "" ctx.builder in
            store_local ctx local_index locals new_val;
            compstack

        | OpInvoke ((`Static (_ioc, _classname)), target_sign) -> 
            if List.length (ms_args target_sign) > List.length compstack then raise (Fail ("stack underflow calling " ^ (ms_name target_sign)));
            let func_ty = llfunc_type ctx target_sign in
            let func_value = MethodMap.find target_sign funcmap in 
            let (new_stack, arg_values) = List.fold_left (fun (stack, args) _arg -> 
                (drop_fst stack, (List.nth stack 0) :: args)
                ) (compstack, []) (ms_args target_sign) in
            let result = build_call func_ty func_value (Array.of_list arg_values) "" ctx.builder in
            if (ms_rtype target_sign) == None then new_stack else (result :: new_stack)
            
        | OpInvalid -> compstack (* these slots are the arguments to previous instruction *)
        | _ -> raise (Fail ("TODO: emit_op " ^ JPrint.jopcode op))

        in
    
    let _ = Array.fold_left
    (fun (stack, index) op ->
        let new_stack = emit_op ctx stack op index in
        (new_stack, index + 1)
    ) ([], 0) code.c_code in

    ()

let emit_method ctx m funcmap = 
    match m with
    | AbstractMethod _ -> raise (Fail "TODO: AbstractMethod")
    | ConcreteMethod func ->
        (match func.cm_implementation with
            | Native -> ()
            | Java code ->
                let jcode = Lazy.force code in
                    convert_method ctx jcode func.cm_signature funcmap
        )

let forward_declare_method ctx m = 
    match m with
    | AbstractMethod _ -> raise (Fail "TODO: AbstractMethod")
    | ConcreteMethod func ->
        let sign = func.cm_signature in
        let func = match func.cm_implementation with
            | Native -> define_function (ms_name sign) (llfunc_type ctx sign) ctx.the_module 
            | Java _ -> declare_function (ms_name sign) (llfunc_type ctx sign) ctx.the_module 
            in
        func
       
let () = 
    let c = create_context () in
    let ctx = { context = c; the_module = create_module c "javatest"; builder = builder c } in
    
    let path = class_path "./java" in
    let cls = get_class path (make_cn "OpTest") in
    let methods = get_methods cls in
    let funcmap = MethodMap.map (forward_declare_method ctx) methods in

    let _ = MethodMap.map (fun m ->
        let name = ms_name (get_method_signature m) in
        if not (name = "<init>") then 
            let _ = emit_method ctx m funcmap in
            ()
        ) methods in
    let code = string_of_llmodule ctx.the_module in
    print_endline code;

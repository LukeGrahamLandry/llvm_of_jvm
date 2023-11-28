open Llvm
open Javalib_pack
open Javalib
open JBasics
open JCode
open Printf

(* This kinda represents permission to do imperative stuff with the llvm api. *)
type codegen = {
    context: llcontext;
    the_module: llmodule;
    builder: llbuilder;
}

(*== Semantic messages for exceptions that should not be caught ==*)
(* `export OCAMLRUNPARAM=b` in shell makes it print a stack trace if it hits an assertion *)
exception Panic of string
let todo msg = raise (Panic ("NOT YET IMPLEMENTED: " ^ msg))
let illegal msg = raise (Panic ("ILLEGAL INPUT: " ^ msg))
let unreachable () = raise (Panic "UNREACHABLE") (* needs to take an argument, otherwise its evaluated as an entry point???? *)

(*== Little utilities for working with lists ==*)

let tblmap f m = Hashtbl.of_seq (Seq.map f (Hashtbl.to_seq m))
let pre (x, xs) = x :: xs
let assert_empty l = if not (List.length l == 0) then illegal "Expected list to be empty" else l
let replace_chars s before after = String.map (fun c -> if String.contains before c then after else c) s

(* [a; b; c; d] -> [(a, b); (b, c); (c; d)]*)
let rec sliding_pairs l =
    match l with
    | a :: [b] -> [(a, b)]
    | a :: (b :: rest) -> (a, b) :: (sliding_pairs rest)
    | _ -> []

let drop_fst l = 
    match l with 
    | [] -> illegal "drop_fst empty"
    | _ :: rest -> rest

(* pop_n [1; 2; 3; 4; 5] 2 = ([2; 1], [3; 4; 5])*)
let pop_n l n = 
    List.fold_left (fun (remaining, taken) _ -> 
    (drop_fst remaining, (List.hd remaining) :: taken)
    ) (l, []) (List.init n (fun _ -> ()))


(* Choose an arbitrary entry from a hash table. *)
let take_any m = 
    match Hashtbl.to_seq m () with
    | Nil -> None
    | Cons (d, _) -> Some d

(*== Wrap functions that use exceptions ==*)

let get_method_opt cls target_sign = 
    try Some (get_method cls target_sign) with 
    Not_found -> None

(*== Conversion between different type representations ==*)
(* TODO: is there any way to deal with overlaping enums in a less painful way? *)

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
    | TObject _ -> pointer_type ctx.context
    | TBasic basic -> lltype_of_basic ctx basic

let lltype_of_arrtype ctx ty = 
    (match ty with
    | `Int -> i32_type
    | `Short | `Char -> i16_type
    | `ByteBool -> i8_type
    | `Long -> i64_type
    | `Float -> float_type
    | `Double -> double_type
    | `Object -> pointer_type) ctx.context

let lltype_of_jvmtype ctx (ty: jvm_type) = 
    match ty with
    | `Object -> pointer_type ctx.context
    | `Int2Bool -> i32_type ctx.context  (* no destinction? *)
    | `Long -> i64_type ctx.context
    | `Float -> float_type ctx.context
    | `Double -> double_type ctx.context

let arraytype_of_basictype (ty: java_basic_type): jvm_array_type = 
    match ty with
    | `Byte | `Bool -> `ByteBool
    | `Int -> `Int
    | `Short -> `Short
    | `Char -> `Char
    | `Float -> `Float
    | `Long -> `Long
    | `Double -> `Double

let arraytype_of_valuetype ty = 
    match ty with
    | TObject _ -> `Object
    | TBasic basic -> arraytype_of_basictype basic

let is_float ty = 
    match ty with
    | `Int2Bool | `Long -> false
    | `Float | `Double -> true

let int_or_float ty if_int if_float = 
    if is_float ty then if_float else if_int

let rec inner_array_type (obj: object_type): jvm_array_type = 
    match obj with
    | TClass _ -> `Object
    | TArray (TBasic b) -> arraytype_of_basictype b
    | TArray (TObject obj2) -> inner_array_type obj2

let ctype_name (ty: jvm_array_type) = 
    match ty with
    | `Int -> "i32"
    | `Short -> "i16"
    | `Char -> "u16"
    | `ByteBool -> "i8"
    | `Long -> "i64"
    | `Float -> "f32"
    | `Double -> "f64"
    | `Object -> "objptr"

let intcmp kind: Llvm.Icmp.t = 
    match kind with
    | `IEq | `Eq -> Eq
    | `INe | `Ne -> Ne
    | `ILt | `Lt -> Slt
    | `IGe | `Ge -> Sge
    | `IGt | `Gt -> Sgt
    | `ILe | `Le -> Sle
    | `AEq | `ANe | `Null | `NonNull -> todo "cmp ptr"

let fcmp_flag fcmpkind icmpkind: Llvm.Fcmp.t = 
    match fcmpkind with
    | `L -> unreachable ()
    | `FL | `DL -> 
        (match icmpkind with
        | `Eq -> Oeq
        | `Ne -> One
        | `Lt -> Olt
        | `Ge -> Oge 
        | `Gt -> Ogt
        | `Le -> Ole
        | `Null | `NonNull -> illegal "fcmp Null/NonNull")
    | `FG | `DG -> 
        (match icmpkind with
        | `Eq -> Ueq
        | `Ne -> Une
        | `Lt -> Ult
        | `Ge -> Uge 
        | `Gt -> Ugt
        | `Le -> Ule
        | `Null | `NonNull -> illegal "fcmp Null/NonNull")

let llfunc_type ctx sign is_static = 
    let arg_types = List.map (lltype_of_valuetype ctx) (ms_args sign) in
    let arg_types = if is_static then arg_types else (pointer_type ctx.context) :: arg_types in
    let arg_types = Array.of_list arg_types in
    let ret = match ms_rtype sign with 
        | Some ty -> lltype_of_valuetype ctx ty 
        | None -> void_type ctx.context 
    in
    function_type ret arg_types

(*== Hardcoded knowledge about the bytecode format ==*)

let slotcount_for ty = 
    match ty with
    | TObject _ -> 1
    | TBasic basic -> match basic with
        | `Int | `Short | `Char | `Byte | `Bool | `Float -> 1
        | `Long | `Double -> 2

(* Result is not in jvm index units. All types take one slot. *)
let rec op_stack_delta op = 
    match op with
    | (OpArrayStore _) -> -3
    | (OpIfCmp _) | (OpPutField _) -> -2

    | (OpAdd _) | (OpSub _) | (OpMult _) | (OpDiv _)| (OpRem _)
    | (OpStore _) | (OpPutStatic _) 
    | (OpCmp _) (* fcmp takes two but returns a value *)
    | OpIAnd | OpLAnd | OpIOr | OpLOr | OpIXor | OpLXor | OpIUShr | OpLUShr | OpIShr | OpLShr | OpIShl | OpLShl
    | (OpArrayLoad _)
    | OpIf _ (* comparing to zero *)
    -> -1

    | OpI2L | OpI2F | OpI2D | OpL2I | OpL2F | OpL2D | OpF2I | OpF2L | OpF2D | OpD2I | OpD2L | OpD2F | OpI2B | OpI2C | OpI2S 
    | (OpNeg _) | (OpIInc _) 
    | OpGoto _
    | OpInvalid
    | (OpNewArray _) | OpArrayLength | (OpGetField _)
    -> 0

    | (OpLoad _) | (OpConst _) | (OpGetStatic _) | (OpNew _) | OpDup -> 1

    | OpReturn ty -> if ty == `Void then 0 else -1
    | OpInvoke ((`Static _), sign) -> 
        let ret = if (ms_rtype sign) == None then 0 else 1 in
        ret - List.length (ms_args sign)

    | OpInvoke ((`Special ty), sign) -> (* -1, signature doesn't include this pointer *)
        -1 + op_stack_delta (OpInvoke ((`Static ty), sign))
    | OpAMultiNewArray (_, dim) -> 1 - dim

    | OpInvoke ((`Virtual _), sign) ->
        let ret = if (ms_rtype sign) == None then 0 else 1 in
        -1 + ret - List.length (ms_args sign)

    | _ -> todo ("stack_delta " ^ (JPrint.jopcode op))


(*== Bytecode preprocessing: scan for locals and blocks ==*)

(* Find the indexes of opcodes that begin basic blocks. So which instructions are jump targets. 
   The list will have no duplicates. HACK: Jumps to an OpCmp are treated as a jump to the following OpIf instead. *)
let find_basic_blocks (code: jopcode array): int list =
    let rec add_target l i = (* TODO: should sort here instead of later *)
        if List.exists ((=) i) l then l else 
            let target_fcmp = match Array.get code i with | (OpCmp _) -> true | _ -> false in
            let next_if = i + 1 < (Array.length code) && match Array.get code (i + 1) with | (OpIf _) -> true | _ -> false in
            let target_if = match Array.get code i with | (OpIf _) -> true | _ -> false in
            let prev_fcmp = i != 0 && match Array.get code (i - 1) with | (OpCmp _) -> true | _ -> false in
            if target_if && prev_fcmp then illegal "jumped directly to OpIf after fcmp";
            if target_fcmp && next_if then (add_target l (i + 1)) else i :: l
    in
    let rec iter_block blocks index = 
        let op = Array.get code index in
        let blocks = match op with
            | OpIf (_, i) 
            | OpIfCmp (_, i) (* conditional jump can fall through. represent that as jumping to the next instruction *)
                -> add_target (add_target blocks (index + i)) (index + 1) 
            | OpGoto i 
                -> add_target blocks (index + i)
            | _ -> blocks in
        if index < Array.length code - 1 then iter_block blocks (index + 1) else blocks
    in
    iter_block [0] 0

type blockmap = (int, llbasicblock) Hashtbl.t

let init_basic_blocks ctx func (blocks: int list): blockmap = 
    let pairs = List.map (fun index -> (index, (append_block ctx.context ("op" ^ string_of_int index) func))) blocks in
    Hashtbl.of_seq (List.to_seq pairs)

let count_local_stores (code: jopcode array) =
    let locals = Hashtbl.create 0 in
    let maybe_init key = 
        if Hashtbl.find_opt locals key == None then (Hashtbl.replace locals key 0)
    in
    let inc key = 
        let prev = Option.default 0 (Hashtbl.find_opt locals key) in
        (Hashtbl.replace locals key (prev + 1))
    in
    Array.iter (fun op -> 
        match op with 
        (* TODO: should make key (ty, index) instead of (index, ty) so don't have to reorder *)
        | OpLoad (ty, index) -> maybe_init (index, ty)
        | OpStore (ty, index) -> inc (index, ty)
        | OpIInc (index, _) -> inc (index, `Int2Bool)
        | _ -> ()
    ) code;
    locals

(* A function argument can be reassigned like a local but if it isn't, don't ask for a stack slot *)
type argplace = 
    FinalArg of llvalue (* direct value. must never be stored to. *) 
    | Mut of llvalue (* pointer to stack slot*)
type localinfo = { place: argplace; ty: lltype; }
type localmap = (int * jvm_type, localinfo) Hashtbl.t

let assert_mut place = 
    match place with
    | Mut ptr -> ptr
    | (FinalArg _) -> unreachable () 

(* Returns map of local indexes for the function arguments to thier llvm ssa. 
   ie. f(int, long, int) -> {0=%0, 1=%1, 3=%2} because longs take two slots *)
let find_arg_locals func sign is_static = 
    let largs = Array.to_list (params func) in
    let jargs = ms_args sign in
    let start = if is_static then ([], 0) else ([(0, List.hd largs)], 1) in
    let largs = if is_static then largs else drop_fst largs in
    let pairs = List.rev (fst (List.fold_left2 (fun (l, off) jty ssa -> 
        ((off, ssa) :: l, off + (slotcount_for jty))
    ) start jargs largs)) in
    Hashtbl.of_seq (List.to_seq pairs)

(* Create the entry block before block 0 and emit alloca instructions for each local variable in the method 
   Also copy mutable arguments to thier stack slot. *)
let alloc_locals ctx func code sign is_static: localmap = 
    let arg_locals = find_arg_locals func sign is_static in
    let local_stores = count_local_stores code.c_code in
    let entry = append_block ctx.context "entry" func in 
    position_at_end entry ctx.builder; 
    tblmap (fun (key, stores) -> 
        let jvmtype = snd key in
        let i = fst key in
        let ty = lltype_of_jvmtype ctx jvmtype in
        let place = if stores > 0 then 
            let name = "var" ^ (string_of_int i) ^ "_" in
            let ptr = build_alloca ty name ctx.builder in
            (match Hashtbl.find_opt arg_locals i with
            | Some ssa -> (* its an argument. copy it to its stack slot*)
                let _ = build_store ssa ptr ctx.builder in 
                ()
            | None -> () (* it's not an argument. no initial value. *)
            );
            Mut ptr
        else (* if it's never stored, it must be a function argument *)
            let ssa = Hashtbl.find arg_locals i in
            FinalArg ssa 
        in
        ((i, jvmtype), { place; ty; })
    ) local_stores

(* It seems javac doesn't generate code that would return false here which is very convient. (Except for ternary operator)*)
let stack_comptime_safe (block_starts: int list) (code: jopcode array) =
    let starts = List.sort compare block_starts in
    let block_ranges = sliding_pairs (List.concat [starts; [Array.length code]]) in

    let rec block_safe start last stack = 
        let op = Array.get code start in
        let new_stack = stack + op_stack_delta op in
        new_stack >= 0 && if start < last - 1 then (block_safe (start + 1) last new_stack) else (new_stack == 0)
    in
    let rec safe = function 
        | [] -> true
        | (a, b) :: rest -> (block_safe a b 0) && safe rest 
    in
    safe block_ranges 

(* TODO: dont like this *)
let () = 
    let expect_no ops = 
        let l = Array.of_list ops in
        assert (not (stack_comptime_safe (find_basic_blocks l) l)) in

    expect_no [ (* ternary operator needs a phi node *)
        OpLoad (`Int2Bool, 0); OpIf (`Ge, 7); OpInvalid; OpInvalid; OpConst (`Int 0l); (OpGoto 4); OpInvalid; OpInvalid; OpLoad (`Int2Bool, 0); OpStore (`Int2Bool, 1); OpLoad (`Int2Bool, 1); OpReturn `Int2Bool;
    ];
    expect_no [ (* negate mutates stack in place *)
        OpLoad (`Int2Bool, 0); OpIf (`Ge, 4); OpInvalid; OpInvalid; OpNeg `Int2Bool; OpReturn `Int2Bool;
    ];
    expect_no [ (* array load pops 2 pushes 1 *)
        OpLoad (`Int2Bool, 0); OpLoad (`Int2Bool, 1); OpIf (`Ge, 5); OpInvalid; OpInvalid; OpLoad (`Object, 2); OpArrayLoad `Int; OpReturn `Int2Bool;
    ];
    ()

(*== Runtime intrinsics ==*)

type rt_intrinsic = (* Represents a callable function in runtime.c *)
| ArrLen
| ArrInit of jvm_array_type 
| ArrGet of jvm_array_type 
| ArrSet of jvm_array_type 
| ArrFillMulti of jvm_array_type

(* Any changes in runtime.c must be reflected here. *)
let intrinsic_signature ctx op = 
    let arr_t = pointer_type ctx.context in
    let i_t = i32_type ctx.context in

    let (name, args, ret) = match op with
        | ArrInit ty -> "array_init_" ^ (ctype_name ty), [i_t], arr_t
        | ArrGet ty -> "array_get_" ^ (ctype_name ty), [arr_t; i_t], lltype_of_arrtype ctx ty
        | ArrSet ty -> "array_set_" ^ (ctype_name ty), [arr_t; i_t; lltype_of_arrtype ctx ty], void_type ctx.context
        | ArrLen -> "array_length", [arr_t], i_t
        | ArrFillMulti ty -> "array_fillmulti_" ^ (ctype_name ty), [i_t; arr_t; i_t], arr_t
    in
    (name, args, ret)

let get_rtintrinsic ctx memo op =
    match Hashtbl.find_opt memo op with
    | Some f -> f 
    | None -> (* First time calling, need to forward declare it. *)

    let (name, args, ret) = intrinsic_signature ctx op in
    let func_ty = function_type ret (Array.of_list args) in
    let v = declare_function name func_ty ctx.the_module in
    let f = (func_ty, v) in 
    Hashtbl.replace memo op f;
    f

(*== Lazily forward declare functions and structs for classes ==*)

let forward_declare_method ctx m = 
    match m with
    | AbstractMethod _ -> todo "AbstractMethod"
    | ConcreteMethod func ->
        let sign = func.cm_signature in
        let cls = fst (cms_split func.cm_class_method_signature) in
        let name = (cn_name cls) ^ "_" ^ (ms_name sign) in
        let name = replace_chars name "><.$" '_' in
        declare_function name (llfunc_type ctx sign func.cm_static) ctx.the_module

let emit_static_field ctx anyfield = 
    match anyfield with
    | InterfaceField _ -> todo "interface static field"
    | ClassField field -> 
        assert field.cf_static;
        assert (field.cf_value == None);  (* Seems to always use <clinit> *)
        let ty = lltype_of_valuetype ctx (fs_type field.cf_signature) in
        let cls = fst (cfs_split field.cf_class_signature) in
        let name = replace_chars (cn_name cls) ".$" '_' in
        let name = name ^ "_" ^ (fs_name field.cf_signature) in 
        let init_val = const_int ty 0 in
        let v = define_global name init_val ctx.the_module in
        { place=Mut v; ty }

type compilation = {
    classes: class_path;
    intrinsics: (rt_intrinsic, lltype * llvalue) Hashtbl.t;
    func_queue: (class_method_signature, llvalue) Hashtbl.t; (* referenced but not yet compiled *)
    funcs_done: (class_method_signature, llvalue) Hashtbl.t; (* already compiled *)
    globals: (class_field_signature, localinfo) Hashtbl.t; (* static fields *)
    structs: (class_name, lltype * (field_signature, int * lltype) Hashtbl.t) Hashtbl.t;
}

let is_static_cms classes sign = 
    let (cs, ms) = cms_split sign in
    let cls = get_class classes cs in
    let cm = get_concrete_method cls ms in
    cm.cm_static

let method_of_cms classes sign = 
    let (cs, ms) = cms_split sign in
    let cls = get_class classes cs in
    get_method cls ms

let field_of_cfs classes sign = 
    let (cs, fs) = cfs_split sign in
    let cls = get_class classes cs in
    get_field cls fs

(* TODO: its a bit unserious for every method call to be two lookups *)
let find_func ctx comp sign = 
    match Hashtbl.find_opt comp.funcs_done sign with
    | Some f -> f
    | None -> (* haven't compiled yet *)
    match Hashtbl.find_opt comp.func_queue sign with
    | Some f -> f
    | None -> (* haven't seen yet *)

    let m = method_of_cms comp.classes sign in
    let func = forward_declare_method ctx m in
    Hashtbl.replace comp.func_queue sign func;
    func

let find_global ctx comp sign = 
    match Hashtbl.find_opt comp.globals sign with
    | Some f -> f
    | None -> (* haven't seen yet *)

    let field = field_of_cfs comp.classes sign in
    let global = emit_static_field ctx field in
    Hashtbl.replace comp.globals sign global;
    global

let find_class_lltype ctx comp class_name = 
    match Hashtbl.find_opt comp.structs class_name with
    | Some f -> f
    | None -> (* haven't seen yet *)
    
    let cls = get_class comp.classes class_name in
    let fields = get_fields cls in
    let add_field sign _value acc = 
        let ty = lltype_of_valuetype ctx (fs_type sign) in
        (sign, ty) :: acc
    in
    let field_info = FieldMap.fold add_field fields [] in
    let field_types = List.map snd field_info in


    let field_info = List.mapi (fun i (sign, ty) -> 
        (sign, (i, ty))
        ) field_info in
    let field_slots = Hashtbl.of_seq (List.to_seq field_info) in
    
    (* TODO: first field is super class. TODO: does llvm mess with field order? TODO: add canary to object header. *)
    
    let name = JPrint.class_name class_name in
    let name = replace_chars name ".$" '_' in
    (* TODO: store indexes of each field *)
    let ll = named_struct_type ctx.context name in
    let is_packed = false in
    struct_set_body ll (Array.of_list field_types) is_packed;
    let v = (ll, field_slots) in
    Hashtbl.replace comp.structs class_name v;
    v

(*== Emiting llvm ir for a single method ==*)

let load_local ctx i (locals: localmap) = 
    let local = Hashtbl.find locals i in
    match local.place with
    | Mut ptr -> build_load local.ty ptr "" ctx.builder
    | FinalArg v -> v

let store_local ctx i (locals: localmap) v = 
    let local_ptr = assert_mut ((Hashtbl.find locals i).place) in
    let _ = build_store v local_ptr ctx.builder in 
    ()

let bin_op (f: llvalue -> llvalue -> llvalue) compstack = 
    if List.length compstack < 2 then illegal "Stack underflow on bin_op";
    let a = List.nth compstack 0 in
    let b = List.nth compstack 1 in
    (* Argument order matters for sub/div/rem *)
    (f b a) :: (drop_fst (drop_fst compstack))

let emit_const ctx (v: jconst): llvalue = 
    match v with
    | `Int n -> const_int (i32_type ctx.context) (Int32.to_int n)
    | (`Byte n) | (`Short n) -> const_int (i32_type ctx.context) n
    | `Long n -> const_int (i64_type ctx.context) (Int64.to_int n)
    | `Float n -> const_float (float_type ctx.context) n
    | `Double n -> const_float (double_type ctx.context) n
    | _ -> todo "emit_const other types"

let total_seen = ref 0  (* TODO: can remove. was just curious *)

let convert_method ctx code current_cms comp = 
    let current_sign = snd (cms_split current_cms) in
    let is_static = is_static_cms comp.classes current_cms in
    let func = find_func ctx comp current_cms in 
    let ret_ty = ms_rtype current_sign in

    let block_positions = find_basic_blocks code.c_code in
    if not (stack_comptime_safe block_positions code.c_code)
        then todo ("use rt stack. not stack_comptime_safe in " ^ (ms_name current_sign) ^ " [often we dont like (a ? b : c)]");

    let locals = alloc_locals ctx func code current_sign is_static in
    let basic_blocks = init_basic_blocks ctx func block_positions in
    let first = Hashtbl.find basic_blocks 0 in
    let _ = build_br first ctx.builder in (* jump from stack setup to first instruction *)

    let do_cast f out s = 
        let v = List.nth s 0 in
        let res = f v (out ctx.context) "" ctx.builder in
        res :: drop_fst s
    in
    let cmp f k s =
        let a = List.nth s 0 in
        let b = List.nth s 1 in
        (f k b a "" ctx.builder), (drop_fst (drop_fst s))
    in
    let call_intin intrin in_stack = 
        let f = get_rtintrinsic ctx comp.intrinsics intrin in 
        let arg_count = Array.length (param_types (fst f)) in
        let (stack, args) = pop_n in_stack arg_count in
        let ret = build_call (fst f) (snd f) (Array.of_list args) "" ctx.builder in
        (ret, stack)
    in
    let call_method classname target_sign is_static compstack = 
        let arg_count = List.length (ms_args target_sign) in
        let arg_count = if is_static then arg_count else arg_count + 1 in
        if arg_count > List.length compstack 
            then illegal ("stack underflow calling " ^ (ms_name target_sign) ^ " from " ^ (ms_name current_sign));
        let func_ty = llfunc_type ctx target_sign is_static in
        let cms = make_cms classname target_sign in
        let func_value = find_func ctx comp cms in 
        let (new_stack, arg_values) = pop_n compstack arg_count in
        let result = build_call func_ty func_value (Array.of_list arg_values) "" ctx.builder in
        if (ms_rtype target_sign) == None then new_stack else (result :: new_stack)
    in
    let get_field_ptr obj cls sign =
        let (obj_ty, fields) = find_class_lltype ctx comp cls in
        let (field_index, ty) = Hashtbl.find fields sign in
        let ptr = build_struct_gep obj_ty obj field_index "" ctx.builder in
        (ptr, ty)
    in

    let emit_op compstack op index prev_op = 
        if op != OpInvalid then total_seen := !total_seen + 1;

        (match Hashtbl.find_opt basic_blocks index with
        | Some bb -> (* entering a new block *)
            let current = insertion_block ctx.builder in
            let _ = assert_empty compstack in
            (match block_terminator current with 
            | Some _ -> () (* last instruction was a jump *)
            | None -> 
                let _ = build_br bb ctx.builder in ()); (* this is the head of a loop so fallthrough *)
            position_at_end bb ctx.builder (* either way start writing the new block*)
        | None -> () (* continue the previous block*)
        );

        let do_bin f = bin_op (fun a b -> f a b "" ctx.builder) compstack in 
        match op with
        | OpLoad (ty, i) -> load_local ctx (i, ty) locals :: compstack
        | OpStore (ty, i) -> 
            store_local ctx (i, ty) locals (List.hd compstack);
            drop_fst compstack
        | OpAdd ty -> do_bin (int_or_float ty build_add build_fadd) (* TODO: test overflow on smaller types *)
        | OpSub ty -> do_bin (int_or_float ty build_sub build_fsub) 
        | OpMult ty -> do_bin (int_or_float ty build_mul build_fmul) 
        | OpDiv ty -> do_bin (int_or_float ty build_sdiv build_fdiv) 
        | OpRem ty -> do_bin (int_or_float ty build_srem build_frem) (* TODO: test on floats/doubles *)

        (* TODO: write tests for bitwise ops *)
        | OpIAnd | OpLAnd -> do_bin build_and 
        | OpIOr | OpLOr -> do_bin build_or 
        | OpIXor | OpLXor -> do_bin build_xor
        | OpIUShr | OpLUShr -> do_bin build_lshr
        | OpIShr | OpLShr -> do_bin build_ashr
        | OpIShl | OpLShl -> do_bin build_shl

        | OpIfCmp (kind, offset) -> 
            let true_block = Hashtbl.find basic_blocks (index + offset) in
            let false_block = Hashtbl.find basic_blocks (index + 1) in
            let (c, stack) = cmp build_icmp (intcmp kind) compstack in
            let _ = build_cond_br c true_block false_block ctx.builder in
            assert_empty stack
        | OpIf (kind, offset) -> 
            let true_block = Hashtbl.find basic_blocks (index + offset) in
            let false_block = Hashtbl.find basic_blocks (index + 1) in

            (* alas this is context dependent *)
            let (c, stack) = (match prev_op with
            | Some (OpCmp `L) -> (* Comparing longs is just a normal integer compare in llvm *)
                cmp build_icmp (intcmp kind) compstack
            | Some (OpCmp fkind) -> (* This is actually a (2) floats/doubles compare. *)
                let flag = fcmp_flag fkind kind in 
                cmp build_fcmp flag compstack
            | _ -> (* This is just a normal integer compare to zero. *)
                let zero = const_int (type_of (List.hd compstack)) 0 in 
                cmp build_icmp (intcmp kind) (zero :: compstack)
            ) in
            let _ = build_cond_br c true_block false_block ctx.builder in
            assert_empty stack
        | OpGoto offset -> 
            let bb = Hashtbl.find basic_blocks (index + offset) in
            let _ = build_br bb ctx.builder in
            assert_empty compstack
        | OpReturn _ -> 
            (match ret_ty with
            | None -> 
                let _ = build_ret_void ctx.builder in
                assert_empty compstack
            | (Some (TObject _)) | (Some (TBasic (`Int | `Float | `Double | `Long))) ->
                let v = List.hd compstack in
                let _ = build_ret v ctx.builder in
                assert_empty (drop_fst compstack)
            | Some (TBasic b) -> 
                let stack = (match b with
                | `Short -> do_cast build_intcast i16_type compstack
                | `Byte -> do_cast build_intcast i8_type compstack
                | `Char -> todo "what's a char?"
                | `Bool -> do_cast build_intcast i1_type compstack
                | _ -> unreachable ()
                ) in
                let _ = build_ret (List.hd stack) ctx.builder in
                assert_empty (drop_fst stack))
        | OpConst v -> (emit_const ctx v) :: compstack
        | OpIInc (local_index, v) -> 
            let key = (local_index, `Int2Bool) in
            let old_val = load_local ctx key locals in
            let inc = const_int (i32_type ctx.context) v in
            let new_val = build_add old_val inc "" ctx.builder in
            store_local ctx key locals new_val;
            compstack

        | OpNeg ty -> 
            let v = List.nth compstack 0 in
            let neg = int_or_float ty build_neg build_fneg in 
            let result = neg v "" ctx.builder in
            result :: drop_fst compstack

        (* TODO: write tests for every type of cast (and every overflow condition). 
           If I trust that other tests test the basics could write all in java and just return count of successes or whatever *)
        | OpF2I | OpD2I -> do_cast build_fptosi i32_type compstack
        | OpF2L | OpD2L -> do_cast build_fptosi i64_type compstack
        | OpI2F | OpL2F -> do_cast build_sitofp float_type compstack
        | OpI2D | OpL2D -> do_cast build_sitofp double_type compstack
        | OpF2D -> do_cast build_fpcast double_type compstack
        | OpD2F -> do_cast build_fpcast float_type compstack
        | OpI2L -> do_cast build_sext i64_type compstack
        | OpL2I -> do_cast build_intcast i32_type compstack
        | OpI2B -> 
            let stack = do_cast build_intcast i8_type compstack in
            do_cast build_sext i32_type stack
        | OpI2S -> 
            let stack = do_cast build_intcast i16_type compstack in
            do_cast build_sext i32_type stack
        | OpI2C -> todo "what's a char?"

        | OpInvoke ((`Static (_ioc, classname)), target_sign) -> 
            call_method classname target_sign true compstack
        | OpInvoke ((`Special (_ioc, classname)), target_sign) -> 
            call_method classname target_sign false compstack
        | OpInvoke ((`Virtual objty), target_sign) -> 
            let classname = match objty with
            | TClass cl -> cl
            | TArray _ -> todo "unreachable? virtual method call on an array."
            in

            (* Walk the inheritance chain up from classname and find a method matching sign. 
               Note: the method might be overriden between classname and the class this returns. 
               Can only use static dispatch if the method is final. *)
            let rec resolve_method classname sign =
                let cls = get_class comp.classes classname in
                let m = get_method_opt cls target_sign in
                (match m with
                | Some m -> Some (classname, m)
                | None -> 
                    let super = match cls with
                    | JInterface _ -> todo "interface resolve_method"
                    | JClass cls -> cls.c_super_class
                    in

                    match super with 
                    | None -> None
                    | Some super -> resolve_method super sign
                    )
            in

            let (foundclass, m) = Option.get (resolve_method classname target_sign) in
            if is_final_method m then 
                call_method foundclass target_sign false compstack
            else
                todo "vtable call"
                

        | OpNew classname -> (* TODO: call an intrinsic to register it with the garbage collector. *)
            let ty = fst (find_class_lltype ctx comp classname) in
            let obj = build_malloc ty "" ctx.builder in
            obj :: compstack
        
        | OpGetField (cls, sign) -> 
            let (ptr, ty) = get_field_ptr (List.hd compstack) cls sign in
            let v = build_load ty ptr "" ctx.builder in
            v :: (drop_fst compstack)
        
        | OpPutField (cls, sign) -> 
            let obj = List.nth compstack 1 in
            let value = List.nth compstack 0 in
            let (ptr, _) = get_field_ptr obj cls sign in
            let _ = build_store value ptr ctx.builder in
            drop_fst (drop_fst compstack)

        | (OpCmp _ ) -> (* really this produces a value but instead, use it as a modifier to the opif *)
            (* TODO: pass next_op to emit_op if i start using it for anything *)
            let next_op = Array.get code.c_code (index + 1) in 
            (match next_op with 
            | (OpIf _) -> compstack 
            | _ -> illegal "OpCmp must be followed by OpIf")

        | OpGetStatic (classname, field_sign) ->
            let cfs = make_cfs classname field_sign in
            let global = find_global ctx comp cfs in
            let ptr = assert_mut global.place in
            let v = build_load global.ty ptr "" ctx.builder in
            v :: compstack

        | OpPutStatic (classname, field_sign) ->
            let cfs = make_cfs classname field_sign in
            let global = find_global ctx comp cfs in
            let ptr = assert_mut global.place  in
            let _ = build_store (List.hd compstack) ptr ctx.builder in
            drop_fst compstack

        | (OpNewArray ty) ->
            let ty = arraytype_of_valuetype ty in
            pre (call_intin (ArrInit ty) compstack)
        | OpArrayLength -> pre (call_intin ArrLen compstack)
        | (OpArrayStore ty) -> snd (call_intin (ArrSet ty) compstack)
        | (OpArrayLoad ty) -> pre (call_intin (ArrGet ty) compstack)
        
        (* TOOD: make this just one a runtime call passing the counts array *)
        | OpAMultiNewArray (ty, dim) -> 
            let (rest_stack, counts) = pop_n compstack dim in
            let inner_type = inner_array_type ty in

            let (arr, counts) = call_intin (ArrInit `Object) counts in
            let rec do_fill working depth = (* working = array_ref :: [..counts] *)
                let isLast = (List.length working) == 2 in
                let this_ty = if isLast then inner_type else `Object in
                let depth_v = const_int (i32_type ctx.context) depth in
                let (arr, counts) = call_intin (ArrFillMulti this_ty) (depth_v :: working) in
                if isLast then [arr] else do_fill (arr :: counts) (depth + 1)
            in

            let working = do_fill (arr :: counts) 0 in
            let _ = assert (List.length working == 1) in
            (List.hd working) :: rest_stack
        | OpDup -> List.hd compstack :: compstack
        
        
        | OpInvalid (* these slots are the arguments to previous instruction *)
        | OpNop -> compstack 
        | (OpRet _) | (OpJsr _ ) -> illegal ((JPrint.jopcode op) ^ " was deprecated in Java 7.")
        | _ -> todo ("emit_op " ^ JPrint.jopcode op)
        in

    let rec loop stack index prev_op = 
        let op = Array.get code.c_code index in
        let new_stack = emit_op stack op index prev_op in
        if index < Array.length code.c_code - 1 then loop new_stack (index + 1) (Some op) else ()
    in
    loop [] 0 None

(*== Walking classes to find code ==*)

let emit_method ctx comp m = 
    match m with
    | AbstractMethod _ -> todo "AbstractMethod"
    | ConcreteMethod func ->
        let sign = func.cm_class_method_signature in
        assert ((Hashtbl.find_opt comp.funcs_done sign) == None);
        (match func.cm_implementation with
            | Native -> ()
            | Java code ->
                let jcode = Lazy.force code in
                convert_method ctx jcode sign comp
        );
        
        let ll = Hashtbl.find comp.func_queue sign in
        Hashtbl.remove comp.func_queue sign;
        Hashtbl.replace comp.funcs_done sign ll;
        ()


let () = 
    let entry_classes = drop_fst (Array.to_list Sys.argv) in
    
    (* TODO: don't just hardcode the path lol *)
    (* TODO: past java 8 need to figure out what to do with a jmod file *)
    let classes = class_path "./java:./out/java:/Library/Java/JavaVirtualMachines/jdk1.8.0_202.jdk/Contents/Home/jre/lib" in (*  *)

    let c = create_context () in
    let ctx = { context = c; the_module = create_module c "javatest"; builder = builder c } in
    let comp = { 
        classes;
        intrinsics = Hashtbl.create 0; 
        func_queue = Hashtbl.create 0; 
        funcs_done = Hashtbl.create 0; 
        globals = Hashtbl.create 0; 
        structs = Hashtbl.create 0; 
    } in

    (* Treat all static methods in the class <name> as roots. TODO: ugly *)
    let reference_all name =
        let cls = get_class classes (make_cn name) in
        let methods = MethodMap.filter is_static_method (get_methods cls) in
        MethodMap.iter (fun ms _ -> 
            let _ = find_func ctx comp (make_cms (make_cn name) ms) in
            ()
            ) methods;
    in

    List.iter reference_all entry_classes;

    let rec emit_next () = 
        match take_any comp.func_queue with
        | None -> ()
        | Some (cms, _) -> 
            let m = method_of_cms comp.classes cms in
            emit_method ctx comp m;
            emit_next ()
    in
    emit_next ();
    assert (Hashtbl.length comp.func_queue == 0);
    
    let code = string_of_llmodule ctx.the_module in
    print_endline code;
    eprintf "Compiled %d bytecode instructions.\n" (!total_seen);

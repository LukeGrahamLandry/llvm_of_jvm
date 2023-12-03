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
let _assert_empty l = if not (List.length l == 0) then illegal "Expected list to be empty" else l
let replace_chars s before after = String.map (fun c -> if String.contains before c then after else c) s

(* [a; b; c; d] -> [(a, b); (b, c); (c; d)]*)
let rec sliding_pairs l =
    match l with
    | a :: [b] -> [(a, b)]
    | a :: (b :: rest) -> (a, b) :: (sliding_pairs (b :: rest))
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


(* Choose an arbitrary entry from a hash table (does not auto remove it) *)
let take_any m = 
    match Hashtbl.to_seq m () with
    | Nil -> None
    | Cons (d, _) -> Some d

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
    | `AEq -> Eq (* TODO: test this *)
    | `ANe -> Ne
    | `Null -> Eq
    | `NonNull -> Ne
        

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

let safe_cn cn = 
    let name = JPrint.class_name cn in
    replace_chars name ".$" '_'


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
    | (OpArrayLoad _) | OpPop
    | OpIf _ (* comparing to zero *)
    | OpMonitorEnter | OpMonitorExit
    -> -1

    | OpI2L | OpI2F | OpI2D | OpL2I | OpL2F | OpL2D | OpF2I | OpF2L | OpF2D | OpD2I | OpD2L | OpD2F | OpI2B | OpI2C | OpI2S 
    | (OpNeg _) | (OpIInc _) 
    | OpGoto _
    | OpInvalid
    | (OpNewArray _) | OpArrayLength | (OpGetField _) | (OpInstanceOf _) | (OpCheckCast _) | OpNop
    -> 0

    | OpThrow -> -1

    | (OpLoad _) | (OpConst _) | (OpGetStatic _) | (OpNew _) | OpDup -> 1

    | OpReturn ty -> if ty == `Void then 0 else -1
    | OpInvoke ((`Static _), sign) -> 
        let ret = if (ms_rtype sign) == None then 0 else 1 in
        ret - List.length (ms_args sign)

    | OpInvoke ((`Special ty), sign) -> (* -1, signature doesn't include this pointer *)
        -1 + op_stack_delta (OpInvoke ((`Static ty), sign))
    | OpAMultiNewArray (_, dim) -> 1 - dim

    | OpInvoke ((`Virtual _), sign)
    | OpInvoke ((`Interface _), sign) ->
        let ret = if (ms_rtype sign) == None then 0 else 1 in
        -1 + ret - List.length (ms_args sign)

    | OpInvoke ((`Dynamic _), sign) ->
        let ret = if (ms_rtype sign) == None then 0 else 1 in
        ret - List.length (ms_args sign)

    | _ -> todo ("stack_delta " ^ (JPrint.jopcode op))


(*== Bytecode preprocessing: scan for locals and blocks ==*)

(* Find the indexes of opcodes that begin basic blocks. So which instructions are jump targets. 
   The list will have no duplicates. HACK: Jumps to an OpCmp are treated as a jump to the following OpIf instead. *)
let find_basic_blocks (code: jopcode array) exceptions: int list =
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

    let e_blocks = List.fold_left (fun prev e -> 
        add_target prev e.e_handler
        ) [] exceptions in
    
    iter_block (0 :: e_blocks) 0

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
        if new_stack < 0 then (0, false) else 
        if start < last - 1 then (block_safe (start + 1) last new_stack) else new_stack, (new_stack <= 1)
    in
    let rec safe s = function 
        | [] -> true
        | (a, b) :: rest -> 
            let (stack, was_safe) = (block_safe a b s)  in 
            (* TODO: need to require that the second phi block doesn't pop the stack but allow the reconnecting block to pop the result *)
            was_safe && safe stack rest
    in
    let _  = safe 0 block_ranges in

    (* TODO: I'm ignoring cause it doesnt work!!! *)
    true

(* TODO: dont like this *)
(* TODO: update for new safety requirements. basic phi is allowed but branch may not pop the stack. must be exactly two branches. must have only one incoming block *)
(* let () = 
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
    () *)

(*== Runtime intrinsics ==*)

type rt_intrinsic = (* Represents a callable function in runtime.c *)
| ArrLen
| ArrInit of jvm_array_type 
| ArrGet of jvm_array_type 
| ArrSet of jvm_array_type 
| ArrFillMulti of jvm_array_type
| FillStr
| LogThrow
| InstCheck
| FindInterface
| CheckCast
| ConcatStrings
| AssertVptrOrNull

(* Any changes in runtime.c must be reflected here. *)
let intrinsic_signature ctx op = 
    let arr_t = pointer_type ctx.context in
    let i_t = i32_type ctx.context in
    let i64_t = i64_type ctx.context in

    let (name, args, ret) = match op with
        | ArrInit ty -> "array_init_" ^ (ctype_name ty), [i_t], arr_t
        | ArrGet ty -> "array_get_" ^ (ctype_name ty), [arr_t; i_t], lltype_of_arrtype ctx ty
        | ArrSet ty -> "array_set_" ^ (ctype_name ty), [arr_t; i_t; lltype_of_arrtype ctx ty], void_type ctx.context
        | ArrLen -> "array_length", [arr_t], i_t
        | ArrFillMulti ty -> "array_fillmulti_" ^ (ctype_name ty), [i_t; arr_t; i_t], arr_t
        | FillStr -> "fill_string_const", [arr_t; i_t; i64_t; i64_t; i64_t; arr_t;], arr_t
        | LogThrow -> "log_throw", [arr_t], void_type ctx.context
        | InstCheck -> "check_instanceof", [arr_t; arr_t], i1_type ctx.context
        | CheckCast -> "assert_instanceof", [arr_t; arr_t], void_type ctx.context
        | FindInterface -> "resolve_interface_vtable", [arr_t; arr_t], arr_t
        | ConcatStrings -> "concat_strings", [arr_t; i_t], arr_t
        | AssertVptrOrNull -> "assert_has_vptr_or_null", [arr_t], void_type ctx.context
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

type structfield = { index: int; ty: lltype; }

type vtableinfo = {
    ty: lltype;
    value_ptr: llvalue;
    fields: (method_signature, structfield) Hashtbl.t;
    value_direct: llvalue;
}

type structinfo = {
    ty: lltype;
    fields: (field_signature, structfield) Hashtbl.t;
    vtable: vtableinfo;
}

(* type wip_func = Done of llvalue | Queued of llvalue | Tabled of llvalue *)



type compilation = {
    classes: class_path;
    intrinsics: (rt_intrinsic, lltype * llvalue) Hashtbl.t;
    funcs_vtabled: (class_method_signature, llvalue) Hashtbl.t; (* not referenced yet but in a vtable *)
    func_queue: (class_method_signature, llvalue) Hashtbl.t; (* referenced but not yet compiled *)
    funcs_done: (class_method_signature, llvalue) Hashtbl.t; (* already compiled *)
    globals: (class_field_signature, localinfo) Hashtbl.t; (* static fields *)
    structs: (class_name, structinfo) Hashtbl.t;
    virtually_called: (class_method_signature, unit) Hashtbl.t;
    overload_count: (string, int) Hashtbl.t;
    wip_vtable_memo: (class_name, vtableinfo) Hashtbl.t;
    interface_called: (class_method_signature, unit) Hashtbl.t;
    class_cache: (class_name, jcode interface_or_class) Hashtbl.t;
    const_strings: (string, int * llvalue) Hashtbl.t;
    string_const_ty: lltype;
    string_pool: llvalue;
}

let javalangObject = make_cn "java.lang.Object"
let javalangThrowable = make_cn "java.lang.Throwable"
let javalangString = make_cn "java.lang.String"
let javalanginvokeStringConcatFactory = make_cn "java.lang.invoke.StringConcatFactory"

let clinit_ms = make_ms "<clinit>" [] None
let string_value_field_sign = make_fs "value" (TObject (TArray (TBasic `Char))) 

(* Never call get_class directly, it reloads from disk and reparses every time. 
   This function generates lambda classes. *)
let get_class_cached comp cn = 
    match Hashtbl.find_opt comp.class_cache cn with
    | Some c -> c
    | None -> 
        let c = get_class comp.classes cn in

        (* Replace usages of java.lang.invoke.LambdaMetafactory::metafactory *)
        (* TODO: might want to do this myself instead of trusting the library at some point *)
        let prefix = safe_cn cn ^ "_lambda" in
        let (c, generated) = remove_invokedynamics c ~prefix in
        ClassMap.iter (fun gen_cn gen_c -> Hashtbl.replace comp.class_cache gen_cn gen_c) generated;
        Hashtbl.replace comp.class_cache cn c;
        c

let get_super childcls = 
    match childcls with
    | JInterface _ -> Some javalangObject
    | JClass cls -> (match cls.c_super_class with
        | Some super -> Some super
        | None -> 
            assert ((cn_equal (get_name childcls)) javalangObject);
            None)

let assert_super childcls = 
    Option.get (get_super childcls)

let mangled_name comp sign = (* TODO: this should be deterministic not based on compile order *)
    let (cs, ms) = cms_split sign in
    let name = (cn_name cs) ^ "_" ^ (ms_name ms) in
    let name = replace_chars name "><.$" '_' in
    let count = Option.default 0 (Hashtbl.find_opt comp.overload_count name) in
    Hashtbl.replace comp.overload_count name (count + 1);
    if count == 0 then name else (name ^ (string_of_int count))

let forward_declare_method ctx comp m = 
    match m with
    | AbstractMethod func -> (* this will get filled in with `unreachable`*)
        let name = mangled_name comp func.am_class_method_signature in
        let ty = llfunc_type ctx func.am_signature false in
        declare_function name ty ctx.the_module
    | ConcreteMethod func ->
        let name = mangled_name comp func.cm_class_method_signature in
        let ty = llfunc_type ctx func.cm_signature func.cm_static in
        declare_function name ty ctx.the_module

let emit_static_field ctx anyfield = 
    match anyfield with
    | InterfaceField _ -> todo "interface static field"
    | ClassField field -> 
        assert field.cf_static;
        assert (field.cf_value == None);  (* Seems to always use <clinit> *)
        let jty = fs_type field.cf_signature in 
        let ty = lltype_of_valuetype ctx jty in
        let cls = fst (cfs_split field.cf_class_signature) in
        let name = replace_chars (cn_name cls) ".$" '_' in
        let name = name ^ "_" ^ (fs_name field.cf_signature) in 
        let init_val = match jty with
        | TObject _ -> const_null ty
        | TBasic _ -> const_int ty 0 in
        let v = define_global name init_val ctx.the_module in
        { place=Mut v; ty }

let is_static_cms comp sign = 
    let (cs, ms) = cms_split sign in
    let cls = get_class_cached comp cs in
    let cm = get_concrete_method cls ms in
    cm.cm_static

let method_of_cms comp sign = 
    let (cs, ms) = cms_split sign in
    let cls = get_class_cached comp cs in
    get_method cls ms

let field_of_cfs comp sign = 
    let (cs, fs) = cfs_split sign in
    let cls = get_class_cached comp cs in
    get_field cls fs

(* TODO: its a bit unserious for every method call to be three lookups *)
let find_func_inner referenced ctx comp sign = 
    match Hashtbl.find_opt comp.funcs_done sign with
    | Some f -> 
        f
    | None -> (* haven't compiled yet *)
    match Hashtbl.find_opt comp.func_queue sign with
    | Some f -> 
        f
    | None -> (* haven't referenced it yet *)

    match Hashtbl.find_opt comp.funcs_vtabled sign with
    | Some f -> 
        if referenced then
            (Hashtbl.remove comp.funcs_vtabled sign;
            Hashtbl.replace comp.func_queue sign f;
            ()) else ();
        f
    | None -> (* haven't seen it yet *)

    let m = method_of_cms comp sign in
    if referenced && is_synchronized_method m then eprintf "Warning: referenced synchronized method %s\n" (JPrint.class_method_signature sign);
    let func = forward_declare_method ctx comp m in
    let funcs = if referenced then comp.func_queue else comp.funcs_vtabled in
    Hashtbl.replace funcs sign func;
    func

let find_func = find_func_inner true

let find_global ctx comp sign = 
    match Hashtbl.find_opt comp.globals sign with
    | Some f -> f
    | None -> (* haven't seen yet *)

    let field = field_of_cfs comp sign in
    let global = emit_static_field ctx field in
    Hashtbl.replace comp.globals sign global;
    global

let tobjcls = TObject (TClass (javalangObject))

        
 (* It's a bit misleading to say this is the type of the field because its an inlined struct not a pointer. *)
let parent_field_sign = make_fs "__parent" tobjcls
(* TODO: its very cringe that im saying a java type here but it works for now *)
let vtable_field_sign =  make_fs "__vtable" tobjcls

(* walk up the chain and find the one who isn't overriding the method 
   Ie. Integer.toString returns Object 
   Note: for interface methods, it returns the first class in the chain to implement it NOT the interface itself 
        default methods may have None and thus crash so this should only be called when you know there's ~some~ override *)
let find_base_declaration comp cn ms =
    let rec aux comp cn ms = 
        let check_cls = get_class_cached comp cn in
        let i_declare = defines_method check_cls ms in
        match get_super check_cls with
        | Some super -> 
            let upchain = aux comp super ms in
            (match upchain with
            | Some c -> Some c
            | None -> 
                if i_declare then Some cn else None)
        | None -> (* no super class. either I declare it or I don't *)
            if i_declare then Some cn else None
    in
    match aux comp cn ms with 
    | Some c -> c 
    | None -> illegal ("find_base_declaration failed on " ^ (JPrint.class_method_signature (make_cms cn ms)) ^ ". interface default method?")


(* find all the interfaces implemented by <cn> including by its super classes and interfaces implemented by its interfaces *)
(* TODO: this should be a Seq iterator instead of allocating the whole list. *)
let rec all_interfaces comp cn = 
    let cls = get_class_cached comp cn in
    match cls with
    | JInterface info -> (* itself and any supers *)
        cn :: List.concat_map (all_interfaces comp) info.i_interfaces
        
    | JClass info -> 
        let supers = match get_super cls with
        | None -> []
        | Some super -> all_interfaces comp super  
        in

        info.c_interfaces @ supers

(* find the interface that <cn> implements that provides <ms> *)
let find_interface_declaration comp cn ms =
    let defines i = 
        defines_method (get_class_cached comp i) ms
    in
    List.find_opt defines (all_interfaces comp cn)

(* Find the class who's method you'd call if you called `ms` on `cn`.
   The class that declares the method lowest in the chain. *)
let rec resolve_override comp cn ms = 
    let cls = get_class_cached comp cn in
    if defines_method cls ms then Some cn else 
    match get_super cls with
    | Some super -> resolve_override comp super ms
    | None -> None (* TODO: interface default method? *)

(* returns [ childname; ...; java.lang.Object; ] *)
let rec inheritance_chain comp childname = 
    let cls = get_class_cached comp childname in
    match get_super cls with
    | Some super -> childname :: inheritance_chain comp super
    | None -> [childname]

let rec find_vtable ctx comp class_name: vtableinfo = 
    (* find_vtable is called recursivly so it might not be in the structs map yet but still needs to refer to same global address for a given class *)
    match Hashtbl.find_opt comp.wip_vtable_memo class_name with
    | Some vtable -> vtable 
    | None -> (* haven't seen yet *)
    
    let cls = get_class_cached comp class_name in
    let ptr_ty = pointer_type ctx.context in
    let name = safe_cn class_name in

    let interfaceinfo_ty = named_struct_type ctx.context "InterfaceInfo" in
    struct_set_body interfaceinfo_ty (Array.of_list [ptr_ty; ptr_ty; ptr_ty;]) false;
    
    let interface_list_ptr = match cls with
    | JInterface _ -> const_null ptr_ty
    | JClass classinfo -> 
        (* for every interface i implement, need to get function pointers for all its methods. *)
        let emit_interface_impl_vtable (prev, interfaces) interface_cn = 
            assert (not ((get_super cls) == None)); (* Object implements no interfaces *)

            let _interface_cls = match get_class_cached comp interface_cn with 
            | JInterface c -> c | JClass _ -> assert false
            in

            let prev_vptr = Option.default (const_null ptr_ty) prev in
            let interface_vtable = find_vtable ctx comp interface_cn in
            let lang_object_vtable = find_vtable ctx comp javalangObject in

            (* resolve each function in the interface against my concrete class. *)
            let vstruct_values = Hashtbl.fold (fun sign value prev -> 
                let func = match resolve_override comp class_name sign with 
                | Some target_class -> find_func_inner false ctx comp (make_cms target_class sign)
                | None -> 
                    (* default method that isn't overriden *)
                    find_func_inner false ctx comp (make_cms interface_cn sign)
                in
                
                (value.index, func) :: prev
                ) interface_vtable.fields [(0, lang_object_vtable.value_direct)] 
            in

            (* TODO: should really have a consistant way of emitting in the right order instead of these one-liner sorts *)
            let vstruct_values = List.map snd (List.sort (fun (a, _) (b, _) -> compare a b) vstruct_values) in 

            let iname = safe_cn interface_cn in
            let vtable_value = const_named_struct interface_vtable.ty (Array.of_list vstruct_values) in
            let vtable_value_ptr = define_global ("vtable_" ^ name ^ "_impl_" ^ iname) vtable_value ctx.the_module in
            set_global_constant true vtable_value_ptr;

            let node_value = const_named_struct interfaceinfo_ty (Array.of_list [prev_vptr; vtable_value_ptr; interface_vtable.value_ptr]) in
            let vptr = define_global ("node_" ^ name ^ "_impl_" ^ iname) node_value ctx.the_module in
            set_global_constant true vptr;

            (Some vptr, interfaces)
        in

        let (last, _) = List.fold_left emit_interface_impl_vtable (None, []) classinfo.c_interfaces in

        match last with
        | None -> const_null ptr_ty
        | Some p -> p
    in

    (* no super class (java.lang.Object), has a slot for the vptr of the parent class. used for instanceof checks *)
    let root_vtable_ty = struct_type ctx.context (Array.of_list [ptr_ty; ptr_ty;]) in
    let parent_vptr = match get_super (get_class_cached comp class_name) with
    | Some cls -> (find_vtable ctx comp cls).value_ptr
    | None -> const_null ptr_ty
    in
    let root_vtable_value = const_struct ctx.context (Array.of_list [parent_vptr; interface_list_ptr]) in

    (* For each class in the chain, starting at the top, look at each base method declaration
    but resolve it against my class and make a new vtable value with that function pointer instead of the original. 
    At each level, the layout of my vtable matches so it can be read by a caller who just knows the base class. *)
    let my_inheritance = drop_fst (inheritance_chain comp class_name) in
    let fill_vtable nextclass prev_val =
        let next_vtable = find_vtable ctx comp nextclass in
        let vtable_values = Hashtbl.fold (fun ms field prevfields ->
            let firstclass = Option.get (resolve_override comp class_name ms) in
            let func = find_func_inner false ctx comp (make_cms firstclass ms) in
            (field.index, func) :: prevfields
        ) next_vtable.fields [] in 
        let vtable_values = List.sort (fun a b -> compare (fst a) (fst b)) vtable_values in
        let vtable_values = List.map snd vtable_values in

        let vtable_values = prev_val :: vtable_values in
        const_named_struct next_vtable.ty (Array.of_list vtable_values)
    in
    let inherited_vtable_value = List.fold_right fill_vtable my_inheritance root_vtable_value in

    let needs_vtable_entry m =
        not (is_final_method m) && not (is_static_method m) && (cn_equal class_name (find_base_declaration comp class_name (get_method_signature m))) 
    in
    
    let methods = MethodMap.filter needs_vtable_entry (get_methods cls) in
    let method_count = List.length (MethodMap.elements methods) in

    let ptr_ty = pointer_type ctx.context in
    let add_method ms _ (index, types, fields, funcs) =
        let cms = make_cms class_name ms in
        let func = find_func_inner false ctx comp cms in (* forward declare but don't actually request compile yet *)
        let ty = llfunc_type ctx ms false in
        (index - 1, ptr_ty :: types, (ms, { index; ty; }) :: fields, func :: funcs)
    in

    let (_, vstruct_field_types, fields, vstruct_values) = MethodMap.fold add_method methods (method_count, [], [], []) in
    
    (* The first field in my vtable is my parents whole vtable. If no super class (java.lang.Object), use an empty struct *)
    let super_table_ty = match get_super cls with
    | Some super -> (find_vtable ctx comp super).ty
    | None -> root_vtable_ty
    in

    (* Note: the inherited_vtable_value takes a slot but you can never refer to it directly. *)
    let vstruct_field_types = super_table_ty :: vstruct_field_types in
    let vstruct_values = inherited_vtable_value :: vstruct_values in

    let vtable_ty = named_struct_type ctx.context ("VTable_" ^ name) in
    struct_set_body vtable_ty (Array.of_list vstruct_field_types) false;

    let vtable_value = const_named_struct vtable_ty (Array.of_list vstruct_values) in
    let vtable_value_ptr = define_global ("vtable_" ^ name) vtable_value ctx.the_module in
    set_global_constant true vtable_value_ptr;

    let fields = Hashtbl.of_seq (List.to_seq fields) in 
    let vtable = { ty=vtable_ty; value_ptr=vtable_value_ptr; fields; value_direct=vtable_value} in
    Hashtbl.replace comp.wip_vtable_memo class_name vtable;
    vtable


let rec find_class_lltype ctx comp class_name = 
    match Hashtbl.find_opt comp.structs class_name with
    | Some f -> f
    | None -> (* haven't seen yet *)
    
    let cls = get_class_cached comp class_name in
    let fields = FieldMap.filter (fun f -> not (is_static_field f)) (get_fields cls) in
    let add_field sign _value acc = 
        let ty = lltype_of_valuetype ctx (fs_type sign) in
        (sign, ty) :: acc
    in
    let field_info = FieldMap.fold add_field fields [] in

    let field_info = match get_super cls with
    | None -> (* only java.lang.Object has a slot for the v-pointer *)
        (vtable_field_sign, pointer_type ctx.context) :: field_info
    | Some super -> 
        let { ty=lparent; _ } = find_class_lltype ctx comp super in
        (parent_field_sign, lparent) :: field_info
    in

    let vtable = find_vtable ctx comp class_name in

    let field_types = List.map snd field_info in
    let field_info = List.mapi (fun i (sign, ty) -> 
        (sign, { index=i; ty; })
        ) field_info in
    
    let field_slots = Hashtbl.of_seq (List.to_seq field_info) in
    
    let name = safe_cn class_name in
    (* TODO: store indexes of each field *)
    let ll = named_struct_type ctx.context name in
    let is_packed = false in
    struct_set_body ll (Array.of_list field_types) is_packed;
    let v = { ty=ll; fields=field_slots; vtable } in
    Hashtbl.replace comp.structs class_name v;

    (* TODO: collect these clinits in a list and emit a new function that calls them all. order is scary tho. *)
    
    let has_static_block = defines_method cls clinit_ms in
    let hackhackhack = (cn_equal class_name javalangThrowable) in
    (if has_static_block && not hackhackhack then let _ = find_func ctx comp (make_cms class_name clinit_ms) in ());
    v

let rec get_field_ptr ctx comp obj classname sign =
    let { ty=obj_ty; fields; _ } = find_class_lltype ctx comp classname in
    let field = Hashtbl.find_opt fields sign in
    match field with
    | None -> (* the field was declared on a parent class. look up the chain. *)
        let cls = get_class_cached comp classname in
        let super = assert_super cls in
        (* alas, not a tail call *)
        let (parent_ptr, _) = get_field_ptr ctx comp obj classname parent_field_sign in
        get_field_ptr ctx comp parent_ptr super sign
        
    | Some field -> (* The field was declared by this class. *)
        let debug_field_name = fs_name sign in
        let ptr = build_struct_gep obj_ty obj field.index debug_field_name ctx.builder in
        (ptr, field.ty)


(* TODO: copy paste. vtable_lookup should call this. *)
let vfunc_lookup ctx comp (sign: class_method_signature) vptr: llvalue = 
    let (cs, ms) = cms_split sign in
    let vtable = find_vtable ctx comp cs in

    match Hashtbl.find_opt vtable.fields ms with
        | Some { index=field_index; _ } ->
            let field_ptr = build_struct_gep vtable.ty vptr field_index (ms_name ms) ctx.builder in
            let fptr = build_load (pointer_type ctx.context) field_ptr "vfunc" ctx.builder in 
            fptr

        | None ->  
            assert false (* should be doing lookup on base class so it should be the one declaring the method  *)
            
            

(* returns the function pointer to call *)
let vtable_lookup ctx comp (sign: class_method_signature) obj: llvalue = 
    let (cs, ms) = cms_split sign in
    let vtable = find_vtable ctx comp cs in

    match Hashtbl.find_opt vtable.fields ms with
        | Some { index=field_index; _ } ->
            let vsign = vtable_field_sign in
            (* let vptr_field_ptr = obj in This also works cause vtable is first field but feels kinda cringe somehow  *)
            let (vptr_field_ptr, vptrty) = get_field_ptr ctx comp obj cs vsign in
            
            (* if these fail you're probably not passing garbage as the obj parameter. reading the stack backwards? *)
            assert (address_space vptrty == 0);
            assert (vptrty == (type_of vptr_field_ptr)); 
            assert (address_space (type_of vptr_field_ptr) == 0);

            let vptr = build_load (pointer_type ctx.context) vptr_field_ptr "vptr" ctx.builder in
            let field_ptr = build_struct_gep vtable.ty vptr field_index (ms_name ms) ctx.builder in
            let fptr = build_load (pointer_type ctx.context) field_ptr "vfunc" ctx.builder in 
            fptr

        | None ->  
            assert false; (* should be doing lookup on base class so it should be the one declaring the method  *)
                    

(*== Emiting llvm ir for a single method ==*)


type phistate = Normal | DidPhi of llvalue * llbasicblock 

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

let total_seen = ref 0  (* TODO: can remove. was just curious *)

let convert_method ctx code current_cms comp = 
    let current_sign = snd (cms_split current_cms) in
    let is_static = is_static_cms comp current_cms in
    let func = find_func ctx comp current_cms in 
    let ret_ty = ms_rtype current_sign in

    let block_positions = find_basic_blocks code.c_code code.c_exc_tbl in
    if not (stack_comptime_safe block_positions code.c_code)
        then todo ("use rt stack. not stack_comptime_safe in " ^ (ms_name current_sign) ^ " [often we dont like (a ? b : c)]");

    let locals = alloc_locals ctx func code current_sign is_static in
    let basic_blocks = init_basic_blocks ctx func block_positions in
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
    let call_method_inner is_static func_value target_sign compstack = 
        let arg_count = List.length (ms_args target_sign) in
        let arg_count = if is_static then arg_count else arg_count + 1 in
        if arg_count > List.length compstack 
            then illegal ("stack underflow calling " ^ (ms_name target_sign) ^ " from " ^ (ms_name current_sign));
        let func_ty = llfunc_type ctx target_sign is_static in
        let (new_stack, arg_values) = pop_n compstack arg_count in
        let result = build_call func_ty func_value (Array.of_list arg_values) "" ctx.builder in
        if (ms_rtype target_sign) == None then new_stack else (result :: new_stack)
    in
    let call_method_direct is_static classname target_sign compstack = 
        let cms = make_cms classname target_sign in
        let func_value = find_func ctx comp cms in 
        call_method_inner is_static func_value target_sign compstack
    in
    let call_maybe_virtual classname target_sign compstack = 
        let obj_cls = get_class_cached comp classname in
        let overriding_class = match resolve_override comp classname target_sign with 
        | Some c -> c 
        | None -> 
            todo ("None resolve_override of " ^ (JPrint.class_method_signature (make_cms classname target_sign)));
    
        in
        let m = get_method (get_class_cached comp overriding_class) target_sign in

        if is_final_method m || is_final obj_cls then (* nobody can override OR nobody past us can override *)
            call_method_direct false overriding_class target_sign compstack
        else (* somebody past us might override, can't assume overriding_class, need runtime vtable. *)
            (* Any method overriding this must be emitted because it ~could~ be the one being called now. *)
            let base_class = find_base_declaration comp classname target_sign in
            let target_cms = make_cms base_class target_sign in
            Hashtbl.replace comp.virtually_called target_cms ();

            let arg_count = List.length (ms_args target_sign) in
            let objptr = List.nth compstack arg_count in

            let v_func_ptr = vtable_lookup ctx comp target_cms objptr in
            call_method_inner false v_func_ptr target_sign compstack
    in
    let make_new_uninit classname = (* You must call constructor manually! *)
        let {ty; vtable; _} = find_class_lltype ctx comp classname in
        let obj = build_malloc ty "" ctx.builder in
        let vptr_field = fst (get_field_ptr ctx comp obj classname (vtable_field_sign)) in
        let _ = build_store vtable.value_ptr vptr_field ctx.builder in
        obj
    in
    let emit_string_obj ctx comp s = 
        let str_index = match Hashtbl.find_opt comp.const_strings s with 
        | Some (i, _) -> i
        | None -> 
            (* TODO: do i need to deal with the modified utf8 thing? set the coder field or turn off compact strings (former pls) 
           tho it being a static field that only gets set in clinit means llvm can't constant fold it. should be a special case because strings are kinda important *)
            let len = String.length s in
            let len = const_int (i32_type ctx.context) len in
            let data = const_stringz ctx.context s in
            let data = define_global "str" data ctx.the_module in
            set_global_constant true data;
            let i = Hashtbl.length comp.const_strings in 
            let conststr = const_struct ctx.context (Array.of_list [data; const_null (pointer_type ctx.context); len]) in
            Hashtbl.replace comp.const_strings s (i, conststr);
            i
        in

        let { ty=str_struct; vtable; _;} = find_class_lltype ctx comp javalangString in
        let (value_field_offset, _) = get_field_ptr ctx comp (const_null (pointer_type ctx.context)) javalangString string_value_field_sign in
        let obj = fst (call_intin (FillStr) (List.rev [
            comp.string_pool; 
            const_int (i32_type ctx.context) str_index; 
            size_of str_struct; 
            size_of comp.string_const_ty;
            build_ptrtoint value_field_offset (i64_type ctx.context) "" ctx.builder;
            vtable.value_ptr;
            ])) in 
        
        let _ = call_intin AssertVptrOrNull [obj] in (* TODO: have a debug mode flag here beyond just disabling assertions? *)
        obj
    in
    let emit_const ctx comp (v: jconst): llvalue = 
        match v with
        | `Int n -> const_int (i32_type ctx.context) (Int32.to_int n)
        | (`Byte n) | (`Short n) -> const_int (i32_type ctx.context) n
        | `Long n -> const_int (i64_type ctx.context) (Int64.to_int n)
        | `Float n -> const_float (float_type ctx.context) n
        | `Double n -> const_float (double_type ctx.context) n
        | `String s -> emit_string_obj ctx comp (jstr_raw s)
        | `Class c -> 
            eprintf "warning emit const class %s\n" (JPrint.object_type c);
            const_null (pointer_type ctx.context)
        | `MethodType _m -> 
            eprintf "warning emit const MethodType\n";
            const_null (pointer_type ctx.context)
        | `MethodHandle _m -> 
            eprintf "warning emit const MethodHandle\n";
            const_null (pointer_type ctx.context)
        | `ANull -> 
            const_null (pointer_type ctx.context)
    in
    
    let emit_op compstack op index prev_op = 
        if op != OpInvalid then total_seen := !total_seen + 1;
        (* eprintf "[%d. %s] " index (JPrint.jopcode op); *)

        let end_block s = 
            assert (List.length s <= 1);
            s
        in

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
            end_block stack
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
                let zero = match kind with
                | `NonNull | `Null -> const_null (type_of (List.hd compstack)) 
                | _ -> const_int (type_of (List.hd compstack)) 0 
                in 
                cmp build_icmp (intcmp kind) (zero :: compstack)
            ) in
            let _ = build_cond_br c true_block false_block ctx.builder in
            end_block stack
        | OpGoto offset -> 
            let bb = Hashtbl.find basic_blocks (index + offset) in
            let _ = build_br bb ctx.builder in
            end_block compstack
        | OpReturn _ -> 
            (match ret_ty with
            | None -> 
                let _ = build_ret_void ctx.builder in
                end_block compstack
            | (Some (TObject _)) | (Some (TBasic (`Int | `Float | `Double | `Long))) ->
                let v = List.hd compstack in
                let _ = build_ret v ctx.builder in
                end_block (drop_fst compstack)
            | Some (TBasic b) -> 
                let stack = (match b with
                | `Short -> do_cast build_intcast i16_type compstack
                | `Byte -> do_cast build_intcast i8_type compstack
                | `Char -> do_cast build_intcast i16_type compstack
                | `Bool -> do_cast build_intcast i1_type compstack
                | _ -> unreachable ()
                ) in
                let _ = build_ret (List.hd stack) ctx.builder in
                end_block (drop_fst stack))
        | OpConst v -> (emit_const ctx comp v) :: compstack
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
            call_method_direct true classname target_sign compstack
        | OpInvoke ((`Special (_ioc, classname)), target_sign) -> (* direct call like constructors or super *)
            call_method_direct false classname target_sign compstack
        | OpInvoke ((`Virtual objty), target_sign) ->  
            let classname = match objty with
            | TClass cl -> cl
            | TArray _ -> todo "unreachable? virtual method call on an array."
            in
            call_maybe_virtual classname target_sign compstack
        
        | OpInvoke ((`Interface interface_cn), target_sign) -> 
            Hashtbl.replace comp.interface_called (make_cms interface_cn target_sign) ();

            let arg_count = List.length (ms_args target_sign) in
            let obj_data_ptr = List.nth compstack arg_count in

            let root_interface_vptr = (find_class_lltype ctx comp interface_cn).vtable.value_ptr in (* this points to a table of the abstract methods *)
            let obj_vptr_field_ptr = obj_data_ptr in  (* TODO: this relies on vtable being first field *)
            let obj_vptr = build_load (pointer_type ctx.context) obj_vptr_field_ptr "vptr" ctx.builder in
            let (obj_interface_vptr, _) = call_intin FindInterface [root_interface_vptr; obj_vptr] in

            let v_func_ptr = vfunc_lookup ctx comp (make_cms interface_cn target_sign) obj_interface_vptr in
            call_method_inner false v_func_ptr target_sign compstack
        
        | OpInvoke  ((`Dynamic bs_method), target_sign) -> 
            (match bs_method.bm_ref with
            | `InvokeStatic (`Method (cs, ms)) -> 
                assert (cn_equal cs javalanginvokeStringConcatFactory);
                assert (ms_name ms = "makeConcatWithConstants");

                (* TODO: read the argument recipe and deal with constants correctly *)
                (* eprintf "%s\n" (JPrint.jopcode op); *)

                let args = ms_args target_sign in
                assert (List.for_all (fun s -> 
                    match s with
                    | TObject (TClass cn) -> cn_equal cn javalangString
                    | _ -> false
                ) args); 
                let arg_count = List.length args in
                let (stack, strings) = pop_n compstack arg_count in

                let ptr_ty = pointer_type ctx.context in
                let str_arr_ty = array_type ptr_ty arg_count in
                let i32 = i32_type ctx.context in
                let arr = build_array_alloca str_arr_ty (const_int i32 arg_count) "" ctx.builder in
                
                List.iteri (fun i s -> (* TODO: null check *)
                    let (value_field_ptr, _) = get_field_ptr ctx comp s javalangString string_value_field_sign in
                    let value_arr = build_load ptr_ty value_field_ptr "" ctx.builder in
                    
                    let slot = build_gep str_arr_ty arr (Array.of_list [const_int i32 0; const_int i32 i;]) "" ctx.builder in
                    let _ = build_store value_arr slot ctx.builder in
                    ()
                ) strings;

                let result_value_arr = fst (call_intin ConcatStrings [ const_int i32 arg_count; arr; ]) in
                let obj = make_new_uninit javalangString in
                let (value_field_ptr, _) = get_field_ptr ctx comp obj javalangString string_value_field_sign in
                let _ = build_store result_value_arr value_field_ptr ctx.builder in
                
                obj :: stack
            | _ -> todo "more general invoke dynamic"
            )
            
        | OpNew classname -> (* TODO: call an intrinsic to register it with the garbage collector. *)
            make_new_uninit classname :: compstack
        
        | OpGetField (cls, sign) -> 
            let (ptr, ty) = get_field_ptr ctx comp (List.hd compstack) cls sign in
            let v = build_load ty ptr "" ctx.builder in
            v :: (drop_fst compstack)
        
        | OpPutField (cls, sign) -> 
            let obj = List.nth compstack 1 in
            let value = List.nth compstack 0 in
            let (ptr, _) = get_field_ptr ctx comp obj cls sign in
            let _ = build_store value ptr ctx.builder in
            drop_fst (drop_fst compstack)

        | (OpCmp _ ) -> (* really this produces a value but instead, use it as a modifier to the opif *)
            (* TODO: pass next_op to emit_op if i start using it for anything *)
            let next_op = Array.get code.c_code (index + 1) in 
            (match next_op with 
            | (OpIf _) -> compstack 
            | _ -> illegal "OpCmp must be followed by OpIf")

        | OpInstanceOf target_class_ty ->
            let target_class = (match target_class_ty with
                | TArray _ -> todo "instanceof array"
                | TClass c -> c
            ) in
            let target_vptr = (find_vtable ctx comp target_class).value_ptr in

            let obj = List.hd compstack in 
            let vptr_field_ptr = obj in (* TODO: this relies on vtable being first field *)
            let current_vptr = build_load (pointer_type ctx.context) vptr_field_ptr "vptr" ctx.builder in

            let _ = call_intin AssertVptrOrNull [obj] in
            let (result, _) = call_intin InstCheck [target_vptr; current_vptr] in
            result :: drop_fst compstack
            
        | OpCheckCast target_class_ty -> (* todo kinda copy paste from OpInstanceOf *)
            let target_class = (match target_class_ty with
                | TArray _ -> todo "OpCheckCast array"
                | TClass c -> c
            ) in
            let target_vptr = (find_vtable ctx comp target_class).value_ptr in

            let obj = List.hd compstack in
            let vptr_field_ptr = obj in  (* TODO: this relies on vtable being first field *)
            let current_vptr = build_load (pointer_type ctx.context) vptr_field_ptr "vptr" ctx.builder in
            let _ = call_intin AssertVptrOrNull [obj] in
            let _ = call_intin CheckCast [target_vptr; current_vptr] in
            compstack

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
        | OpPop -> drop_fst compstack


        | OpMonitorEnter | OpMonitorExit -> 
            (* eprintf "Warning: ignoring syncronised block in %s\n"  (JPrint.class_method_signature current_cms); *)
            drop_fst compstack
        
        | OpInvalid (* these slots are the arguments to previous instruction *)
        | OpNop -> compstack 
        | (OpRet _) | (OpJsr _ ) -> illegal ((JPrint.jopcode op) ^ " was deprecated in Java 7.")
        | OpBreakpoint -> illegal "OpBreakpoint is reserved"

        | OpThrow -> 
            let _ = (call_intin (LogThrow) compstack) in
            let _ = build_unreachable ctx.builder in (* TODO: this is wrong because it probably leaks undefined behaviour *)
            []

        | _ -> todo ("emit_op " ^ JPrint.jopcode op)
    in

    let block_ranges = sliding_pairs ((List.sort compare block_positions) @ [Array.length code.c_code]) in

    let emit_block stack (start, stop) dophi = 
        (* eprintf "\nemit block %d -> %d\n" start stop; *)
        let bb = Hashtbl.find basic_blocks start in (* entering a new block *)

        let current = insertion_block ctx.builder in

        (match block_terminator current with 
        | Some _ -> () (* last instruction was a jump *)
        | None -> 
            let _ = build_br bb ctx.builder in ()); (* this is the head of a loop so fallthrough *)
        
        position_at_end bb ctx.builder; (* either way start writing the new block*)


        let stack = (match dophi with
        | None -> stack
        | Some (a_v, a_b, b_v, b_b) -> (* last two blocks were part of a ternary operator s*)
            let inputs = [(a_v, a_b); (b_v, b_b); ] in
            let value_c = build_phi inputs "" ctx.builder in
            value_c :: stack) in

        let rec loop stack index prev_op = 
            let op = Array.get code.c_code index in
            let new_stack = emit_op stack op index prev_op in
            let last_good_op = (match op with
            | OpInvalid -> prev_op
            | other -> Some other) in
            if index < stop - 1 then loop new_stack (index + 1) last_good_op else new_stack
        in

        let s = loop stack start None in 


        (match dophi with
        | None -> ()
        | Some _ -> 
            assert (List.length s == 0));

        assert (List.length s <= 1);
        if List.is_empty s then Normal else DidPhi (List.hd s, bb)
    in 

    (* TODO: check that phi blocks are of the normal form *)
    let _ = (List.fold_left (fun (prev_state, dophi) block -> 
        (* Note: always starts with an empty stack. if this is a phi reciever, 
           it creates the value based on `dophi` as its first instruction *)
        let notCatchBlock = (List.find_opt (fun e -> e.e_handler = (fst block)) code.c_exc_tbl) == None in

        let stack = if notCatchBlock then [] else [const_null (pointer_type ctx.context)] in

        let next_state = emit_block stack block dophi in 
        (match prev_state with 
        | Normal -> next_state, None
        | DidPhi (a_v, a_b) -> 
            match next_state with
            | Normal -> assert false;
            | DidPhi (b_v, b_b) -> Normal, Some (a_v, a_b, b_v, b_b)
            )
        ) (Normal, None) block_ranges) in
    ()

(*== Walking classes to find code ==*)

let emit_method ctx comp m = 
    let sign = (match m with
    | AbstractMethod func -> (* has a vtable slot but cannot be called directly *)
        let sign = func.am_class_method_signature in
        assert ((Hashtbl.find_opt comp.funcs_done sign) == None);
        let func = find_func ctx comp sign in
        let entry = append_block ctx.context "entry_trap_abstract" func in 
        position_at_end entry ctx.builder;
        let _ = build_unreachable ctx.builder in
        sign
    | ConcreteMethod func ->
        let sign = func.cm_class_method_signature in
        (* eprintf "\n=== emit method %s ===\n" (JPrint.class_method_signature sign); *)
        assert ((Hashtbl.find_opt comp.funcs_done sign) == None);
        (match func.cm_implementation with
            | Native -> ()
            | Java code ->
                let jcode = Lazy.force code in
                convert_method ctx jcode sign comp
        );
        sign) 
    in
    
    let ll = Hashtbl.find comp.func_queue sign in
    Hashtbl.remove comp.func_queue sign;
    Hashtbl.replace comp.funcs_done sign ll;
    ()


let () = 
    let entry_classes = drop_fst (Array.to_list Sys.argv) in
    
    (* TODO: don't just hardcode the path lol *)
    (* TODO: past java 8 need to figure out what to do with a jmod file *)
    let classes = class_path "./test/java:./out/java:/Library/Java/JavaVirtualMachines/jdk1.8.0_202.jdk/Contents/Home/jre/lib/rt.jar" in (*  *)

    let c = create_context () in
    let ctx = { context = c; the_module = create_module c "javatest"; builder = builder c } in
    let string_const_ty = named_struct_type ctx.context "StringConst" in
    struct_set_body string_const_ty (Array.of_list [(pointer_type ctx.context); (pointer_type ctx.context); (i32_type ctx.context)]) false;
    let comp = { 
        classes;
        intrinsics = Hashtbl.create 0; 
        funcs_vtabled = Hashtbl.create 0; 
        func_queue = Hashtbl.create 0; 
        funcs_done = Hashtbl.create 0; 
        globals = Hashtbl.create 0; 
        structs = Hashtbl.create 0; 
        virtually_called = Hashtbl.create 0; 
        overload_count = Hashtbl.create 0; 
        wip_vtable_memo = Hashtbl.create 0;
        interface_called = Hashtbl.create 0;
        class_cache = Hashtbl.create 0;
        const_strings = Hashtbl.create 0;
        string_const_ty;
        string_pool = define_global "WipConstStringPool" (const_null (pointer_type ctx.context)) ctx.the_module;
    } in

    (* Treat all static methods in the class <name> as roots. TODO: ugly *)
    let reference_all name =
        let cls = get_class_cached comp (make_cn name) in
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
            let m = method_of_cms comp cms in
            emit_method ctx comp m;
            emit_next ()
    in
    emit_next ();
    assert (Hashtbl.length comp.func_queue == 0);
    
    let rec emit_trap_func () = 
        match take_any comp.funcs_vtabled with
        | None -> ()
        | Some (cms, func) -> 
            Hashtbl.remove comp.funcs_vtabled cms;
            let (cs, ms) = cms_split cms in
            let _ = match find_interface_declaration comp cs ms with 
            | Some interface -> 
                let called = not ((Hashtbl.find_opt comp.interface_called (make_cms interface ms)) = None) in
                if called then(
                    Hashtbl.replace comp.func_queue cms func;
                    emit_next ()) (* TODO: like below, this might call more interfaces or virtuals that you already emited traps*)
                else (
                    (* eprintf "WARNING: emit unreachable method stub for [%s] extends [%s] %b\n" (JPrint.class_method_signature cms) (JPrint.class_method_signature base_method) has_vcall; *)
                    let entry = append_block ctx.context "entry_trap_i" func in 
                    position_at_end entry ctx.builder;
                    let _ = build_unreachable ctx.builder in
                    ()
                )
            | None -> 
                let base_method = find_base_declaration comp cs ms in
                let base_method = (make_cms base_method ms) in
                let has_vcall = not (Hashtbl.find_opt comp.virtually_called base_method == None) in (* TODO: this needs to include invokeinterface *)
                if has_vcall then (
                    let vcalls = Hashtbl.length comp.virtually_called in
                    Hashtbl.replace comp.func_queue cms func;
                    emit_next ();
                    (* TODO: the method you just did might have added things to virtually_called that you already emitted a trap for *)
                    assert (vcalls == Hashtbl.length comp.virtually_called);
                    ()
                ) else (
                    (* eprintf "WARNING: emit unreachable method stub for [%s] extends [%s] %b\n" (JPrint.class_method_signature cms) (JPrint.class_method_signature base_method) has_vcall; *)
                    let entry = append_block ctx.context "entry_trap_c" func in 
                    position_at_end entry ctx.builder;
                    let _ = build_unreachable ctx.builder in
                    ()
                )
            in

            emit_trap_func ()
    in
    emit_trap_func ();

    let _count = Hashtbl.length comp.const_strings in
    let strings = List.of_seq (Hashtbl.to_seq comp.const_strings) in
    let strings = List.sort (fun (_, (a, _)) (_, (b, _)) -> compare a b) strings in
    let strings = List.map (fun (_, (_, v)) -> v) strings in
    let strings = const_array comp.string_const_ty (Array.of_list strings) in
    let strings = define_global "ConstStringPool" strings ctx.the_module in
    replace_all_uses_with comp.string_pool strings;

    let code = string_of_llmodule ctx.the_module in
    print_endline code;
    eprintf "Compiled %d bytecode instructions.\n" (!total_seen);

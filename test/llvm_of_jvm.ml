let time () = Unix.gettimeofday ()

let end_time start task = 
	print_endline ((string_of_int (int_of_float ((time () -. start) *. 1000.0))) ^ " ms: " ^ task)

let run cmd = 
	let start = time () in
	(match Unix.system cmd with
	| WEXITED 0 -> ()
	| _ -> assert false);
	end_time start cmd

let read_to_string filename =
	let ch = open_in_bin filename in
	let s = really_input_string ch (in_channel_length ch) in
	close_in ch;
	s

let write_string filename s =
	let ch = open_out_bin filename in
	output_string ch s;
	close_out ch

let replace before after s = 
	Str.global_replace (Str.regexp_string before) after s

let template f size ty = 
	let original = read_to_string ("test/templates/" ^ f ^ ".java") in
	let name = (replace "TYPE" ty f) in
	let new_name = "out/java/" ^ name ^ ".java" in
	let result = replace "TYPE" ty (replace "SIZE" size original) in
	write_string new_name result;
	name

(* TODO: make the dirs if not already *)
let do_templates () = 
    let start = time () in
    let ints = ["int"; "long"] in (*TODO: "byte"; "short";  smaller types need to do math as integers *)
    (* let sizes = ["1"; "2"; "4"; "8"] in *)
    let all = "double" :: "float" :: ints in
    let classes = List.map (template "PrimitiveTemplateTYPE" "?~?") all in
    (* List.iter2 (template "IntegerTemplateTYPE") sizes ints; TODO *)
    end_time start "Evaluate Templates";
    classes

let () = 
    assert (Sys.file_exists "./test/llvm_of_jvm.ml");  (* must run in correct working directory *)
    if not (Sys.file_exists "./out/java") then run "mkdir -p out/java";
	if not (Sys.file_exists "./out/skiptime.o") then run "gcc -c skiptime.c -o out/skiptime.o";

	(* run "dune build"; want to have `dune build --watch` anyway *)
	(* TODO: This is ugly. Need to tell it which entry points to convert. Eventually just start at main? *)
	let classes = "OpTest" :: do_templates () in
	let classes = String.concat " " classes in

	run "javac test/java/*.java out/java/*.java";

	(* run things twice just to see times because first run after compiling is much slower *)
	run ("./_build/default/bin/main.exe " ^ classes ^ " > out/test.ll");
	run ("./_build/default/bin/main.exe " ^ classes ^ " &> /dev/null");
	run "gcc -O2 test/entry.c out/test.ll runtime/runtime.c -o out/testbin";
	run "./out/testbin";
	run "./out/testbin";

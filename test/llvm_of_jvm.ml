let time () = Unix.gettimeofday ()

let end_time start task = 
	print_endline ((string_of_int (int_of_float ((time () -. start) *. 1000.0))) ^ " ms: " ^ task)

let run cmd = 
	let start = time () in
	(match Unix.system cmd with
	| WEXITED 0 -> ()
	| _ -> assert false);
	end_time start cmd

let do_templates () = 
	let read_to_string filename =
		let ch = open_in_bin filename in
		let s = really_input_string ch (in_channel_length ch) in
		close_in ch;
		s
	in
	let write_string filename s =
		let ch = open_out_bin filename in
		output_string ch s;
		close_out ch
	in
	let replace before after s = 
		Str.global_replace (Str.regexp_string before) after s
	in
	let template f size ty = 
		let original = read_to_string f in
		let new_name = "out/" ^ replace "TYPE" ty (replace ".txt" ".java" f) in
		let result = replace "TYPE" ty (replace "SIZE" size original) in
		write_string new_name result
	in
	let start = time () in
	
    let ints = ["byte"; "short"; "int"; "long"] in 
	let sizes = ["1"; "2"; "4"; "8"] in
    let all = "double" :: "float" :: ints in
	List.iter (template "java/PrimitiveTemplateTYPE.txt" "?~?") all;
	List.iter2 (template "java/IntegerTemplateTYPE.txt") sizes ints;
	end_time start "Evaluate Templates"

let () = 
	run "dune build";
	do_templates ();
	run "javac java/*.java out/java/*.java";
	run "./_build/default/bin/main.exe > out/test.ll";
	run "gcc runtime/test_prog.c out/test.ll runtime/runtime.c -o out/testbin";
	run "./out/testbin";

(lang dune 3.11)

(name llvm_of_jvm)

(generate_opam_files true)

(source
 (github LukeGrahamLandry/llvm_of_jvm))

(package
 (name llvm_of_jvm)
 (synopsis "Ahead of time compliation of java bytecode to llvm ir.")
 (depends ocaml dune llvm=16.0.6+nnp javalib=3.2.2))

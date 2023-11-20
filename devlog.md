## Getting Started 

- https://github.com/llvm/llvm-project/tree/main/llvm/bindings/ocaml
- https://github.com/javalib-team/javalib
- https://ocamlverse.net/content/quickstart_ocaml_project_dune.html

For some reason ocaml is like first class citizen with in-tree llvm bindings and i want to practise a functional-y language so might as well. 


- Build and run: `dune exec llvm_of_jvm`
- Make ide work: `dune build --watch`
- Add dependency: 
    - in `dune-project` `depends` section (`name=version`)
    - in individual lib/bin `dune` `libraries` section
    - open `Whatever` in .ml code (can be same or not as library name)
    - `opam install libname` (why doesnt build system do that? what am i missing? seems to be just in the local switch thingy tho which is good)


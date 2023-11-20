(lang dune 3.11)

(name llvm_of_jvm)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name llvm_of_jvm)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune llvm=16.0.6+nnp javalib=3.2.2)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

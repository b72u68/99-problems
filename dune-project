(lang dune 3.10)

(name ocaml_problems)

(generate_opam_files true)

(source
 (github b72u68/99-problems))

(authors "My Dinh")

(maintainers "My Dinh")

(package
 (name ocaml_problems)
 (synopsis "99 OCaml problems")
 (description "99 OCaml problems from https://ocaml.org/problems")
 (depends ocaml dune)
 (tags
  (topics "ocaml" "exercises")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

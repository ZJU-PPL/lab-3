(lang dune 3.3)

(name lab3)

(generate_opam_files true)

(authors "PPL2022 Your Name")
(maintainers "Your Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name lab3)
 (synopsis "Learn about type inference")
 (description "Learn about type inference")
 (depends (ocaml (>= 4.14.0)) dune base ppxlib menhir) 
 (tags
  (ppl lab)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

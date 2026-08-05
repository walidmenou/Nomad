(lang dune 3.20)

(name nomad)

(generate_opam_files true)

(source
 (github walidmenou/nomad))

(authors "Walid Menou <contact@walidmenou.com>")

(license LICENSE)

(package
 (name nomad)
 (synopsis "A functional language with borrow checking")
 (description
  "A small, strict, statically typed functional language in the ML family. Types are inferred and never written down. Arrays are updated in place under a borrow check, so a table costs what an array costs and still behaves as a value.")
 (depends
  ocaml
  (alcotest :with-test))
 (tags
  (language interpreter "borrow checking" "type inference")))

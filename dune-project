(lang dune 3.6)

(name iomux)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name iomux)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   dune-configurator
   alcotest :with_test)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

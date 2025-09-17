(lang dune 3.19)
(name iomux)
(generate_opam_files true)
(source
 (github ocaml-multicore/ocaml-iomux))
(authors "Christiano Haesbaert")
(maintainers "OCaml Multicore Team")
(license ISC)
(documentation https://ocaml-multicore.github.io/ocaml-iomux)

(package
 (name iomux)
 (synopsis "IO Multiplexer bindings")
 (description "Low level bindings for Unix IO Multiplexers (poll/ppoll/kevent/epoll)")
 (depends
   (ocaml (>= 4.08))
   dune
   dune-configurator
   kqueue
   (alcotest :with-test))
 (tags
  (io multiplexing poll ppoll epoll kevent kqueue)))

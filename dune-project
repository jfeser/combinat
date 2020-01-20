(lang dune 2.0)
(name combinat)
(source (github jfeser/combinat))
(documentation https://jfeser.github.io/combinat/combinat/index.html)
(license MIT)
(authors "Jack Feser <feser@mit.edu>")
(maintainers "Jack Feser <feser@mit.edu>")

(package
  (name combinat)
  (synopsis "Fast combinatorics for OCaml")
  (description "combinat provides fast iterators over combinatorial objects such as
permutations, combinations, and integer partitions.")
  (depends
    (ocaml (>= 4.07))
    (dune (and :build (>= 2)))
    (base (>= v0.12))
    (ppx_expect (and :with-test (>= v0.12)))
    (ppx_sexp_conv (and :with-test (>= v0.12)))
    (stdio (and :with-test (>= v0.12)))
    (core_bench (and :with-test (>= v0.12)))))

(generate_opam_files true)

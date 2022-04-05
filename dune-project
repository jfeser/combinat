(lang dune 2.7)
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
    (ocaml (>= 4.08))
    dune
    (core (and :with-test (>= v0.15)))
    (expect_test_helpers_core (and :with-test (>= v0.15)))
    (ppx_jane (and :with-test (>= v0.15)))
    (core_bench (and :with-test (>= v0.15)))
    (core_unix (and :with-test (>= v0.15)))))

(generate_opam_files true)

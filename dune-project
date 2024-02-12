(lang dune 3.13)

(name micrograd)

(generate_opam_files)

(license MIT)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin")

(source
 (github mbarbin/micrograd))

(documentation "https://mbarbin.github.io/micrograd/")

(package
 (name micrograd)
 (synopsis
  "A replication of karpathy/micrograd in OCaml for educational purposes")
 (depends
  (ocaml
   (>= 5.1))
  (appendable-list
   (and
    (>= v0.16)
    (< v0.17)))
  (base
   (and
    (>= v0.16)
    (< v0.17)))
  (bisect_ppx
   (and
    :dev
    (>= 2.8.3)))
  (expect_test_helpers_core
   (and
    :with-test
    (>= v0.16)
    (< v0.17)))
  (ppx_jane
   (and
    (>= v0.16)
    (< v0.17)))
  (ppx_js_style
   (and
    (>= v0.16)
    (< v0.17)))
  (torch
   (and
    (>= v0.16)
    (< v0.17)))))

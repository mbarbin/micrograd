(lang dune 3.17)

(name micrograd)

(generate_opam_files)

(license MIT)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/micrograd))

(documentation "https://mbarbin.github.io/micrograd/")

(implicit_transitive_deps false)

(package
 (name micrograd)
 (synopsis
  "A replication of karpathy/micrograd in OCaml for educational purposes")
 (depends
  (ocaml
   (>= 5.2))
  (appendable-list
   (>= v0.17))
  (base
   (>= v0.17))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))))

(package
 (name micrograd-tests)
 (synopsis "Tests for micrograd")
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (appendable-list
   (>= v0.17))
  (base
   (>= v0.17))
  (bisect_ppx
   (and
    :with-dev-setup
    (>= 2.8.3)))
  (expect_test_helpers_core
   (>= v0.17))
  (micrograd
   (= :version))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_js_style
   (and
    :with-dev-setup
    (>= v0.17)))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))

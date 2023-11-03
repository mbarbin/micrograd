(lang dune 3.11)

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
 (description
  "A replication of karpathy/micrograd in OCaml for educational purposes")
 (depends
  (ocaml
   (>= 5.1))
  (dune
   (>= 3.11))
  (appendable-list
   (and
    (>= v0.16)
    (< v0.17)))
  (base
   (and
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
  (stdio
   (and
    (>= v0.16)
    (< v0.17)))
  (torch
   (and
    (>= v0.16)
    (< v0.17)))))

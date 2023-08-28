open! Core
open! Micrograd

let%expect_test "value" =
  let a = Value.leaf (-4.) in
  let b = Value.leaf 2. in
  let c = Value.add a b in
  print_s [%sexp { c = (Value.data c : float) }];
  [%expect {| ((c -2)) |}];
  ()
;;

let%expect_test "karpathy" =
  let open Value.Infix in
  let a = Value.leaf (-4.0) in
  let b = Value.leaf 2.0 in
  let c = a + b in
  let d = (a * b) + (b ** 3) in
  let c = c + c + Value.leaf 1. in
  let c = c + Value.leaf 1. + c + Value.negate a in
  let d = d + (d * Value.leaf 2.) + Value.relu (b + a) in
  let d = d + (Value.leaf 3. * d) + Value.relu (b - a) in
  let e = c - d in
  let f = e ** 2 in
  let g = f / Value.leaf 2. in
  let g = g + (Value.leaf 10.0 / f) in
  print_s [%sexp { g = (Value.data g : float) }];
  [%expect {| ((g 24.704081632653061)) |}];
  Value.backward_propagation g;
  print_s [%sexp { grad_a = (Value.gradient a : float) }];
  [%expect {| ((grad_a -145.77551020408166)) |}];
  print_s [%sexp { grad_b = (Value.gradient b : float) }];
  [%expect {| ((grad_b -638.63556851311955)) |}];
  ()
;;

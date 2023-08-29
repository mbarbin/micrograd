open! Core
open! Micrograd

let%expect_test "Value.data" =
  let c =
    let open Value.Expression in
    let a = leaf (-4.) in
    let b = leaf 2. in
    a + b
  in
  print_s [%sexp { c = (Value.data c : float) }];
  [%expect {| ((c -2)) |}];
  ()
;;

let%expect_test "karpathy" =
  let f a b =
    let open Value.Expression in
    let a = leaf a in
    let b = leaf b in
    let c = a + b in
    let d = (a * b) + (b ** 3) in
    let c = c + c + leaf 1. in
    let c = c + leaf 1. + c + negate a in
    let d = d + (d * leaf 2.) + relu (b + a) in
    let d = d + (leaf 3. * d) + relu (b - a) in
    let e = c - d in
    let f = e ** 2 in
    let g = f / leaf 2. in
    let g = g + (leaf 10.0 / f) in
    a, b, g
  in
  let a, b, g = f (-4.) 2. in
  print_s [%sexp { g = (Value.data g : float) }];
  [%expect {| ((g 24.704081632653061)) |}];
  Value.run_backward_propagation g;
  let grad_a = Value.gradient a
  and grad_b = Value.gradient b in
  print_s [%sexp { grad_a : float }];
  [%expect {| ((grad_a -145.77551020408166)) |}];
  print_s [%sexp { grad_b : float }];
  [%expect {| ((grad_b -638.63556851311955)) |}];
  let _, _, g2 = f (-3.) 2. in
  print_s [%sexp { g2 = (Value.data g2 : float) }];
  [%expect {| ((g2 162.03086419753086)) |}];
  let _, _, g3 = f (-4.) 3. in
  print_s [%sexp { g3 = (Value.data g3 : float) }];
  [%expect {| ((g3 14964.500334124094)) |}];
  ()
;;

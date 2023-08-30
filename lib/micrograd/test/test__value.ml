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

let%expect_test "Value.gradient" =
  let epsilon = 0.001 in
  let test f x y =
    (* [f(x, y)] is a function of two variables for which we compute its
       gradient in x and y in a certain point. Then, we compare the computation
       of [f(x+e, y)] and [f(x, y+e)] using 2 different methods, one using the
       gradient, and one with a direct computation. We also compute an
       approximation of the gradient of x and y:

       - gx ~= [(f(x+e, y) - f(x, y)) / e]
       - gy ~= [(f(x, y+e) - f(x, y)) / e]
    *)
    let fxy = f x y in
    Value.run_backward_propagation fxy;
    let fxy = Value.data fxy in
    let gx = Value.gradient x
    and gy = Value.gradient y in
    let xe = Value.Expression.leaf (Value.data x +. epsilon) in
    let ye = Value.Expression.leaf (Value.data y +. epsilon) in
    let fxe = f xe y |> Value.data in
    let fye = f x ye |> Value.data in
    let fxe' = fxy +. (gx *. epsilon) in
    let fye' = fxy +. (gy *. epsilon) in
    let gx' = (fxe -. fxy) /. epsilon in
    let gy' = (fye -. fxy) /. epsilon in
    List.iter
      ~f:(fun s -> print_s s)
      [ [%sexp { fxy : float }]
      ; [%sexp { gx : float; gx' : float }]
      ; [%sexp { gy : float; gy' : float }]
      ; [%sexp { fxe : float; fxe' : float }]
      ; [%sexp { fye : float; fye' : float }]
      ]
  in
  let open Value.Expression in
  let f x y = x + y in
  test f (leaf 1.) (leaf 2.);
  [%expect
    {|
    ((fxy 3))
    ((gx 1) (gx' 0.99999999999988987))
    ((gy 1) (gy' 0.99999999999988987))
    ((fxe 3.001) (fxe' 3.001))
    ((fye 3.001) (fye' 3.001)) |}];
  let f x y = x - y in
  test f (leaf 1.) (leaf 2.);
  [%expect
    {|
    ((fxy -1))
    ((gx 1) (gx' 0.99999999999988987))
    ((gy -1) (gy' -0.99999999999988987))
    ((fxe -0.99900000000000011) (fxe' -0.999))
    ((fye -1.001) (fye' -1.001)) |}];
  let f x y = x + negate y in
  test f (leaf 1.) (leaf 2.);
  [%expect
    {|
    ((fxy -1))
    ((gx 1) (gx' 0.99999999999988987))
    ((gy -1) (gy' -0.99999999999988987))
    ((fxe -0.99900000000000011) (fxe' -0.999))
    ((fye -1.001) (fye' -1.001)) |}];
  let f x y = relu (x - y) in
  test f (leaf 1.) (leaf 2.);
  [%expect
    {|
      ((fxy 0))
      ((gx 0) (gx' 0))
      ((gy 0) (gy' 0))
      ((fxe 0) (fxe' 0))
      ((fye 0) (fye' 0)) |}];
  test f (leaf 1.) (leaf (-2.));
  [%expect
    {|
    ((fxy 3))
    ((gx 1) (gx' 0.99999999999988987))
    ((gy -1) (gy' -0.99999999999988987))
    ((fxe 3.001) (fxe' 3.001))
    ((fye 2.999) (fye' 2.999)) |}];
  let f x y = x * y in
  test f (leaf 12.) (leaf 7.);
  [%expect
    {|
    ((fxy 84))
    ((gx 7) (gx' 6.9999999999907914))
    ((gy 12) (gy' 12.000000000000455))
    ((fxe 84.006999999999991) (fxe' 84.007))
    ((fye 84.012) (fye' 84.012)) |}];
  let f x y = x / y in
  test f (leaf 12.) (leaf 7.);
  [%expect
    {|
    ((fxy 1.7142857142857142))
    ((gx 0.14285714285714285) (gx' 0.14285714285700024))
    ((gy -0.24489795918367346) (gy' -0.24486297875836449))
    ((fxe 1.7144285714285712) (fxe' 1.7144285714285714))
    ((fye 1.7140408513069558) (fye' 1.7140408163265306)) |}];
  let f x y = (x ** 3) + (y ** 2) in
  test f (leaf 12.) (leaf 7.);
  [%expect
    {|
    ((fxy 1777))
    ((gx 432) (gx' 432.0360009999149))
    ((gy 14) (gy' 14.00100000000748))
    ((fxe 1777.432036001) (fxe' 1777.432))
    ((fye 1777.014001) (fye' 1777.014)) |}];
  let f x y = tanh (x + y) in
  test f (leaf 0.5) (leaf 2.);
  [%expect
    {|
      ((fxy 0.98661429815143031))
      ((gx 0.026592226683160525) (gx' 0.026566007425010696))
      ((gy 0.026592226683160525) (gy' 0.026566007425010696))
      ((fxe 0.98664086415885532) (fxe' 0.98664089037811342))
      ((fye 0.98664086415885532) (fye' 0.98664089037811342)) |}];
  ()
;;

let%expect_test "karpathy's example" =
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
  [%expect {| ((grad_a 138.83381924198252)) |}];
  print_s [%sexp { grad_b : float }];
  [%expect {| ((grad_b 645.57725947521863)) |}];
  (* Compare with karpathy's expected values. *)
  let karpathy's_expected_values = "24.7041", "138.8338", "645.5773" in
  let () =
    let print4f f = Printf.sprintf "%.4f" f in
    let g4 = print4f (Value.data g)
    and ga4 = print4f grad_a
    and gb4 = print4f grad_b in
    print_s [%sexp { g4 : string; ga4 : string; gb4 : string }];
    if not
         ([%compare.equal: string * string * string]
            (g4, ga4, gb4)
            karpathy's_expected_values)
    then raise_s [%sexp "unexpected values"]
  in
  [%expect {| ((g4 24.7041) (ga4 138.8338) (gb4 645.5773)) |}];
  ()
;;

open! Torch

let get_float t =
  match Tensor.to_float0 t with
  | Some f -> f
  | None ->
    (match Tensor.to_float1 t with
     | Some f -> f.(0)
     | None -> raise_s [%sexp "get_float: tensor has no float value"] [@coverage off])
;;

let%expect_test "karpathy's neuron example 2" =
  let open Value.Expression in
  let x1 = leaf 2. in
  let x2 = leaf 0. in
  let w1 = leaf (-3.) in
  let w2 = leaf 1. in
  let b = leaf 6.8813735870195432 in
  let r = tanh ((x1 * w1) + (x2 * w2) + b) in
  print_s [%sexp (Value.data r : float)];
  [%expect {| 0.70710678118654768 |}];
  Value.run_backward_propagation r;
  print_s
    (let gradient x = x |> Value.gradient in
     [%sexp
       { gx1 = (gradient x1 : float)
       ; gx2 = (gradient x2 : float)
       ; gw1 = (gradient w1 : float)
       ; gw2 = (gradient w2 : float)
       }]);
  [%expect
    {|
    ((gx1 -1.4999999999999993)
     (gx2 0.49999999999999978)
     (gw1 0.99999999999999956)
     (gw2 0)) |}];
  let r_map, r_tensor = Value.tensor r in
  print_s [%sexp { shape = (Tensor.shape r_tensor : int list) }];
  [%expect {| ((shape ())) |}];
  Tensor.print r_tensor;
  [%expect {|
    0.707107
    [ CPUFloatType{} ] |}];
  print_s [%sexp (get_float r_tensor : float)];
  [%expect {| 0.70710670948028564 |}];
  let print_tensor_gradients () =
    let gradient x =
      x
      |> Value.Value_map.find r_map
      |> Option.value_exn ~here:[%here]
      |> Tensor.grad
      |> get_float
    in
    print_s
      [%sexp
        { gx1 = (gradient x1 : float)
        ; gx2 = (gradient x2 : float)
        ; gw1 = (gradient w1 : float)
        ; gw2 = (gradient w2 : float)
        }]
  in
  Tensor.backward r_tensor;
  print_tensor_gradients ();
  [%expect
    {|
    ((gx1 -1.5000003576278687)
     (gx2 0.50000011920928955)
     (gw1 1.0000002384185791)
     (gw2 0)) |}];
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
  let map, gt = Value.tensor g in
  Tensor.backward gt;
  let gt = gt |> Tensor.grad |> get_float in
  let gta =
    Value.Value_map.find map a
    |> Option.value_exn ~here:[%here]
    |> Tensor.grad
    |> get_float
  in
  let gtb =
    Value.Value_map.find map b
    |> Option.value_exn ~here:[%here]
    |> Tensor.grad
    |> get_float
  in
  (* Compare with karpathy's expected values. *)
  let module Expected_values = struct
    [@@@coverage off]

    type t = string * string * string [@@deriving equal, sexp_of]
  end
  in
  let karpathy's_expected_values : Expected_values.t =
    "24.7041", "138.8338", "645.5773"
  in
  let () =
    let print4f f = Printf.sprintf "%.4f" f in
    let g4 = print4f (Value.data g)
    and ga4 = print4f grad_a
    and gb4 = print4f grad_b in
    print_s [%sexp { g4 : string; ga4 : string; gb4 : string }];
    require_equal
      [%here]
      (module Expected_values)
      (g4, ga4, gb4)
      karpathy's_expected_values
  in
  [%expect {|
    ((g4  24.7041)
     (ga4 138.8338)
     (gb4 645.5773)) |}];
  let () =
    let print4f f = Printf.sprintf "%.4f" f in
    let g4 = print4f gt
    and ga4 = print4f gta
    and gb4 = print4f gtb in
    print_s [%sexp { g4 : string; ga4 : string; gb4 : string }];
    require_equal
      [%here]
      (module Expected_values)
      (g4, ga4, gb4)
      karpathy's_expected_values
  in
  [%expect {|
     ((g4  24.7041)
      (ga4 138.8338)
      (gb4 645.5773)) |}];
  ()
;;

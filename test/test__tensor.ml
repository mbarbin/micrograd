open! Torch

let get_float t =
  (* mbarbin: I couldn't yet find a way to access the float value of a 0-dim
     tensor, which is apparently what you get when building a tensor from a
     scalar). *)
  let t = Tensor.reshape t ~shape:[ 1 ] in
  Tensor.get_float1 t 0
;;

let%expect_test "karpathy's neuron example 2" =
  let open Value.With_tensor.Expression in
  let x1 = leaf 2. in
  let x2 = leaf 0. in
  let w1 = leaf (-3.) in
  let w2 = leaf 1. in
  let b = leaf 6.8813735870195432 in
  let rt = tanh ((x1 * w1) + (x2 * w2) + b) in
  let r = Value.With_tensor.value rt in
  print_s [%sexp (Value.data r : float)];
  [%expect {| 0.70710678118654768 |}];
  Value.run_backward_propagation r;
  print_s
    (let gradient x = x |> Value.With_tensor.value |> Value.gradient in
     [%sexp
       { gx1 = (gradient x1 : float)
       ; gx2 = (gradient x2 : float)
       ; gw1 = (gradient w1 : float)
       ; gw2 = (gradient w2 : float)
       }]);
  [%expect
    {|
    ((gx1 -1.4999999999999993) (gx2 0.49999999999999978)
     (gw1 0.99999999999999956) (gw2 0)) |}];
  let r_tensor = Value.With_tensor.tensor rt in
  print_s [%sexp { shape = (Tensor.shape r_tensor : int list) }];
  [%expect {| ((shape ())) |}];
  Tensor.print r_tensor;
  [%expect {|
    0.707107
    [ CPUFloatType{} ] |}];
  print_s [%sexp (get_float r_tensor : float)];
  [%expect {| 0.70710670948028564 |}];
  let print_tensor_gradients () =
    let gradient x = x |> Value.With_tensor.tensor |> Tensor.grad |> get_float in
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
    ((gx1 -1.5000003576278687) (gx2 0.50000011920928955) (gw1 1.0000002384185791)
     (gw2 0)) |}];
  ()
;;

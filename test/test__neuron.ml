let%expect_test "apply" =
  let n1 = Neuron.create ~num_inputs:2 ~linear:true in
  let n2 = Neuron.create ~num_inputs:2 ~linear:false in
  Value.update_data n1.weights.(0) ~f:(fun _ -> -0.5);
  Value.update_data n1.weights.(1) ~f:(fun _ -> 0.5);
  Value.update_data n2.weights.(0) ~f:(fun _ -> -0.5);
  Value.update_data n2.weights.(1) ~f:(fun _ -> 0.5);
  let v1 = Neuron.apply n1 ~inputs:Value.Expression.[| leaf 1.0; leaf (-2.0) |] in
  let v2 = Neuron.apply n2 ~inputs:Value.Expression.[| leaf 1.0; leaf (-2.0) |] in
  Value.run_forward v1;
  Value.run_forward v2;
  print_s [%sexp { v1 = (Value.data v1 : float); v2 = (Value.data v2 : float) }];
  [%expect
    {|
  ((v1 -0.9051482536448664)
   (v2 0)) |}];
  ()
;;

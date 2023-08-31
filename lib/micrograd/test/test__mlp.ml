open! Core
open! Micrograd

let example =
  (* [ [inputs], desired output ] *)
  [ [ 2.; 3.; -1. ], 1.
  ; [ 3.; -1.; 0.5 ], -1.
  ; [ 0.5; 1.; 1. ], -1.
  ; [ 1.; 1.; -1. ], 1.
  ]
  |> List.map ~f:(fun (inputs, desired_target) ->
    let inputs = inputs |> Array.of_list_map ~f:Value.Expression.leaf in
    inputs, desired_target)
;;

let%expect_test "learning" =
  let mlp = Mlp.create ~num_inputs:3 ~num_outputs:1 ~hidden_layers:[ 4; 4 ] in
  let prediction =
    List.map example ~f:(fun (inputs, _desired_target) -> (Mlp.apply mlp ~inputs).(0))
  in
  let loss =
    let open Value.Expression in
    List.map2_exn example prediction ~f:(fun (_, desired_target) output ->
      (output - leaf desired_target) ** 2)
    |> List.reduce ~f:( + )
    |> Option.value_exn ~here:[%here]
  in
  let print_loss () =
    let prediction = List.map prediction ~f:Value.data in
    let loss = Value.data loss in
    print_s [%sexp { prediction : float list; loss : float }]
  in
  print_loss ();
  [%expect
    {|
      ((prediction (0.12933227507965742 0.388781565773137 0 0.056035278540874679))
       (loss 4.5778459200086559)) |}];
  Value.run_backward_propagation loss;
  let ws =
    Array.mapi mlp.layers ~f:(fun i layer ->
      let neurons =
        Array.mapi layer.neurons ~f:(fun i neuron ->
          [%sexp
            { neuron = (i : int)
            ; ws =
                (Array.map neuron.weights ~f:(fun w -> Value.data w, Value.gradient w)
                  : (float * float) array)
            ; b = ((Value.data neuron.bias, Value.gradient neuron.bias) : float * float)
            }])
      in
      [%sexp { layer = (i : int) }, (neurons : Sexp.t array)])
  in
  print_s [%sexp (ws : Sexp.t array)];
  [%expect
    {|
    ((((layer 0))
      (((neuron 0)
        (ws
         ((0.27070282463351214 0.5861283958514466)
          (0.56510093449375964 0.073345568693581942)
          (0.54417517621722777 0.039099244256986673)))
        (b (0 0.18850284919627294)))
       ((neuron 1)
        (ws
         ((-0.025689636291400308 -0.71698411503613257)
          (0.10803504458667734 -1.054355733289321)
          (-0.82259676421212446 0.37961249678294412)))
        (b (0 -0.37961249678294412)))
       ((neuron 2)
        (ws
         ((0.36473830945288621 0.17215343494104574)
          (-0.94439733586942276 -0.057384478313681909)
          (-0.49180448134078891 0.028692239156840955)))
        (b (0 0.057384478313681909)))
       ((neuron 3)
        (ws
         ((-0.41892762563833519 -0.40482548204214536)
          (0.52761480068130484 -0.60723822306321806)
          (0.40699996206643529 0.20241274102107268)))
        (b (0 -0.20241274102107268)))))
     (((layer 1))
      (((neuron 0)
        (ws
         ((0.2850340163541778 -0.45071079357212762)
          (-0.20557775764047836 -0.67127614151575954)
          (0.77493257479855537 0.6635457243491818)
          (0.9903620865549434 -0.27368360905544287)))
        (b (0 -0.13895390092100834)))
       ((neuron 1)
        (ws
         ((-0.12796852321265328 -0.008871616923477401)
          (0.4620677759444245 -0.028241745954512669)
          (0.11010826593335254 0.025812256126451109)
          (0.37439673266422346 -0.0056491278219272757)))
        (b (0 -0.01007786312500079)))
       ((neuron 2)
        (ws
         ((0.948942194415308 0.99368654097756171)
          (-0.99877724153745706 0.84943346170305034) (-0.48240378298270548 0)
          (0.5837479564158663 0.34631949666259859)))
        (b (0 1.0633701334684349)))
       ((neuron 3)
        (ws
         ((0.58718700536427293 -0.0775726108257191)
          (0.70603344956869862 -0.11900537931590832) (-0.54031389887588 0)
          (0.17059758782553458 -0.016098009565369192)))
        (b (0 -0.1600751059273329)))))
     (((layer 2))
      (((neuron 0)
        (ws
         ((0.58476160504891883 0.96618800140917649)
          (0.011593830416168904 -1.0476622086184491)
          (-0.670676044257005 -0.46574386583408961)
          (0.089793595423356631 -2.516202625128142)))
        (b (0 -1.2364784566595235)))))) |}];
  let parameters = Mlp.parameters mlp in
  let optimization_step () =
    let epsilon = 0.01 in
    Appendable_list.iter parameters ~f:(fun parameter ->
      Value.update_data parameter ~f:(fun data ->
        data -. (epsilon *. Value.gradient parameter)));
    Value.run_forward loss;
    Value.run_backward_propagation loss
  in
  optimization_step ();
  print_loss ();
  [%expect
    {|
    ((prediction
      (0.19937542402496783 0.39205826710453479 0 0.086684201988406553))
     (loss 4.4129716775668344)) |}];
  for _ = 1 to 30 do
    optimization_step ()
  done;
  print_loss ();
  [%expect {| ((prediction (0 0 0 0)) (loss 4)) |}];
  ()
;;

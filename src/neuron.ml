type t =
  { num_inputs : int
  ; weights : Value.t array
  ; bias : Value.t
  ; linear : bool
  }

let create ~num_inputs ~linear =
  let weights =
    Array.init num_inputs ~f:(fun _ ->
      Value.Expression.leaf (Random.float_range (-1.) 1.))
  in
  let bias = Value.Expression.leaf 0. in
  { num_inputs; weights; bias; linear }
;;

let apply t ~inputs:x =
  if Array.length x <> t.num_inputs
  then
    raise_s
      [%sexp
        "Unexpected input length"
        , { x = (Array.map x ~f:Value.data : float array)
          ; x_length = (Array.length x : int)
          ; t_num_inputs = (t.num_inputs : int)
          }];
  let open Value.Expression in
  let wx =
    Array.map2_exn t.weights x ~f:(fun w x -> Value.Expression.(w * x))
    |> Array.reduce_exn ~f:( + )
  in
  let sum = tanh (wx + t.bias) in
  if t.linear then sum else relu sum
;;

let parameters t =
  Appendable_list.(append ([ t.bias ] |> of_list) (t.weights |> Array.to_list |> of_list))
;;

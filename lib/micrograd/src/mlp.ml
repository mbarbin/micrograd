open! Core

type t = { layers : Layer.t array }

let create ~num_inputs ~num_outputs ~hidden_layers =
  let shape = Array.of_list ((num_inputs :: hidden_layers) @ [ num_outputs ]) in
  let layers =
    let num_layers = Array.length shape - 1 in
    Array.init num_layers ~f:(fun i ->
      Layer.create
        ~num_inputs:shape.(i)
        ~num_outputs:shape.(i + 1)
        ~linear:(i = num_layers))
  in
  { layers }
;;

let apply t ~inputs:x =
  let x = Array.map x ~f:Value.Expression.leaf in
  Array.fold t.layers ~init:x ~f:(fun acc layer ->
    Layer.apply layer ~inputs:(acc |> Array.map ~f:Value.data))
;;

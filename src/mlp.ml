open! Base

type t = { layers : Layer.t array }

let create ~num_inputs ~num_outputs ~hidden_layers =
  let shape = Array.of_list ((num_inputs :: hidden_layers) @ [ num_outputs ]) in
  let layers =
    let num_layers = Array.length shape - 1 in
    Array.init num_layers ~f:(fun i ->
      Layer.create ~num_inputs:shape.(i) ~num_outputs:shape.(i + 1) ~linear:true)
  in
  { layers }
;;

let apply t ~inputs:x =
  Array.fold t.layers ~init:x ~f:(fun x layer -> Layer.apply layer ~inputs:x)
;;

let parameters t =
  Array.fold t.layers ~init:Appendable_list.empty ~f:(fun acc layer ->
    Appendable_list.append acc (Layer.parameters layer))
;;

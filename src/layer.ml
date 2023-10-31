type t = { neurons : Neuron.t array }

let create ~num_inputs ~num_outputs ~linear =
  let neurons = Array.init num_outputs ~f:(fun _ -> Neuron.create ~num_inputs ~linear) in
  { neurons }
;;

let apply t ~inputs:x =
  Array.map t.neurons ~f:(fun neuron -> Neuron.apply neuron ~inputs:x)
;;

let parameters t =
  Array.fold t.neurons ~init:Appendable_list.empty ~f:(fun acc neuron ->
    Appendable_list.append acc (Neuron.parameters neuron))
;;

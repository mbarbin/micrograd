open! Core

type t = { neurons : Neuron.t array }

let create ~num_inputs ~num_outputs ~linear =
  let neurons = Array.init num_outputs ~f:(fun _ -> Neuron.create ~num_inputs ~linear) in
  { neurons }
;;

let apply t ~inputs:x =
  Array.map t.neurons ~f:(fun neuron -> Neuron.apply neuron ~inputs:x)
;;

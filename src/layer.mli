open! Base

type t = private { neurons : Neuron.t array }

val create : num_inputs:int -> num_outputs:int -> linear:bool -> t

(** Apply a layer to a vector of scalar inputs. Will raise if the input is not
    of the expected size (num_inputs). This returns an array of size
    [num_outputs]. *)
val apply : t -> inputs:Value.t array -> Value.t array

val parameters : t -> Value.t Appendable_list.t

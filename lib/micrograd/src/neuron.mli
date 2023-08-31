open! Core

type t

val create : num_inputs:int -> linear:bool -> t

(** Apply a neuron to a array of scalar inputs. Will raise if the input is not
    of the expected size (num_inputs). *)
val apply : t -> inputs:float array -> Value.t

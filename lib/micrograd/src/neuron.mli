open! Core

type t = private
  { num_inputs : int
  ; weights : Value.t array
  ; bias : Value.t
  ; linear : bool
  }

val create : num_inputs:int -> linear:bool -> t

(** Apply a neuron to a array of scalar inputs. Will raise if the input is not
    of the expected size (num_inputs). *)
val apply : t -> inputs:Value.t array -> Value.t

val parameters : t -> Value.t Appendable_list.t

open! Core

type t [@@deriving sexp_of]

(** {1 Build expressions} *)

module Expression : sig
  val leaf : float -> t
  val negate : t -> t
  val relu : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> int -> t
end

(** {1 Micrograd operations} *)

val data : t -> float
val gradient : t -> float
val run_backward_propagation : t -> unit

open! Core

type t [@@deriving sexp_of]

(** {1 Build expressions} *)

val leaf : float -> t
val add : t -> t -> t
val sub : t -> t -> t
val power : t -> int -> t
val multiply : t -> t -> t
val divide : t -> t -> t
val negate : t -> t
val relu : t -> t

module Infix : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> int -> t
end

(** {1 Micrograd operations} *)

val data : t -> float
val gradient : t -> float
val backward_propagation : t -> unit

open! Core

type t [@@deriving sexp_of]

(** {1 Build expressions} *)

module Expression : sig
  val leaf : float -> t
  val negate : t -> t
  val relu : t -> t
  val tanh : t -> t
  val exp : t -> t
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

(** Assuming some leaves data have changed, recompute then entire graph. *)
val run_forward : t -> unit

(** Update the data contained in a [leaf] node. This function will raise if [t]
    was not built with [leaf]. After updating leaf data, the values must be
    recomputed with a [run_forward]. *)
val update_data : t -> f:(float -> float) -> unit

(** {1 Interacting with Torch} *)

val tensor : t -> Torch.Tensor.t

module With_tensor : sig
    (** Building both values and tensors terms at the same time. *)

    type t
    type value

    module Expression : sig
      val leaf : float -> t
      val negate : t -> t
      val relu : t -> t
      val tanh : t -> t
      val exp : t -> t
      val ( + ) : t -> t -> t
      val ( - ) : t -> t -> t
      val ( * ) : t -> t -> t
      val ( / ) : t -> t -> t
      val ( ** ) : t -> int -> t
    end

    val value : t -> value
    val tensor : t -> Torch.Tensor.t
  end
  with type value := t

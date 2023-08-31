open! Core

type +'a t

val iter : 'a t -> f:('a -> unit) -> unit
val of_list : 'a list -> 'a t
val empty : 'a t
val append : 'a t -> 'a t -> 'a t

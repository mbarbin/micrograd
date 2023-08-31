open! Core

type +'a t =
  | List of 'a list
  | Append of 'a t * 'a t

let rec iter t ~f =
  match t with
  | List list -> List.iter list ~f
  | Append (t1, t2) ->
    iter t1 ~f;
    iter t2 ~f
;;

let of_list list = List list
let empty = List []
let append t1 t2 = Append (t1, t2)

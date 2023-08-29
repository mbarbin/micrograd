open! Core

type t =
  { data : float
  ; mutable gradient : float
  ; node : node
  ; id : int
  }

and node =
  | Leaf of float
  | Add of t * t
  | Multiply of t * t
  | Power of t * int
  | Negate of t
  | Relu of t
[@@deriving sexp_of]

let compute_data node =
  match (node : node) with
  | Leaf f -> f
  | Add (t1, t2) -> t1.data +. t2.data
  | Multiply (t1, t2) -> t1.data *. t2.data
  | Power (t, n) -> Float.int_pow t.data n
  | Negate t -> -1. *. t.data
  | Relu t -> Float.max 0. t.data
;;

let children node =
  let two t1 t2 = if phys_equal t1 t2 then [ t1 ] else [ t1; t2 ] in
  match (node : node) with
  | Leaf (_ : float) -> []
  | Add (t1, t2) | Multiply (t1, t2) -> two t1 t2
  | Power (t, (_ : int)) | Negate t | Relu t -> [ t ]
;;

let add_gradient t value = t.gradient <- t.gradient +. value

let gradient_step t =
  match t.node with
  | Leaf _ -> ()
  | Add (t1, t2) ->
    add_gradient t1 t.gradient;
    add_gradient t2 t.gradient;
    ()
  | Multiply (t1, t2) ->
    add_gradient t1 (t.gradient *. t2.data);
    add_gradient t2 (t.gradient *. t1.data);
    ()
  | Power (t1, n) ->
    add_gradient t1 (Float.of_int n *. Float.int_pow t1.data (n - 1) *. t.gradient)
  | Negate t1 -> add_gradient t1 (-.t.gradient)
  | Relu t1 -> add_gradient t1 (if Float.(t.data > 0.) then t.gradient else 0.)
;;

let next_id =
  let v = ref (-1) in
  fun () ->
    incr v;
    !v
;;

let make node = { data = compute_data node; gradient = 0.; node; id = next_id () }
let leaf data = make (Leaf data)
let add t1 t2 = make (Add (t1, t2))

let sub t1 t2 =
  let t2' = make (Negate t2) in
  make (Add (t1, t2'))
;;

let multiply t1 t2 = make (Multiply (t1, t2))

let divide t1 t2 =
  let t2' = make (Power (t2, -1)) in
  make (Multiply (t1, t2'))
;;

let power t n = make (Power (t, n))
let negate t = make (Negate t)
let relu t = make (Relu t)

module Expression = struct
  let leaf = leaf
  let relu = relu
  let negate = negate
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = multiply
  let ( / ) = divide
  let ( ** ) = power
end

let run_backward_propagation t =
  let stack = ref [] in
  let visited = Hash_set.create (module Int) in
  let rec visit (t : t) =
    if not (Hash_set.mem visited t.id)
    then (
      Hash_set.strict_add_exn visited t.id;
      List.iter (children t.node) ~f:visit;
      stack := t :: !stack)
  in
  visit t;
  t.gradient <- 1.;
  List.iter !stack ~f:gradient_step
;;

let gradient t = t.gradient
let data t = t.data

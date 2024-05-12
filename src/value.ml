module T = struct
  [@@@coverage off]

  type t =
    { mutable data : float
    ; mutable gradient : float
    ; node : node
    ; id : int
    }

  and node =
    | Leaf of { mutable parameter : float }
    | Add of t * t
    | Multiply of t * t
    | Power of t * int
    | Negate of t
    | Relu of t
    | Tanh of t
    | Exp of t
  [@@deriving sexp_of]
end

include T

let compute_data node =
  match (node : node) with
  | Leaf f -> f.parameter
  | Add (t1, t2) -> t1.data +. t2.data
  | Multiply (t1, t2) -> t1.data *. t2.data
  | Power (t, n) -> Float.int_pow t.data n
  | Negate t -> -1. *. t.data
  | Relu t -> Float.max 0. t.data
  | Tanh t -> Float.tanh t.data
  | Exp t -> Float.exp t.data
;;

let update_data t ~f =
  match t.node with
  | Leaf t -> t.parameter <- f t.parameter
  | Add _ | Multiply _ | Power _ | Negate _ | Relu _ | Tanh _ | Exp _ ->
    raise_s [%sexp "Cannot update value since it is not a leaf", (t : t)] [@coverage off]
;;

module Expression = struct
  let next_id =
    let v = ref (-1) in
    fun () ->
      Int.incr v;
      !v
  ;;

  let make node = { data = compute_data node; gradient = 0.; node; id = next_id () }
  let leaf data = make (Leaf { parameter = data })
  let relu t = make (Relu t)
  let negate t = make (Negate t)
  let tanh t = make (Tanh t)
  let exp t = make (Exp t)
  let ( + ) t1 t2 = make (Add (t1, t2))

  let ( - ) t1 t2 =
    let t2' = make (Negate t2) in
    make (Add (t1, t2'))
  ;;

  let ( * ) t1 t2 = make (Multiply (t1, t2))

  let ( / ) t1 t2 =
    let t2' = make (Power (t2, -1)) in
    make (Multiply (t1, t2'))
  ;;

  let ( ** ) t n = make (Power (t, n))
end

let children node =
  let two t1 t2 = if phys_equal t1 t2 then [ t1 ] else [ t1; t2 ] in
  match (node : node) with
  | Leaf { parameter = (_ : float) } -> []
  | Add (t1, t2) | Multiply (t1, t2) -> two t1 t2
  | Power (t, (_ : int)) | Negate t | Relu t | Tanh t | Exp t -> [ t ]
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
  | Tanh t1 -> add_gradient t1 (t.gradient *. (1. -. Float.square t.data))
  | Exp t1 -> add_gradient t1 (t.gradient *. t.data)
;;

let topological_sort t =
  let queue = Queue.create () in
  let visited = Hash_set.create (module Int) in
  let rec visit (t : t) =
    if not (Hash_set.mem visited t.id)
    then (
      Hash_set.strict_add_exn visited t.id;
      List.iter (children t.node) ~f:visit;
      Queue.enqueue queue t)
  in
  visit t;
  Queue.to_list queue
;;

let run_forward t =
  List.iter (topological_sort t) ~f:(fun t -> t.data <- compute_data t.node)
;;

let run_backward_propagation t =
  let order = topological_sort t |> List.rev in
  List.iter order ~f:(fun t -> t.gradient <- 0.);
  t.gradient <- 1.;
  List.iter order ~f:gradient_step
;;

let gradient t = t.gradient
let data t = t.data

module Value_map = struct
  type 'a t = 'a Map.M(Int).t

  let find t value = Map.find t value.id
end

(* I ran into issue trying to use [Tensor.float_power_], such as in:

   {v
     let tensor_power t n = Tensor.float_power_ t ~exponent:(Scalar.int n)]
   v}

   Thus, implemented the following function to compute the power of a tensor.
*)
let tensor_power t ~exponent:n =
  let open Torch in
  let rec aux t n =
    let tensor =
      if n = 0
      then Tensor.f 1.
      else if n = 1
      then t
      else (
        let n_over_2 = n / 2 in
        let t_squared = Tensor.(set_requires_grad (t * t) ~r:true) in
        if n % 2 = 0 then aux t_squared n_over_2 else Tensor.(t * aux t_squared n_over_2))
    in
    Tensor.set_requires_grad tensor ~r:true
  in
  aux t n
;;

let tensor t =
  let value_table = Hashtbl.create (module Int) in
  let rec tensor t =
    let open Torch in
    let tensor =
      match t.node with
      | Leaf f -> Tensor.f f.parameter
      | Add (t1, t2) -> Tensor.(tensor t1 + tensor t2)
      | Multiply (t1, t2) -> Tensor.(tensor t1 * tensor t2)
      | Power (t, n) -> tensor_power (tensor t) ~exponent:n
      | Negate t -> Tensor.(~-(tensor t))
      | Relu t -> Tensor.relu (tensor t)
      | Tanh t -> Tensor.tanh (tensor t)
      | Exp t -> Tensor.exp (tensor t)
    in
    let tensor = Tensor.set_requires_grad tensor ~r:true in
    Hashtbl.set value_table ~key:t.id ~data:tensor;
    tensor
  in
  let tensor = tensor t in
  Hashtbl.to_alist value_table |> Map.of_alist_exn (module Int), tensor
;;

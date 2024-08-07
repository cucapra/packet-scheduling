type t =
  | Star
  | Node of t list

type addr = int list
type map = addr -> addr Option.t

val of_policy : Frontend.Policy.t -> t
val size : t -> int
val lift_tilde : map -> t -> Path.t -> Path.t
val build_d_ary : int -> t -> t * map

type t = Star | Node of t list

val of_policy : Frontend.Policy.t -> t
val size : t -> int

(* A few topologies to play with. *)
val one_level_quaternary : t
val one_level_ternary : t
val one_level_binary : t
val two_level_binary : t
val two_level_ternary : t
val three_level_ternary : t
val irregular : t
val flat_four : t

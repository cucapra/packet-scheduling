type t = Star | Node of t list

val of_policy : Frontend.Policy.t -> t
val size : t -> int

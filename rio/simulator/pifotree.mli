type 'a t

type path =
  | Foot of Rank.t
  | Path of int * Rank.t * path

val pop : 'a t -> ('a * 'a t) option
val push : 'a t -> 'a -> path -> 'a t
val size : 'a t -> int
val create : 'b Topo.t -> 'a t
val to_topo : 'a t -> 'b Topo.t

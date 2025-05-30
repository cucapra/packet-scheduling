type ('a, 'b) t

type order =
  | Foot
  | Order of (order * Rank.t) list (* ranks must be unique *)

val pop : ('a, 'b) t -> order -> ('a * ('a, 'b) t) option
val push : ('a, 'b) t -> 'a -> Rank.t -> ('a, 'b) t
val size : ('a, 'b) t -> int
val create : 'b Topo.t -> ('a -> 'b) -> ('a, 'b) t
val to_topo : ('a, 'b) t -> 'b Topo.t

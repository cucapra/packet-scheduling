type 'a t

val pop : 'a t -> ('a * 'a t) option
val push : 'a t -> 'a -> Path.t -> 'a t
val size : 'a t -> int
val create : 'b Topo.t -> 'a t
val to_topo : 'a t -> 'b Topo.t

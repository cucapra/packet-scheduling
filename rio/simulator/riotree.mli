type t

val pop : t -> Ordtree.t -> (Packet.t * t) option
val push : t -> Packet.t -> Rank.t -> t
val size : t -> int
val create : Topo.t -> t
val to_topo : t -> Topo.t

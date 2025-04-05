type t

val pop : t -> (Packet.t * t) option
val push : t -> Packet.t -> Path.t -> t
val size : t -> int
val create : Topo.t -> t
val to_topo : t -> Topo.t

type t

val cmp : t -> t -> int
val create : float -> Time.t -> t
val create_for_pkt : float -> Packet.t -> t

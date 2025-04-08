type t

val cmp : t -> t -> int
val create : float -> t
val create_for_pkt : float -> Packet.t -> t
val to_string : t -> string

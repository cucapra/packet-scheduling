module type Control = sig
  type t

  val init : t
  val push : t -> Packet.t -> t
  val pop : t -> (Packet.t * t) option
end

module type Policy = sig
  val policy : Frontend.Policy.t
end

module PIFOControl (_ : Policy) : Control

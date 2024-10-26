module type Packet = sig
  type t
  type ord

  val compare : ord -> ord -> int
  val rank : t -> ord
  val time : t -> ord
  val weight : t -> ord
end

(* An implementation for packets (see MLI) *)
module PacketImpl : Packet = struct
  type t = float * float * float
  type ord = float

  let compare = compare
  let rank (r, _, _) = r
  let time (_, t, _) = t
  let weight (_, _, w) = w
end

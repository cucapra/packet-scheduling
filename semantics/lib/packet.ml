(** A signature for packets. *)

module type Packet = sig
  (* A type for packets *)
  type t

  (* An ordered type *)
  type ord

  val compare : ord -> ord -> int

  (* rank pkt is the rank of pkt *)
  val rank : t -> ord

  (* time pkt is the pop deadline of pkt *)
  val time : t -> ord

  (* weight pkt is the weight pkt *)
  val weight : t -> ord
end

(* An implementation for packets *)
module PacketImpl :
  Packet with type t = float * float * float and type ord = float = struct
  type t = float * float * float
  type ord = float

  let compare = compare
  let rank (r, _, _) = r
  let time (_, t, _) = t
  let weight (_, _, w) = w
end

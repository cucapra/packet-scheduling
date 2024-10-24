(** A signature for packets. *)

module type Packet = sig
  type t

  (* rank pkt is the rank of pkt *)
  val rank : t -> float

  (* time pkt is the pop deadline of pkt *)
  val time : t -> float

  (* weight pkt is the weight pkt *)
  val weight : t -> float
end

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

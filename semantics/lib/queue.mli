(** A signature for queues. *)

module type Queue = sig
  type elt

  (* An abstract type for a queue with elements of type elt *)
  type t

  (* empty is the empty queue *)
  val empty : t

  (* push pushes the latest element (with some rank) into the queue *)
  val push : elt * t -> t

  (* pop qs returns the highest-priority element (if there is one) and modified queue *)
  val pop : t -> elt option * t

  (* pop qs removes the specified element from qs *)
  val remove : elt option -> t -> t

  (* update qs q q' is qs[q'/q] *)
  val update : t -> t -> t list -> t list

  (* flush q returns all elements enqeued in q. *)
  val flush : t -> elt list

  (* from_list elems returns a queue containing all elements enqueued in priority order *)
  val from_list : elt list -> t
end

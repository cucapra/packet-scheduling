(** A signature for queues. *)

module type Queue = sig
  (* An abstract type for a queue with elements of type 'a *)
  type 'a t

  (* empty is the empty queue *)
  val empty : 'a t

  (* push pushes the latest element (with some rank) into the queue *)
  val push : 'a * 'a t -> 'a t

  (* pop qs returns the highest-priority element (if there is one) and modified queue *)
  val pop : 'a t -> 'a option * 'a t

  (* update qs q q' is qs[q'/q] *)
  val update : 'a t list -> 'a t -> 'a t -> 'a t list

  (* flush q returns all elements enqeued in q. *)
  val flush : 'a t -> 'a list
end

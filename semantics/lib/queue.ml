open Packet

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

(* An implementation for queues (see MLI) *)
module QueueImpl (Pkt : Packet) : Queue with type elt = Pkt.t = struct
  type elt = Pkt.t
  type t = elt list

  let empty = []

  let rec push (elem, lst) =
    match lst with
    | [] -> [ elem ]
    | h :: t ->
        if Pkt.rank h >= Pkt.rank elem then elem :: h :: t
        else h :: push (elem, t)

  let pop = function [] -> (None, []) | h :: t -> (Some h, t)

  let rec update q_old q_new = function
    | [] -> []
    | h :: t -> if h = q_old then q_new :: t else q_old :: update q_old q_new t

  let rec remove elem q =
    match elem with
    | None -> q
    | Some e -> (
        match q with
        | [] -> []
        | h :: t -> if h = e then t else h :: remove elem t)

  let flush q = q
  let from_list elems = List.fold_right (fun x acc -> push (x, acc)) elems empty
end

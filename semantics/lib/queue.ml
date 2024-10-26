open Packet

module type Queue = sig
  type elt
  type t

  val empty : t
  val push : elt * t -> t
  val pop : t -> elt option * t
  val remove : elt option -> t -> t
  val update : t -> t -> t list -> t list
  val flush : t -> elt list
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
end

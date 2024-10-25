open SemanticsImpl.Packet
open SemanticsImpl.Queue
open SemanticsImpl.Semantics
open OUnit2

module PacketImpl : Packet = struct
  type t = float * float * float
  type ord = float

  let compare = compare
  let rank (r, _, _) = r
  let time (_, t, _) = t
  let weight (_, _, w) = w
end

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

module SemanticsTester (Pkt : Packet) (Q : Queue with type elt = Pkt.t) = struct
  module S = Semantics (Pkt) (Q)
end

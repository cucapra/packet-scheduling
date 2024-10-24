open Packet
open Queue

(** 
  Implementation of the operational semantics discussed in #67 (see linked documentation)
*)

(** A signature for semantics. *)
module type SemanticsSig = sig
  type prog
  type pkt
  type queue
  type state

  exception EvaluationError

  val push : pkt * queue * state -> state
  (** push pkt q st enqueues pkt to the queue q, and updates st. *)

  val pop : state -> pkt option * state
  (* pop st dequeues the next packet in line, and updates st. *)
end

(** An implementation for semantics. *)
module Semantics (Pkt : Packet) (Q : Queue) : SemanticsSig = struct
  (* For the time being, we operate in a world without variables. *)
  type clss = string
  type set = Class of clss | Union of set list

  exception EvaluationError

  type stream =
    | Fifo of set
    | EarliestDeadline of set
    | ShortestJobNext of set
    | RoundRobin of stream list
    | Strict of stream list
    | WeightedFair of stream list * int list

  type prog = stream
  type pkt = Pkt.t
  type queue = pkt Q.t
  type state = prog * queue list

  let push (pkt, q, (p, qs)) =
    if List.mem q qs then (p, Q.update qs q (Q.push (pkt, q)))
    else raise EvaluationError

  let pop = failwith "TODO"
end

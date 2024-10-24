open Packet
open Queue

(** Implementation of the semantics discussed in #67. See linked documentation. *)

(** A signature for Rio's operational semantics. *)
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

(** An implementation for Rio's operational semantics. *)
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

  (* Find lowest rank packet in a queue *)
  let head f q =
    try
      Some
        (List.hd (List.sort (fun p1 p2 -> compare (f p1) (f p2)) (Q.flush q)))
    with Failure _ -> None

  (* Find lowest rank packets over a series of queues *)
  let heads f = List.map (fun x -> (x, head f x))

  (* Pop packet that has the highest value for the specified f *)
  let popg f qs =
    (* Sort all non-None pop candidates for each queue by f *)
    let candidates = List.filter (fun (_, x) -> x <> None) (heads f qs) in
    match candidates with
    (* Case – all queues are empty. Then return none. *)
    | [] -> (None, qs)
    | _ ->
        let sorted =
          List.sort (fun (q1, p1) (q2, p2) -> compare p1 p2) candidates
        in
        (* Get pop queue and pop candidate from head of sorted list *)
        let q, pkt = List.hd sorted in
        (* Remove packet from queue *)
        let updated_q = Q.remove pkt q in
        (* Update qs *)
        let updated_qs = Q.update qs q updated_q in
        (pkt, updated_qs)

  (* Pop from a queue with specified function *)
  let pop_set_stream f (p, qs) =
    let pkt, qs_new = popg f qs in
    (pkt, (p, qs_new))

  let pop (p, qs) =
    match p with
    (* FIFO pops the packet with the lowest rank *)
    | Fifo _ -> pop_set_stream Pkt.rank (p, qs)
    (* EDF pops the packet with the earliest deadline *)
    | EarliestDeadline _ -> pop_set_stream Pkt.time (p, qs)
    (* SJN pops the packet with the lowest weight *)
    | ShortestJobNext _ -> pop_set_stream Pkt.weight (p, qs)
    (* To Be Implemented *)
    | _ -> failwith "Stream-To-Stream not yet implemented"
end

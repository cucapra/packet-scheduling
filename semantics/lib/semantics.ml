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
  (** pop st dequeues the next packet in line, and updates st. *)
end

(** An implementation for Rio's operational semantics. *)
module Semantics (Pkt : Packet) (Q : Queue) : SemanticsSig = struct
  type clss = string
  type set = Class of clss | Union of set list

  exception EvaluationError

  type stream =
    (* Set To Stream *)
    | Fifo of set
    | EarliestDeadline of set
    | ShortestJobNext of set
    (* Stream To Stream *)
    | RoundRobin of stream list
    | Strict of stream list
    | WeightedFair of stream list * int list

  type prog = stream
  type pkt = Pkt.t
  type queue = pkt Q.t
  type state = prog * queue list

  let push (pkt, q, (p, qs)) =
    (* Assert that the relevant queue is part of the larger list of queues *)
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
    let candidates =
      List.sort
        (fun (_, p1) (_, p2) -> compare p1 p2)
        (List.filter (fun (_, x) -> x <> None) (heads f qs))
    in
    match candidates with
    (* Case – all queues are empty. Then return none. *)
    | [] -> (None, qs)
    (* Get pop queue and pop candidate from head of sorted list *)
    | (q, pkt) :: _ ->
        (* Remove packet from queue *)
        let updated_q = Q.remove pkt q in
        (* Update qs *)
        let updated_qs = Q.update qs q updated_q in
        (pkt, updated_qs)

  (* Pop from a queue with specified function *)
  let pop_set_stream f (p, qs) =
    let pkt, qs_new = popg f qs in
    (pkt, (p, qs_new))

  (* Compute the number of subclasses in a program *)
  let rec num_classes prog =
    let rec n_classes_set = function
      | Class name -> 1
      | Union lst -> List.fold_right (fun x acc -> n_classes_set x + acc) lst 0
    in
    match prog with
    | Fifo s | EarliestDeadline s | ShortestJobNext s -> n_classes_set s
    | RoundRobin lst | Strict lst | WeightedFair (lst, _) ->
        List.fold_right (fun x acc -> num_classes x + acc) lst 0

  (** Partition lst into a list of lists where each lst[i] has length lengths[i].
      Precondition: sum(lengths) = List.num_classes lst *)
  let partition lst lengths =
    let rec aux lst lengths acc =
      match (lst, lengths) with
      | [], _ | _, [] -> [ acc ]
      | h1 :: t1, h2 :: t2 when h2 = 0 -> acc :: aux lst t2 []
      | h1 :: t1, h2 :: t2 -> aux t1 ((h2 - 1) :: t2) (acc @ [ h1 ])
    in
    aux lst lengths []

  (* update_i idx newval lst updates lst such that lst[i] = newval *)
  let rec update_i idx newval = function
    | [] -> []
    | h :: t -> if idx = 0 then newval :: t else update_i (idx - 1) newval t

  let rec pop (p, qs) =
    match p with
    (* FIFO pops the packet with the lowest rank *)
    | Fifo _ -> pop_set_stream Pkt.rank (p, qs)
    (* EDF pops the packet with the earliest deadline *)
    | EarliestDeadline _ -> pop_set_stream Pkt.time (p, qs)
    (* SJN pops the packet with the lowest weight *)
    | ShortestJobNext _ -> pop_set_stream Pkt.weight (p, qs)
    (* RR alternates popping one packet from each of a series of streams *)
    | RoundRobin substreams -> (
        match substreams with
        | [] -> (None, (p, qs))
        | h :: t -> (
            (* Partition qs into intervals by number of classes per substream *)
            let partitioned = partition qs (List.map num_classes substreams) in
            (* Pop with the first substream and its subset of queues *)
            match pop (h, List.hd partitioned) with
            | None, _ ->
                (* Queues and program don't change, but move to the end *)
                ( None,
                  ( RoundRobin (t @ [ h ]),
                    List.flatten (List.tl partitioned @ [ List.hd partitioned ])
                  ) )
            | Some pkt, (p_new, qs_new) ->
                (* Update program and queues and move both to the end *)
                ( Some pkt,
                  ( RoundRobin (t @ [ p_new ]),
                    List.flatten (List.tl partitioned @ [ qs_new ]) ) )))
    (* Strict pops from the first stream that is not silent *)
    | Strict substreams -> (
        match substreams with
        | [] -> (None, (p, qs))
        | _ -> (
            (* Partition qs into intervals by number of classes per substream *)
            let partitioned = partition qs (List.map num_classes substreams) in
            let combined = List.combine substreams partitioned in

            (* Check first that there is a pop candidate *)
            let can_pop =
              List.filter
                (fun (_, x) -> fst (pop x) <> None)
                (List.mapi (fun i x -> (i, x)) combined)
            in

            (* If there is no pop candidate, return None *)
            match can_pop with
            | [] -> (None, (p, qs))
            (* Otherwise, pop the candidate; update relevant programs and queues *)
            | h :: _ ->
                let (idx, _), (popped, new_state) = (h, pop (snd h)) in
                let p_new, qs_new =
                  List.split (update_i idx new_state combined)
                in
                (popped, (Strict p_new, List.flatten qs_new))))
    (* To Be Implemented *)
    | WeightedFair (substreams, weights) -> failwith "Not yet implemented"
end

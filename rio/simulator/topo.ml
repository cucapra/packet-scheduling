type 'a t =
  | Star
  | CStar of 'a list (* for Rio trees *)
  | Node of 'a t list

type enqdeq =
  | Enq
  | Deq

let rec of_policy e p =
  let open Frontend.Policy in
  match (e, p) with
  | _, RoundRobin ps | _, Strict ps -> Node (List.map (of_policy e) ps)
  | Enq, Fifo _ | Enq, EDF _ -> Star
  | Deq, Fifo clss | Deq, EDF clss -> CStar clss

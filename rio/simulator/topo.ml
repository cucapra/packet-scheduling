type 'a t =
  | Star
  | DecoratedStar of 'a list (* for Rio trees *)
  | Node of 'a t list

type enqdeq =
  | Enq
  | Deq

let rec of_policy e p =
  let open Frontend.Policy in
  match (e, p) with
  | _, Strict ps | _, RR ps | _, WFQ (ps, _) -> Node (List.map (of_policy e) ps)
  | Enq, FIFO _ | Enq, EDF _ -> Star
  | Deq, FIFO clss | Deq, EDF clss -> DecoratedStar clss

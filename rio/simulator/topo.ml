type 'a t =
  | Star
  | DecoratedStar of 'a list (* for Rio trees *)
  | Node of 'a t list

type enqdeq =
  | Enq
  | Deq

let rec of_policy e p =
  let open Rio_core.Policy in
  match (e, p) with
  | _, SP prs | _, WFQ prs -> Node (List.map (fun (p, _) -> of_policy e p) prs)
  | _, RR ps -> Node (List.map (of_policy e) ps)
  | Enq, FIFO _ -> Star
  | Deq, FIFO c -> DecoratedStar [ c ]

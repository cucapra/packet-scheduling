type t =
  | Leaf of Packet.t Pifo.t
  | Internal of t list * int Pifo.t

let ( let* ) = Option.bind
let replace_nth l i v = List.mapi (fun j x -> if j = i then v else x) l

let rec pop t =
  match t with
  | Leaf p ->
      let* pkt, p' = Pifo.pop p in
      Some (pkt, Leaf p')
  | Internal (qs, p) ->
      let* i, p' = Pifo.pop p in
      let* pkt, q' = pop (List.nth qs i) in
      Some (pkt, Internal (replace_nth qs i q', p'))

let rec push t pkt (path : Path.t) =
  match (t, path) with
  | Leaf p, Foot r -> Leaf (Pifo.push p pkt r)
  | Internal (qs, p), Path (i, r, pt) ->
      let p' = Pifo.push p i r in
      let q' = push (List.nth qs i) pkt pt in
      Internal (replace_nth qs i q', p')
  | _ -> failwith "ERROR: invalid path"

let rec size t =
  match t with
  | Leaf p -> Pifo.size p
  | Internal (qs, _) -> List.fold_left (fun acc q -> acc + size q) 0 qs

let rec create (topo : Topo.t) =
  match topo with
  | Star -> Leaf (Pifo.create ())
  | CStar _ -> failwith "ERROR: invalid topology"
  | Node ts ->
      let qs = List.map create ts in
      Internal (qs, Pifo.create ())

let rec to_topo t : Topo.t =
  match t with
  | Leaf _ -> Star
  | Internal (qs, _) -> Node (List.map to_topo qs)

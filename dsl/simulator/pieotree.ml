let ( let* ) = Option.bind

type t =
  | Leaf of (Packet.t * Rank.t * Time.t) Pieo.t
  | Internal of t list * (int * Rank.t * Time.t) Pieo.t

let replace_nth l n nth' = List.mapi (fun i x -> if i = n then nth' else x) l
let predicate now (_, _, ts) = ts <= now

let rec pop t now =
  match t with
  | Leaf p ->
      let* (pkt, _, _), p' = Pieo.pop p (predicate now) in
      Some (pkt, Leaf p')
  | Internal (qs, p) ->
      let* (i, _, _), p' = Pieo.pop p (predicate now) in
      let* pkt, q' = pop (List.nth qs i) now in
      Some (pkt, Internal (replace_nth qs i q', p'))

let rec push t ts pkt path =
  match (t, path) with
  | Leaf p, [ (_, r) ] -> Leaf (Pieo.push p (pkt, r, ts))
  | Internal (qs, p), (i, r) :: pt ->
      let p' = Pieo.push p (i, r, ts) in
      let q' = push (List.nth qs i) ts pkt pt in
      Internal (replace_nth qs i q', p')
  | _ -> failwith "ERROR: invalid path"

let rec size t now =
  (* The size of a PIEO tree is the number of ready packets in its leaves.
     Recall that a packet is _ready_ if its time stamp is <= `now`. *)
  match t with
  | Leaf p -> Pieo.size p (predicate now)
  | Internal (qs, _) -> List.fold_left (fun acc q -> acc + size q now) 0 qs

let rec create (topo : Topo.t) =
  match topo with
  | Star -> Leaf (Pieo.create (fun (_, a, _) (_, b, _) -> Rank.cmp a b))
  | Node topos ->
      let qs = List.map create topos in
      let p = Pieo.create (fun (_, a, _) (_, b, _) -> Rank.cmp a b) in
      Internal (qs, p)

let rec to_topo t : Topo.t =
  match t with
  | Leaf _ -> Star
  | Internal (qs, _) -> Node (List.map to_topo qs)

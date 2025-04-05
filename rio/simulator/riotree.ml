type t =
  | Leaf of Packet.t Pifo.t * Frontend.Ast.clss
  | Internal of t list

let ( let* ) = Option.bind
let replace_nth l i v = List.mapi (fun j x -> if j = i then v else x) l

(* `min cmp l` is the minimum element of list `l` according to comparator `cmp`*)
let min cmp l =
  let rec min_aux min l i =
    match (l, min) with
    | h :: t, Some (_, m) when cmp h m < 0 -> min_aux (Some (i, h)) t (i + 1)
    | _ :: t, Some _ -> min_aux min t (i + 1)
    | h :: t, None -> min_aux (Some (i, h)) t (i + 1)
    | [], _ -> min
  in
  min_aux None l 0

let rec pop t (o : Ordtree.t) =
  match (t, o) with
  | Leaf (p, c), Foot ->
      let* pkt, p' = Pifo.pop p in
      Some (pkt, Leaf (p', c))
  | Internal qs, Order ors when List.length qs = List.length ors ->
      let os, rs = List.split ors in
      let* i, (_, (pkt, q')) =
        List.map2 pop qs os
        |> List.map2 (fun x y -> (x, y)) rs
        |> List.filter_map (fun (x, y) ->
               let* y = y in
               Some (x, y))
        |> min (fun (a, _) (x, _) -> Rank.cmp a x)
      in
      Some (pkt, Internal (replace_nth qs i q'))
  | _ -> failwith "ERROR: invalid ordered tree"

let rec push t pkt rk =
  match t with
  | Leaf (_, c) when c <> Packet.flow pkt -> t
  | Leaf (p, c) ->
      let p' = Pifo.push p pkt rk in
      Leaf (p', c)
  | Internal qs ->
      let qs' = List.map (fun q -> push q pkt rk) qs in
      Internal qs'

let rec size t =
  match t with
  | Leaf (p, _) -> Pifo.size p
  | Internal qs -> List.fold_left (fun acc q -> acc + size q) 0 qs

let rec create (topo : Topo.t) =
  match topo with
  | Star -> failwith "ERROR: invalid topology"
  | CStar c -> Leaf (Pifo.create (), c)
  | Node ts ->
      let qs = List.map create ts in
      Internal qs

let rec to_topo t : Topo.t =
  match t with
  | Leaf (_, c) -> CStar c
  | Internal qs ->
      let ts = List.map to_topo qs in
      Node ts

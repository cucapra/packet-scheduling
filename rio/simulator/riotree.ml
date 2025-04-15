type ('a, 'b) tree =
  | Leaf of 'a Pifo.t * 'b list
  | Internal of ('a, 'b) tree list

type ('a, 'b) t = {
  tree : ('a, 'b) tree;
  canonical : 'a -> 'b;
}

type order =
  | Foot
  | Order of (order * Rank.t) list (* ranks must be unique *)

let ( let* ) = Option.bind
let replace_nth l i v = List.mapi (fun j x -> if j = i then v else x) l

(* `min cmp l` is the minimum element of list `l` according to comparator `cmp` *)
let min cmp l =
  let rec min_aux min l =
    match (l, min) with
    | h :: t, Some m when cmp h m < 0.0 -> min_aux (Some h) t
    | _ :: t, Some _ -> min_aux min t
    | h :: t, None -> min_aux (Some h) t
    | [], _ -> min
  in
  min_aux None l

let rec pop { tree; canonical } order =
  match (tree, order) with
  | Leaf (p, c), Foot ->
      let* v, p' = Pifo.pop p in
      Some (v, { tree = Leaf (p', c); canonical })
  | Internal qs, Order ors when List.length qs = List.length ors ->
      let os, rs = List.split ors in
      let* i, _, (v, { tree = q'; _ }) =
        List.map2 (fun q o -> pop { tree = q; canonical } o) qs os
        |> List.map2 (fun x y -> (x, y)) rs
        |> List.mapi (fun i (x, y) -> (i, x, y))
        |> List.filter_map (fun (i, x, y) ->
               let* y = y in
               Some (i, x, y))
        |> min (fun (_, a, _) (_, b, _) -> a -. b)
      in
      Some (v, { tree = Internal (replace_nth qs i q'); canonical })
  | _ -> failwith "ERROR: invalid order"

let rec push { tree; canonical } v rk =
  match tree with
  | Leaf (p, c) when List.exists (fun x -> x = canonical v) c ->
      let p' = Pifo.push p v rk in
      { tree = Leaf (p', c); canonical }
  | Leaf (p, c) -> { tree = Leaf (p, c); canonical }
  | Internal qs ->
      let f q = (push { tree = q; canonical } v rk).tree in
      let qs' = List.map f qs in
      { tree = Internal qs'; canonical }

let rec size { tree; canonical } =
  match tree with
  | Leaf (p, _) -> Pifo.size p
  | Internal qs ->
      List.fold_left (fun acc q -> acc + size { tree = q; canonical }) 0 qs

let rec create (topo : 'b Topo.t) canonical =
  match topo with
  | Star -> failwith "ERROR: invalid topology"
  | DecoratedStar c -> { tree = Leaf (Pifo.create (), c); canonical }
  | Node ts ->
      let qs = List.map (fun t -> (create t canonical).tree) ts in
      { tree = Internal qs; canonical }

let rec to_topo { tree; canonical } : 'b Topo.t =
  match tree with
  | Leaf (_, c) -> DecoratedStar c
  | Internal qs ->
      let ts = List.map (fun q -> to_topo { tree = q; canonical }) qs in
      Node ts

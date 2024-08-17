type t =
  | Star
  | Node of t list

type addr = int list
type hint = int -> addr Option.t (* A partial map from int to addr. *)
type map = addr -> addr Option.t

let ( let* ) = Option.bind

let rec addr_to_string = function
  | [] -> "ε"
  | h :: t -> Printf.sprintf "%d ∙ %s" h (addr_to_string t)

let rec of_policy (p : Frontend.Policy.t) =
  match p with
  | Class _ -> Star
  | Fifo plst | RoundRobin plst | Strict plst -> Node (List.map of_policy plst)
  | WeightedFair wplst -> Node (List.map (fun (p, _) -> of_policy p) wplst)

let rec height = function
  | Star -> 1
  | Node trees -> 1 + List.fold_left max 0 (List.map height trees)

let pop_d_topos pq d =
  (* `pq` is a priority queue of (decorated) topologies, prioritized by height.
     `pq has at least two elements. We will pop up to `d` of them so long as
     they have the same height `m`. We will return the popped topologies as a
     list, the remaining priority queue, and `m`. *)
  let rec pop_d_topos_aux pq height acc d =
    match (d, Pieo.pop pq (fun (_, _, _, height') -> height = height')) with
    | 0, _ | _, None ->
        (* We finished popping! Success. *)
        (List.rev acc, pq, height)
    | d, Some (topo, pq') ->
        (* We have another topology with the right height. Add it to the
           accumulator and recurse. *)
        pop_d_topos_aux pq' height (topo :: acc) (d - 1)
  in
  match Pieo.pop pq (fun _ -> true) with
  (* Pop the top topology to prime the algorithm. *)
  | None -> failwith "ERROR: cannot pop empty PQ of topologies"
  | Some (((_, _, _, m) as topo_one), pq') ->
      (* Now we need up to `d - 1` more topologies, IF they have height `m`. *)
      pop_d_topos_aux pq' m [ topo_one ] (d - 1)

let rec merge_into_one_topo pq d : t * map =
  match (Pieo.size pq (fun _ -> true), Pieo.pop pq (fun _ -> true)) with
  | 0, _ -> failwith "ERROR: cannot merge an empty PQ of topologies."
  | 1, Some ((t, _, map, _), _) -> (t, map)
  | _ -> (
      (* Extract up to `d` trees with minimum height `m`. *)
      let trees, pq', m = pop_d_topos pq d in
      match trees with
      | [ (topo, hint, map, _) ] ->
          (* There was just one tree with height `m`. Reinsert it with height `m
             + 1` and recurse. *)
          let pq'' = Pieo.push pq' (topo, hint, map, m + 1) in
          merge_into_one_topo pq'' d
      | _ ->
          (* There were two or more trees with height `m`. Pad the tree list
             with Stars until it has length `d`. Then make a new node with those
             `d` topologies as its children. Make, also, a new embedding map and
             a new hint map. *)
          let k = List.length trees in
          let trees' =
            trees
            @ List.init (d - k) (fun _ ->
                  (Star, (fun _ -> None), (fun _ -> None), 1))
          in
          let node = Node (List.map (fun (t, _, _, _) -> t) trees') in
          (* This is the new node. *)
          (* For the map and the hint, it will pay to tag the trees' list with integers. *)
          let trees'' =
            List.mapi (fun i (a, b, c, d) -> (i, a, b, c, d)) trees'
          in
          (* The hint map is just the union of the hints of the children. *)
          let map = function
            | [] -> Some []
            | n :: rest ->
                (* The step `n` will determine which of our children we'll rely
                   on. The rest of the address will be processed by that child's
                   map. Which, if any, of the hints in `trees''` have a value
                   registered for `n`? *)
                let* i, _, hint_i, map_i, _ =
                  List.find_opt
                    (fun (_, _, hint, _, _) -> hint n <> None)
                    trees''
                in
                (* If none of my children can get to it, neither can I. But if
                   my `i`'th child knows how to get to it, I'll go via that
                   child. *)
                let* x = hint_i n in
                (* Now we have the rest of the address, but we need to prepend
                   `i`. *)
                Some ((i :: x) @ Option.get (map_i rest))
          in
          (* Add the new node to the priority queue. *)
          let hint n =
            (* The new hint for the node is the union of the children's hints,
               but, since we are growing taller by one level, we need to
               arbitrate _between_ those `d` children using `0, 1, ..., d - 1`
               as a prefix. *)
            let* i, _, hint_i, _, _ =
              List.find_opt (fun (_, _, hint, _, _) -> hint n <> None) trees''
            in
            (* If none of my children can get to it, neither can I. But if my
               i'th child knows how to get to it, I'll go via that child. *)
            let* x = hint_i n in
            Some (i :: x)
          in
          (* The height of this tree is clearly one more than its children. *)
          let height = m + 1 in
          (* Add the new node to the priority queue. *)
          let pq'' = Pieo.push pq' (node, hint, map, height) in
          (* Recurse. *)
          merge_into_one_topo pq'' d)

let rec build_d_ary d = function
  | Star ->
      (* The embedding of a `Star` is a `Star`, and the map is the identity for
         `[]`. *)
      (Star, fun addr -> if addr = [] then Some [] else None)
  | Node ts ->
      let (ts' : (t * hint * map * int) list) =
        (* We will decorate this list of subtrees a little. *)
        List.mapi
          (fun i t ->
            (* Get embeddings and maps for the subtrees. *)
            let t', map = build_d_ary d t in
            (* For each child, creat a hints map that just has the binding `i ->
               Some []`. *)
            let hint addr = if addr = i then Some [] else None in
            (* Get the height of this tree. *)
            let height = height t' in
            (* Put it all together. *)
            (t', hint, map, height))
          ts
      in
      (* A PIFO of these decorated subtrees, prioritized by height. Shorter is
         higher-priority. *)
      let pq = Pieo.of_list ts' (fun (_, _, _, a) (_, _, _, b) -> a - b) in
      merge_into_one_topo pq d

let rec remove_prefix (prefix : addr) (addr : addr) =
  (* Maybe this is unduly specific to addresses, but ah well. *)
  match (prefix, addr) with
  | [], addr -> addr
  | p :: prefix, a :: addr ->
      if p = a then remove_prefix prefix addr
      else failwith "ERROR: prefix does not match address."
  | _ -> failwith "ERROR: prefix does not match address."

let rec add_prefix prefix r path_rest =
  match prefix with
  | [] -> path_rest
  | j :: prefix ->
      (* Add (j,r) to the path path_rest. *)
      (j, r) :: add_prefix prefix r path_rest

let rec lift_tilde (f : map) tree (path : Path.t) =
  (* Topology `tree` can embed into some topology `tree'`. We don't need `tree'`
     as an argument. We have `f`, the partial map that takes addresses in `tree`
     to addresses in `tree'`. Given a path in `tree`, we want to find the
     corresponding path in `tree'`. *)
  match (tree, path) with
  | Star, [ _ ] ->
      (* When the toplogy is a `Star`, the embedded topology is also a `Star`.
         The path better be a singleton; we have checked this via
         pattern-matching. We return the path unchanged. *)
      path
  | Node ts, (i, r) :: pt ->
      (* When the topology is a node, the embedded topology is a node. The path
         better be a non-empty list; we have checked this via pattern-matching.
         If this node embeds into node' in the embedded topology, this node's
         `i`th child embeds somewhere under node' in the embedded topology. *)
      let f_i addr =
        (* First we compute that embedding. We need to check what `f` would have
           said about `i :: addr`. The resultant list has some prefix that is
           f's answer for `[ i ]` alone. We must remove that prefix. *)
        let* whole = f (i :: addr) in
        let* prefix = f [ i ] in
        Some (remove_prefix prefix whole)
      in
      let path_rest = lift_tilde f_i (List.nth ts i) pt in
      (* We are not done. For each `j` in the prefix, we must add `(j, r)` to
         the front of path_rest. *)
      add_prefix (Option.get (f [ i ])) r path_rest
  | _ -> failwith "ERROR: topology and path do not match."

(** The decorated source tree mirrors [Frontend.Policy.t] but annotates every
    node with its assigned vPIFO and every parent→child edge with its adopt
    step. WFQ edges additionally carry the per-arm weight. *)

open Instr

type t =
  | FIFO of vpifo * clss
  | UNION of vpifo * (step * t) list
  | SP of vpifo * (step * t) list
  | RR of vpifo * (step * t) list
  | WFQ of vpifo * (step * t * float) list

(* Replace element at index [i] in [xs] by applying [f]; other elements
   untouched. Out-of-range [i] silently leaves the list unchanged. *)
let list_replace_nth i f xs = List.mapi (fun j x -> if j = i then f x else x) xs

(* Drop the element at index [i]; other elements untouched. *)
let list_drop_nth i xs = List.filteri (fun j _ -> j <> i) xs

(* Insert [x] so that it lives at index [i] in the result. End-append is
   the special case [i = List.length xs]. *)
let list_insert_at i x xs =
  let rec ins n = function
    | lst when n <= 0 -> x :: lst
    | [] -> [ x ]
    | h :: t -> h :: ins (n - 1) t
  in
  ins i xs

(* The vPIFO at the root of [d]. *)
let root_vpifo = function
  | FIFO (v, _) | UNION (v, _) | SP (v, _) | RR (v, _) | WFQ (v, _) -> v

(* The IR-side [pol_ty] of [d]. *)
let pol_ty : t -> pol_ty = function
  | FIFO _ -> FIFO
  | UNION _ -> UNION
  | SP _ -> SP
  | RR _ -> RR
  | WFQ _ -> WFQ

(* Number of immediate children. Errors on FIFO. *)
let arity = function
  | FIFO _ -> failwith "Decorated.arity: FIFO has no children"
  | UNION (_, es) | SP (_, es) | RR (_, es) -> List.length es
  | WFQ (_, es) -> List.length es

(* The k-th edge's adopt-step. Errors on FIFO. *)
let nth_step d k =
  match d with
  | FIFO _ -> failwith "Decorated.nth_step: FIFO"
  | UNION (_, es) | SP (_, es) | RR (_, es) -> fst (List.nth es k)
  | WFQ (_, es) ->
      let s, _, _ = List.nth es k in
      s

(* The k-th child subtree. Errors on FIFO. *)
let nth_child d k =
  match d with
  | FIFO _ -> failwith "Decorated.nth_child: FIFO"
  | UNION (_, es) | SP (_, es) | RR (_, es) -> snd (List.nth es k)
  | WFQ (_, es) ->
      let _, c, _ = List.nth es k in
      c

(* Pre-order fold over all nodes of [d]. *)
let rec fold (f : 'a -> t -> 'a) (acc : 'a) (d : t) : 'a =
  let acc = f acc d in
  match d with
  | FIFO _ -> acc
  | UNION (_, es) | SP (_, es) | RR (_, es) ->
      List.fold_left (fun a (_, c) -> fold f a c) acc es
  | WFQ (_, es) -> List.fold_left (fun a (_, c, _) -> fold f a c) acc es

(* One vPIFO per node; one step per parent→child edge. Used by [patch]
   to seed its fresh-ID counters past whatever [prev] already used. *)
let count_vpifos d = fold (fun a _ -> a + 1) 0 d

let count_steps d =
  fold
    (fun a -> function
      | FIFO _ -> a
      | UNION (_, es) | SP (_, es) | RR (_, es) -> a + List.length es
      | WFQ (_, es) -> a + List.length es)
    0 d

(* All vPIFO IDs in [d], pre-order. *)
let subtree_vpifos d = List.rev (fold (fun a node -> root_vpifo node :: a) [] d)

(* All leaf classes in [d], pre-order. Mirrors how [compile_arm]
   propagates [classes] up the tree, so each ancestor of [d] holds an
   [Assoc] for every class in this list. *)
let subtree_classes d =
  List.rev
    (fold
       (fun a -> function
         | FIFO (_, c) -> c :: a
         | _ -> a)
       [] d)

(* For each node in [d], pair its vPIFO with the classes it was [Assoc]'d
   to during compilation: a leaf carries its single class, and an
   internal node carries the union of its descendants' classes (matching
   [all_classes] in [compile_arm]). *)
let rec subtree_class_assocs (d : t) : (vpifo * clss list) list =
  match d with
  | FIFO (v, c) -> [ (v, [ c ]) ]
  | UNION (_, es) | SP (_, es) | RR (_, es) ->
      (root_vpifo d, subtree_classes d)
      :: List.concat_map (fun (_, c) -> subtree_class_assocs c) es
  | WFQ (_, es) ->
      (root_vpifo d, subtree_classes d)
      :: List.concat_map (fun (_, c, _) -> subtree_class_assocs c) es

(* Subtree at [path]; [[]] is [d] itself. Errors on a path that goes
   through a FIFO leaf. *)
let rec walk d path =
  match path with
  | [] -> d
  | i :: rest -> walk (nth_child d i) rest

(* For each ancestor of the node at [path], return its vPIFO and the
   step it uses to reach the next node on the path. Length equals
   [List.length path]. *)
let rec ancestor_chain d path : (vpifo * step) list =
  match path with
  | [] -> []
  | i :: rest ->
      (root_vpifo d, nth_step d i) :: ancestor_chain (nth_child d i) rest

(* Apply [f] to the subtree at [path], leaving the surrounding structure
   (including WFQ weights along the path) untouched. Generalizes the
   three [patch]-side path rewrites: insert, remove, set-weight. *)
let rec rewrite_at (d : t) (path : int list) (f : t -> t) : t =
  match path with
  | [] -> f d
  | i :: rest -> (
      let go c = rewrite_at c rest f in
      let bump = list_replace_nth i (fun (s, c) -> (s, go c)) in
      let bump_w = list_replace_nth i (fun (s, c, w) -> (s, go c, w)) in
      match d with
      | FIFO _ -> failwith "Decorated.rewrite_at: path through FIFO leaf"
      | UNION (v, es) -> UNION (v, bump es)
      | SP (v, es) -> SP (v, bump es)
      | RR (v, es) -> RR (v, bump es)
      | WFQ (v, es) -> WFQ (v, bump_w es))

let insert_arm k new_step new_child = function
  | FIFO _ -> failwith "Decorated.insert_arm: FIFO"
  | UNION (v, es) -> UNION (v, list_insert_at k (new_step, new_child) es)
  | SP (v, es) -> SP (v, list_insert_at k (new_step, new_child) es)
  | RR (v, es) -> RR (v, list_insert_at k (new_step, new_child) es)
  | WFQ _ -> failwith "Decorated.insert_arm: WFQ unreachable under OneArmAdded"

let drop_arm k = function
  | FIFO _ -> failwith "Decorated.drop_arm: FIFO"
  | UNION (v, es) -> UNION (v, list_drop_nth k es)
  | SP (v, es) -> SP (v, list_drop_nth k es)
  | RR (v, es) -> RR (v, list_drop_nth k es)
  | WFQ (v, es) -> WFQ (v, list_drop_nth k es)

let set_weight k new_w = function
  | WFQ (v, es) ->
      WFQ (v, list_replace_nth k (fun (s, c, _) -> (s, c, new_w)) es)
  | _ -> failwith "Decorated.set_weight: WFQ-only"

(* Replace child at index [k], preserving the parent→child step (and WFQ
   weight). Used by [OneArmReplaced]: the new arm rides on the existing
   [step_k], with the old root [Designate]d as the new root's predecessor. *)
let replace_arm k new_child = function
  | FIFO _ -> failwith "Decorated.replace_arm: FIFO"
  | UNION (v, es) ->
      UNION (v, list_replace_nth k (fun (s, _) -> (s, new_child)) es)
  | SP (v, es) -> SP (v, list_replace_nth k (fun (s, _) -> (s, new_child)) es)
  | RR (v, es) -> RR (v, list_replace_nth k (fun (s, _) -> (s, new_child)) es)
  | WFQ (v, es) ->
      WFQ (v, list_replace_nth k (fun (s, _, w) -> (s, new_child, w)) es)

let rec to_policy (d : t) : Frontend.Policy.t =
  let module P = Frontend.Policy in
  match d with
  | FIFO (_, c) -> P.FIFO c
  | UNION (_, es) -> P.UNION (List.map (fun (_, c) -> to_policy c) es)
  | SP (_, es) -> P.SP (List.map (fun (_, c) -> to_policy c) es)
  | RR (_, es) -> P.RR (List.map (fun (_, c) -> to_policy c) es)
  | WFQ (_, es) ->
      P.WFQ
        ( List.map (fun (_, c, _) -> to_policy c) es,
          List.map (fun (_, _, w) -> w) es )

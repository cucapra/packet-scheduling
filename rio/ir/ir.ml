(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include Instr

(* --- Small list utilities ------------------------------------------------ *)

(* Split a non-empty list into its prefix and its last element.
   E.g. [list_foot [1; 2; 3]] = ([1; 2], 3). Raises on empty input. *)
let list_foot xs =
  match List.rev xs with
  | [] -> invalid_arg "list_foot: empty list"
  | last :: rev_init -> (List.rev rev_init, last)

(* Replace element at index [i] in [xs] by applying [f]; other elements
   untouched. Out-of-range [i] silently leaves the list unchanged — every
   caller in this file has already validated [i]. *)
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

(* A counter starting at [start]: returns a thunk that hands out fresh
   integers, the first being [start]. *)
let make_counter ~start =
  let n = ref (start - 1) in
  fun () ->
    incr n;
    !n

(* --- Decorated source tree + read/edit helpers --------------------------- *)

(* The decorated source tree mirrors [Frontend.Policy.t] but annotates every
   node with its assigned vPIFO and every parent→child edge with its adopt
   step. WFQ edges additionally carry the per-arm weight. *)
module Decorated = struct
  type t =
    | FIFO of vpifo * clss
    | UNION of vpifo * (step * t) list
    | SP of vpifo * (step * t) list
    | RR of vpifo * (step * t) list
    | WFQ of vpifo * (step * t * float) list

  (* The vPIFO at the root of [d]. *)
  let vpifo = function
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

  (* Pre-order fold over all nodes of [d]. Visits a node before its
     descendants. *)
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
  let subtree_vpifos d = List.rev (fold (fun a node -> vpifo node :: a) [] d)

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
        (vpifo d, subtree_classes d)
        :: List.concat_map (fun (_, c) -> subtree_class_assocs c) es
    | WFQ (_, es) ->
        (vpifo d, subtree_classes d)
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
        (vpifo d, nth_step d i) :: ancestor_chain (nth_child d i) rest

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

  (* --- Parent-local edits, designed to be passed as the [f] of
     [rewrite_at]. ------------------------------------------------------- *)

  let insert_arm k new_step new_child = function
    | FIFO _ -> failwith "Decorated.insert_arm: FIFO"
    | UNION (v, es) -> UNION (v, list_insert_at k (new_step, new_child) es)
    | SP (v, es) -> SP (v, list_insert_at k (new_step, new_child) es)
    | RR (v, es) -> RR (v, list_insert_at k (new_step, new_child) es)
    | WFQ _ ->
        failwith "Decorated.insert_arm: WFQ unreachable under OneArmAdded"

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
end

type compiled = {
  prog : program;
  decorated : Decorated.t;
  pes : pe list;
}

(* --- Compile a [Frontend.Policy.t] to IR --------------------------------- *)

(* Starting IDs for the two ID spaces. *)
let vpifo_start = 100
let step_start = 1000

(* The fake root sits one level above every real root. It exists so that
   the real root always has an editable parent classifier — which lets
   [patch] handle whole-tree replacement, [SuperPol], and [SubPol] as
   ordinary parent-side edits rather than special-cased re-rootings.
   Reserved IDs below [vpifo_start]/[step_start] keep the existing
   numbering of every real node intact. The fake root lives on PE -1; the
   simulator never sees it as a runtime node — [of_policy] emits the
   instructions that wire it up, and that's the entire surface. *)
let fake_root_v : vpifo = 99
let fake_root_step : step = 999
let fake_root_pe : pe = -1

(* The instructions for compiling a subtree, grouped by instruction "kind".
   By merging these [frags] with care, we can make a top-level
   concatenation that has all spawns first, then all adopts, etc. *)
type frag = {
  spawns : instr list;
  adopts : instr list;
  assocs : instr list;
  maps : instr list;
  change_pols : instr list;
  change_weights : instr list;
  root_v : vpifo; (* The root of the subtree that this [frag] creates *)
  classes : clss list; (* Classes handled by this subtree *)
}

(* Flatten a [frag] into the canonical IR program ordering. *)
let frag_to_program (f : frag) : program =
  List.concat
    [ f.spawns; f.adopts; f.assocs; f.maps; f.change_pols; f.change_weights ]

(* Interleave a parent's [local] frag with each of its [children] frags,
   kindwise: every kind list is local-first, then children in order. The
   result inherits [root_v] and [classes] from [local] — the parent's local
   instructions were built around its own identity. Used by [compile_arm] to
   combine self with recursive child results, and by [patch] to combine the
   splice instructions with the new arm's frag. *)
let combine_frags (local : frag) (children : frag list) : frag =
  let collect proj = proj local @ List.concat_map proj children in
  {
    spawns = collect (fun f -> f.spawns);
    adopts = collect (fun f -> f.adopts);
    assocs = collect (fun f -> f.assocs);
    maps = collect (fun f -> f.maps);
    change_pols = collect (fun f -> f.change_pols);
    change_weights = collect (fun f -> f.change_weights);
    root_v = local.root_v;
    classes = local.classes;
  }

(** Stand up a single FIFO leaf: register its spawn and assoc, decorate. *)
let compile_FIFO ~v ~pe c : frag * Decorated.t =
  ( {
      spawns = [ Spawn (v, pe) ];
      assocs = [ Assoc (v, c) ];
      root_v = v;
      classes = [ c ];
      adopts = [];
      maps = [];
      change_pols = [];
      change_weights = [];
    },
    Decorated.FIFO (v, c) )

(* A "stub" frag that stands in for an already-installed subtree (during a
   [SuperPol] splice). Carries the existing subtree's [root_v] and the
   classes its leaves cover, but emits no instructions: every spawn/adopt
   inside the subtree was issued during a prior compile and is still live. *)
let stub_frag (prev_d : Decorated.t) : frag =
  {
    spawns = [];
    adopts = [];
    assocs = [];
    maps = [];
    change_pols = [];
    change_weights = [];
    root_v = Decorated.vpifo prev_d;
    classes = Decorated.subtree_classes prev_d;
  }

(* Compile a [Frontend.Policy.t] subtree at [depth]. Returns both the
   instruction fragment and the decorated-tree decoration that records the
   vPIFO/step IDs assigned to this subtree. The PE for each layer is
   resolved through [pe_of_depth] — the invariant is that all nodes at the
   same depth share a PE; what PE that is depends on the surrounding
   compile context (a fresh [of_policy] uses [d → d]; [SuperPol] uses a
   custom mapping that respects [prev]'s existing PE assignments). Pure
   dispatcher: variant selection and SP weight synthesis happen here, and
   each variant wraps [compile_arm]'s edges in the matching [decorated]
   constructor.

   [splice], when supplied, names a path inside this subtree at which an
   already-installed [Decorated.t] should be grafted in instead of compiled
   afresh. When [splice = Some ([], prev_d)], we short-circuit to
   [stub_frag prev_d] / [prev_d] — caller's parent then [Adopt]s
   [Decorated.vpifo prev_d] and propagates [Decorated.subtree_classes
   prev_d] up the new ancestor chain. *)
let rec compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth ?splice
    (p : Frontend.Policy.t) : frag * Decorated.t =
  match splice with
  | Some ([], prev_d) -> (stub_frag prev_d, prev_d)
  | _ -> (
      let module P = Frontend.Policy in
      match p with
      | P.FIFO c -> compile_FIFO ~v:(fresh_v ()) ~pe:(pe_of_depth depth) c
      | P.UNION children ->
          let frag, edges =
            compile_arm ~fresh_v ~fresh_s ~pe_of_depth ~depth ~pol_ty:UNION
              ~weights:[] ?splice children
          in
          (frag, Decorated.UNION (frag.root_v, edges))
      | P.RR children ->
          let frag, edges =
            compile_arm ~fresh_v ~fresh_s ~pe_of_depth ~depth ~pol_ty:RR
              ~weights:[] ?splice children
          in
          (frag, Decorated.RR (frag.root_v, edges))
      | P.SP children ->
          (* Strict priority: first child has priority 1.0 (highest), then 2.0, 3.0, … *)
          let weights = List.mapi (fun i _ -> float_of_int (i + 1)) children in
          let frag, edges =
            compile_arm ~fresh_v ~fresh_s ~pe_of_depth ~depth ~pol_ty:SP
              ~weights ?splice children
          in
          (frag, Decorated.SP (frag.root_v, edges))
      | P.WFQ (children, ws) ->
          let frag, edges =
            compile_arm ~fresh_v ~fresh_s ~pe_of_depth ~depth ~pol_ty:WFQ
              ~weights:ws ?splice children
          in
          let weighted = List.map2 (fun (s, d) w -> (s, d, w)) edges ws in
          (frag, Decorated.WFQ (frag.root_v, weighted)))

(* Returns [(frag, edges)] where [edges] pairs each adopt-step with its
   child's decorated subtree, in source order. [weights] is empty for
   UNION/RR (they don't carry weights) and one-per-arm for SP/WFQ.
   List length parity with [children] is the caller's responsibility.

   [splice], when [Some (i :: rest, prev_d)], hands the splice down to the
   [i]-th child with the head consumed; siblings get [None]. *)
and compile_arm ~fresh_v ~fresh_s ~pe_of_depth ~depth ~pol_ty ~weights ?splice
    children : frag * (step * Decorated.t) list =
  (* Spawn self first, so that we get a lower ID number than the kids. *)
  let v = fresh_v () in
  let local_spawns = [ Spawn (v, pe_of_depth depth) ] in
  (* Recurse on each child; List.mapi is left-to-right in the stdlib so
     vPIFO IDs come out in source order. *)
  let child_results =
    List.mapi
      (fun i child ->
        let child_splice =
          match splice with
          | Some (j :: rest, prev_d) when i = j -> Some (rest, prev_d)
          | _ -> None
        in
        compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:(depth + 1)
          ?splice:child_splice child)
      children
  in
  let child_frags = List.map fst child_results in
  let child_decorated = List.map snd child_results in
  (* Adopt each child, capturing the fresh step ID we hand out for it. *)
  let adoption_records =
    List.map
      (fun cf ->
        let s = fresh_s () in
        (Adopt (s, v, cf.root_v), s, cf))
      child_frags
  in
  let local_adopts = List.map (fun (a, _, _) -> a) adoption_records in
  let all_classes = List.concat_map (fun cf -> cf.classes) child_frags in
  let local_assocs = List.map (fun c -> Assoc (v, c)) all_classes in
  let local_maps =
    List.concat_map
      (fun (_, s, cf) -> List.map (fun c -> Map (v, c, s)) cf.classes)
      adoption_records
  in
  let local_change_pols = [ Change_pol (v, pol_ty, List.length children) ] in
  let local_change_weights =
    match weights with
    | [] -> []
    | ws ->
        List.map2
          (fun (_, s, _) w -> Change_weight (v, s, w))
          adoption_records ws
  in
  let local =
    {
      spawns = local_spawns;
      adopts = local_adopts;
      assocs = local_assocs;
      maps = local_maps;
      change_pols = local_change_pols;
      change_weights = local_change_weights;
      root_v = v;
      classes = all_classes;
    }
  in
  let combined = combine_frags local child_frags in
  let edges =
    List.map2 (fun (_, s, _) d -> (s, d)) adoption_records child_decorated
  in
  (combined, edges)

(* --- Patch-side helpers -------------------------------------------------- *)

(* Erase IR decorations to recover the source [Frontend.Policy.t]. Used by
   [patch] to feed [Rio_compare.Compare.analyze] without storing the policy
   alongside the decorated form. *)
let rec policy_of_decorated (d : Decorated.t) : Frontend.Policy.t =
  let module P = Frontend.Policy in
  match d with
  | Decorated.FIFO (_, c) -> P.FIFO c
  | Decorated.UNION (_, edges) ->
      P.UNION (List.map (fun (_, c) -> policy_of_decorated c) edges)
  | Decorated.SP (_, edges) ->
      P.SP (List.map (fun (_, c) -> policy_of_decorated c) edges)
  | Decorated.RR (_, edges) ->
      P.RR (List.map (fun (_, c) -> policy_of_decorated c) edges)
  | Decorated.WFQ (_, edges) ->
      let policies = List.map (fun (_, c, _) -> policy_of_decorated c) edges in
      let weights = List.map (fun (_, _, w) -> w) edges in
      P.WFQ (policies, weights)

(* The vPIFO, arity, and pol_ty of a non-FIFO parent at the splice point.
   Errors if the parent is somehow a FIFO — that's a [Compare] bug. *)
let parent_info = function
  | Decorated.FIFO _ -> failwith "Ir.patch: parent is a FIFO leaf"
  | d -> (Decorated.vpifo d, Decorated.arity d, Decorated.pol_ty d)

(* The [(step * Decorated.t) list] of an SP parent. Used by both the
   [OneArmAdded] and [OneArmRemoved] SP-weight-shift loops. *)
let sp_edges = function
  | Decorated.SP (_, es) -> es
  | _ -> failwith "Ir.patch: expected an SP parent"

(* --- Public entry points -------------------------------------------------- *)

(* Max depth (root = 0) of a [Frontend.Policy.t]. Used to size the
   depth→PE list. *)
let rec policy_depth (p : Frontend.Policy.t) : int =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO _ -> 0
  | P.UNION ps | P.SP ps | P.RR ps ->
      1 + List.fold_left max 0 (List.map policy_depth ps)
  | P.WFQ (ps, _) -> 1 + List.fold_left max 0 (List.map policy_depth ps)

(* Append [n] consecutive PEs starting at [start] to [pes]. Used when a
   patch lands new layers below the deepest existing layer. *)
let extend_pes_with_fresh ~n ~start pes =
  let rec extra k cur = if k <= 0 then [] else cur :: extra (k - 1) (cur + 1) in
  pes @ extra n start

(* Grow [pes] so it covers [target_depth], allocating fresh PEs (above
   anything already in use) for any new layers. No-op when [pes] is
   already long enough. *)
let pes_extended_to_depth target_depth pes =
  let cur = List.length pes in
  if target_depth < cur then pes
  else
    let max_pe = List.fold_left max (-1) pes in
    extend_pes_with_fresh ~n:(target_depth - cur + 1) ~start:(max_pe + 1) pes

let of_policy (p : Frontend.Policy.t) : compiled =
  let fresh_v = make_counter ~start:vpifo_start in
  let fresh_s = make_counter ~start:step_start in
  let pe_of_depth d = d in
  let frag, decorated =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:0 p
  in
  (* Wrap the real root in the fake root. The fake root carries every
     class the real tree handles, all routed via its single step to the
     real root. [decorated] continues to describe only the real tree —
     the fake root is a known constant ([fake_root_v]/[fake_root_step]/
     [fake_root_pe]) that [patch] re-derives without storing. *)
  let fake_frag =
    {
      spawns = [ Spawn (fake_root_v, fake_root_pe) ];
      adopts = [ Adopt (fake_root_step, fake_root_v, frag.root_v) ];
      assocs = List.map (fun c -> Assoc (fake_root_v, c)) frag.classes;
      maps =
        List.map (fun c -> Map (fake_root_v, c, fake_root_step)) frag.classes;
      change_pols = [ Change_pol (fake_root_v, UNION, 1) ];
      change_weights = [];
      root_v = fake_root_v;
      classes = frag.classes;
    }
  in
  let combined = combine_frags fake_frag [ frag ] in
  let pes = List.init (policy_depth p + 1) (fun d -> d) in
  { prog = frag_to_program combined; decorated; pes }

(* Whole-tree replacement, riding on the fake root. Reached via
   [Compare.OneArmReplaced { path = []; _ }] — both the precise leaf
   case (constructor mismatch / FIFO-class swap at the root) and the
   "give up" case where [Compare] hit a multi-arm divergence and
   wholesale-replaces. The fake root
   ([fake_root_v]/[fake_root_step]/[fake_root_pe]) plays the same
   structural role here that an internal parent plays in the non-root
   [OneArmReplaced] handler: its classifier is rewritten to route new
   classes through the same step to the new real root, while the old
   real root is [Designate]d so its in-flight traffic drains. *)
let whole_tree_replace ~prev ~(next : Frontend.Policy.t) : compiled option =
  let fresh_v =
    make_counter ~start:(vpifo_start + Decorated.count_vpifos prev.decorated)
  in
  let fresh_s =
    make_counter ~start:(step_start + Decorated.count_steps prev.decorated)
  in
  let new_pes = pes_extended_to_depth (policy_depth next) prev.pes in
  let pe_of_depth d = List.nth new_pes d in
  let new_frag, new_decorated =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:0 next
  in
  let new_root_v = new_frag.root_v in
  let new_classes = new_frag.classes in
  let old_root_v = Decorated.vpifo prev.decorated in
  let old_classes = Decorated.subtree_classes prev.decorated in
  (* Fake-root classifier edits: stop routing the prev tree's classes,
     start routing next's — all via [fake_root_step] to the new real root. *)
  let unmaps_old =
    List.map (fun c -> Unmap (fake_root_v, c, fake_root_step)) old_classes
  in
  let deassocs_old_anc =
    List.map (fun c -> Deassoc (fake_root_v, c)) old_classes
  in
  let assocs_new_anc = List.map (fun c -> Assoc (fake_root_v, c)) new_classes in
  let maps_new =
    List.map (fun c -> Map (fake_root_v, c, fake_root_step)) new_classes
  in
  (* Drain semantics on the prev tree: each node stops accepting its
     classes; [Designate] then fuses the old root with the new root so
     the fake root's single step becomes a super-node that drains old
     before servicing new. GC marks every prev node for collection once
     it underflows. *)
  let inner_deassocs =
    List.concat_map
      (fun (v, cs) -> List.map (fun c -> Deassoc (v, c)) cs)
      (Decorated.subtree_class_assocs prev.decorated)
  in
  let designate = [ Designate (old_root_v, new_root_v) ] in
  let gcs =
    List.map (fun v -> GC v) (Decorated.subtree_vpifos prev.decorated)
  in
  let prog =
    List.concat
      [
        frag_to_program new_frag;
        designate;
        inner_deassocs;
        unmaps_old;
        deassocs_old_anc;
        assocs_new_anc;
        maps_new;
        gcs;
      ]
  in
  Some { prog; decorated = new_decorated; pes = new_pes }

let patch ~prev ~(next : Frontend.Policy.t) : compiled option =
  let open Rio_compare.Compare in
  let prev_policy = policy_of_decorated prev.decorated in
  match analyze prev_policy next with
  | Same ->
      (* Nothing structural to do — return an empty delta. The decorated
         tree is immutable, so we hand back the same reference. *)
      Some { prog = []; decorated = prev.decorated; pes = prev.pes }
  | OneArmReplaced { path = []; _ } -> whole_tree_replace ~prev ~next
  | SuperPol [] | SubPol [] -> None
  | SuperPol path ->
      (* [prev]'s policy sits inside [next] at [path]. We compile only the
         "extra" structure that [next] adds around [prev] — the chain of
         ancestors from [next]'s root down to the splice, plus their
         non-path siblings — and graft [prev]'s already-installed root in
         at the splice via [Adopt]. The propagation of [prev]'s classes
         up through the new ancestor chain falls out of [compile_arm]'s
         existing assoc/map plumbing once [stub_frag] reports them. *)
      let len = List.length path in
      let prev_max_depth = List.length prev.pes - 1 in
      let next_max_depth = policy_depth next in
      (* Build [new_pes] depth by depth: layers above [prev] (depths
         [0..len-1]) get fresh PEs that don't collide with [prev.pes];
         layers at [len..len + prev_max_depth] reuse [prev.pes] so
         already-installed nodes keep their PEs; any layers further below
         (new sibling subtrees that reach deeper than [prev]) get more
         fresh PEs. *)
      let pe_counter =
        make_counter ~start:(List.fold_left max (-1) prev.pes + 1)
      in
      let rec build d =
        if d > next_max_depth then []
        else
          let pe =
            if d < len then pe_counter ()
            else if d - len <= prev_max_depth then List.nth prev.pes (d - len)
            else pe_counter ()
          in
          pe :: build (d + 1)
      in
      let new_pes = build 0 in
      let pe_of_depth d = List.nth new_pes d in
      let fresh_v =
        make_counter ~start:(vpifo_start + Decorated.count_vpifos prev.decorated)
      in
      let fresh_s =
        make_counter ~start:(step_start + Decorated.count_steps prev.decorated)
      in
      let frag, decorated =
        compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:0
          ~splice:(path, prev.decorated) next
      in
      (* Repoint the fake root's single step from prev's real root to
         next's. The class set is unchanged (SuperPol preserves leaves),
         so the fake root's [Assoc]/[Map] entries don't move. *)
      let old_real_root_v = Decorated.vpifo prev.decorated in
      let rewire =
        [
          Emancipate (fake_root_step, fake_root_v, old_real_root_v);
          Adopt (fake_root_step, fake_root_v, frag.root_v);
        ]
      in
      let prog = frag_to_program frag @ rewire in
      Some { prog; decorated; pes = new_pes }
  | SubPol path ->
      (* [next] sits inside [prev] at [path]. Re-root the tree to that
         existing subtree: detach it from its parent, point the runtime at
         it, and GC every node that's no longer reachable. The discarded
         ancestors are best-effort cleaned via [Emancipate]/[GC] only — we
         don't bother [Unmap]/[Deassoc]ing routing state on them since
         they're collectable as a unit. *)
      let parent_path, k = list_foot path in
      let parent = Decorated.walk prev.decorated parent_path in
      let parent_v = Decorated.vpifo parent in
      let step_k = Decorated.nth_step parent k in
      let new_root = Decorated.nth_child parent k in
      let new_root_v = Decorated.vpifo new_root in
      let old_real_root_v = Decorated.vpifo prev.decorated in
      let kept = Decorated.subtree_vpifos new_root in
      let kept_set = List.fold_left (fun s v -> v :: s) [] kept in
      let to_gc =
        List.filter
          (fun v -> not (List.mem v kept_set))
          (Decorated.subtree_vpifos prev.decorated)
      in
      (* The fake root's class set shrinks to whatever the kept subtree
         covers; everything else gets [Unmap]'d and [Deassoc]'d off it. *)
      let old_classes = Decorated.subtree_classes prev.decorated in
      let kept_classes = Decorated.subtree_classes new_root in
      let dropped_classes =
        List.filter (fun c -> not (List.mem c kept_classes)) old_classes
      in
      let unmaps_dropped =
        List.map
          (fun c -> Unmap (fake_root_v, c, fake_root_step))
          dropped_classes
      in
      let deassocs_dropped =
        List.map (fun c -> Deassoc (fake_root_v, c)) dropped_classes
      in
      let prog =
        Emancipate (step_k, parent_v, new_root_v)
        :: Emancipate (fake_root_step, fake_root_v, old_real_root_v)
        :: Adopt (fake_root_step, fake_root_v, new_root_v)
        :: (unmaps_dropped @ deassocs_dropped @ List.map (fun v -> GC v) to_gc)
      in
      (* Re-rooting drops [List.length parent_path + 1] layers off the top
         of [prev.pes] — the survivors are everything from the new root
         downward. *)
      let drop = List.length path in
      let new_pes = List.filteri (fun i _ -> i >= drop) prev.pes in
      Some { prog; decorated = new_root; pes = new_pes }
  | OneArmReplaced { path = arm_path; arm } ->
      let parent_path, k = list_foot arm_path in
      let parent = Decorated.walk prev.decorated parent_path in
      let _ = parent_info parent in
      let removed = Decorated.nth_child parent k in
      let removed_v = Decorated.vpifo removed in
      let fresh_v =
        make_counter ~start:(vpifo_start + Decorated.count_vpifos prev.decorated)
      in
      let fresh_s =
        make_counter ~start:(step_start + Decorated.count_steps prev.decorated)
      in
      (* Compile the new arm at the depth of the slot it's replacing. The
         parent never [Adopt]s the new root directly — instead [Designate]
         fuses the old root and new root into a super-node that occupies
         the existing slot, riding on [step_k]. *)
      let arm_depth = List.length arm_path in
      let new_pes =
        pes_extended_to_depth (arm_depth + policy_depth arm) prev.pes
      in
      let pe_of_depth d = List.nth new_pes d in
      let arm_frag, arm_decorated =
        compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:arm_depth arm
      in
      let new_root_v = arm_frag.root_v in
      let new_classes = arm_frag.classes in
      let removed_classes = Decorated.subtree_classes removed in
      let chain = Decorated.ancestor_chain prev.decorated arm_path in
      (* Each ancestor of the slot held [Assoc]/[Map] entries for every
         class in the removed subtree; rewrite that routing state to the
         new subtree's classes instead, reusing the same step. *)
      let unmaps_old =
        List.concat_map
          (fun (anc_v, anc_step) ->
            List.map (fun c -> Unmap (anc_v, c, anc_step)) removed_classes)
          chain
      in
      let deassocs_old_anc =
        List.concat_map
          (fun (anc_v, _) ->
            List.map (fun c -> Deassoc (anc_v, c)) removed_classes)
          chain
      in
      let assocs_new_anc =
        List.concat_map
          (fun (anc_v, _) -> List.map (fun c -> Assoc (anc_v, c)) new_classes)
          chain
      in
      let maps_new =
        List.concat_map
          (fun (anc_v, anc_step) ->
            List.map (fun c -> Map (anc_v, c, anc_step)) new_classes)
          chain
      in
      (* Inside the removed subtree, each node was [Assoc]'d to the union
         of its descendants' classes. The subtree itself stays adopted
         under the super-node and drains naturally, but it must stop
         accepting new traffic of the old classes. *)
      let inner_deassocs =
        List.concat_map
          (fun (v, cs) -> List.map (fun c -> Deassoc (v, c)) cs)
          (Decorated.subtree_class_assocs removed)
      in
      let designate = [ Designate (removed_v, new_root_v) ] in
      let gcs = List.map (fun v -> GC v) (Decorated.subtree_vpifos removed) in
      let prog =
        List.concat
          [
            frag_to_program arm_frag;
            designate;
            inner_deassocs;
            unmaps_old;
            deassocs_old_anc;
            assocs_new_anc;
            maps_new;
            gcs;
          ]
      in
      let new_decorated =
        Decorated.rewrite_at prev.decorated parent_path
          (Decorated.replace_arm k arm_decorated)
      in
      Some { prog; decorated = new_decorated; pes = new_pes }
  | WeightChanged { path; new_weight } ->
      let parent_path, k = list_foot path in
      let parent = Decorated.walk prev.decorated parent_path in
      let parent_v =
        match parent with
        | Decorated.WFQ (v, _) -> v
        | _ -> failwith "Ir.patch: WeightChanged parent is not a WFQ"
      in
      let step_k = Decorated.nth_step parent k in
      let new_decorated =
        Decorated.rewrite_at prev.decorated parent_path
          (Decorated.set_weight k new_weight)
      in
      Some
        {
          prog = [ Change_weight (parent_v, step_k, new_weight) ];
          decorated = new_decorated;
          pes = prev.pes;
        }
  | OneArmAdded { path = arm_path; arm } ->
      let parent_path, k = list_foot arm_path in
      let parent = Decorated.walk prev.decorated parent_path in
      let parent_v, old_arity, pol_ty = parent_info parent in
      (* Seed the fresh-ID counters past whatever's already in [prev]. We
         derive these from the decorated tree on each call rather than
         caching them on [compiled] — [patch] is interactive (not a hot
         loop) and the walks are the same complexity class as the splice
         below, so the cost is noise. *)
      let fresh_v =
        make_counter ~start:(vpifo_start + Decorated.count_vpifos prev.decorated)
      in
      let fresh_s =
        make_counter ~start:(step_start + Decorated.count_steps prev.decorated)
      in
      (* Compile the new arm first so its internal vPIFO/step IDs land
         lower than the parent's new adopt-step ID — mirroring the
         pre-order numbering [of_policy]/[compile_arm] use. *)
      let arm_depth = List.length arm_path in
      let new_pes =
        pes_extended_to_depth (arm_depth + policy_depth arm) prev.pes
      in
      let pe_of_depth d = List.nth new_pes d in
      let arm_frag, arm_decorated =
        compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:arm_depth arm
      in
      let new_step = fresh_s () in
      let new_arity = old_arity + 1 in
      (* SP weights are positional: index [j] carries weight [j+1]. A
         mid-insert at [k] shifts every existing child at index [j ≥ k] to
         new index [j+1], so its weight must bump from [j+1] to [j+2]. The
         new arm itself takes weight [k+1]. RR/UNION carry no per-arm
         weights. WFQ never reaches this branch (Compare doesn't emit
         OneArmAdded for WFQ). *)
      let change_weights =
        match pol_ty with
        | SP ->
            let shifted =
              List.filter_map
                (fun (j, (s, _)) ->
                  if j >= k then
                    Some (Change_weight (parent_v, s, float_of_int (j + 2)))
                  else None)
                (List.mapi (fun j e -> (j, e)) (sp_edges parent))
            in
            Change_weight (parent_v, new_step, float_of_int (k + 1)) :: shifted
        | _ -> []
      in
      (* Build the parent's local splice instructions as a frag, then
         interleave with the new arm's frag through the same canonical
         ordering [of_policy] uses. *)
      let local =
        {
          spawns = [];
          adopts = [ Adopt (new_step, parent_v, arm_frag.root_v) ];
          assocs = List.map (fun c -> Assoc (parent_v, c)) arm_frag.classes;
          maps =
            List.map (fun c -> Map (parent_v, c, new_step)) arm_frag.classes;
          change_pols = [ Change_pol (parent_v, pol_ty, new_arity) ];
          change_weights;
          root_v = parent_v;
          classes = arm_frag.classes;
        }
      in
      let new_decorated =
        Decorated.rewrite_at prev.decorated parent_path
          (Decorated.insert_arm k new_step arm_decorated)
      in
      Some
        {
          prog = frag_to_program (combine_frags local [ arm_frag ]);
          decorated = new_decorated;
          pes = new_pes;
        }
  | OneArmRemoved { path = arm_path; arm = _ } ->
      let parent_path, k = list_foot arm_path in
      let parent = Decorated.walk prev.decorated parent_path in
      let parent_v, old_arity, pol_ty = parent_info parent in
      let removed = Decorated.nth_child parent k in
      let removed_v = Decorated.vpifo removed in
      let step_k = Decorated.nth_step parent k in
      let new_arity = old_arity - 1 in
      (* SP weights are positional: arm at index [j] carries weight [j+1].
         Removing index [k] shifts every sibling at [j > k] down to [j-1],
         so its weight drops from [j+1] to [j]. RR/UNION carry no per-arm
         weights. WFQ removal just drops the tuple — its weights are
         absolute, not positional. *)
      let change_weights =
        match pol_ty with
        | SP ->
            List.filter_map
              (fun (j, (s, _)) ->
                if j > k then Some (Change_weight (parent_v, s, float_of_int j))
                else None)
              (List.mapi (fun j e -> (j, e)) (sp_edges parent))
        | _ -> []
      in
      let change_pol = [ Change_pol (parent_v, pol_ty, new_arity) ] in
      (* Cleanup of routing state cached on each ancestor of the removed
         subtree. Every ancestor holds a [Map] and an [Assoc] per class in
         the removed subtree; we emit the matching [Unmap]/[Deassoc]. *)
      let chain = Decorated.ancestor_chain prev.decorated arm_path in
      let removed_classes = Decorated.subtree_classes removed in
      let unmaps =
        List.concat_map
          (fun (anc_v, anc_step) ->
            List.map (fun c -> Unmap (anc_v, c, anc_step)) removed_classes)
          chain
      in
      let ancestor_deassocs =
        List.concat_map
          (fun (anc_v, _) ->
            List.map (fun c -> Deassoc (anc_v, c)) removed_classes)
          chain
      in
      (* Inside the removed subtree, every node was [Assoc]'d to the union
         of its descendants' classes; emit a matching [Deassoc] per pair. *)
      let inner_deassocs =
        List.concat_map
          (fun (v, cs) -> List.map (fun c -> Deassoc (v, c)) cs)
          (Decorated.subtree_class_assocs removed)
      in
      let emancipate = [ Emancipate (step_k, parent_v, removed_v) ] in
      let gcs = List.map (fun v -> GC v) (Decorated.subtree_vpifos removed) in
      let prog =
        List.concat
          [
            change_weights;
            change_pol;
            unmaps;
            ancestor_deassocs;
            inner_deassocs;
            emancipate;
            gcs;
          ]
      in
      let new_decorated =
        Decorated.rewrite_at prev.decorated parent_path (Decorated.drop_arm k)
      in
      (* Removal can leave [pes] over-long if the dropped arm was the
         deepest in the tree, but extra trailing entries are harmless —
         they just go unreferenced until a future patch needs them. *)
      Some { prog; decorated = new_decorated; pes = prev.pes }

(* Re-export the per-program / per-instruction JSON exporters as a submodule
   so consumers say [Ir.Json.from_program]. The decorated tree on [compiled]
   is intentionally not serialized — it's runtime state for [patch], not
   part of the IR's external surface. *)
module Json = Json

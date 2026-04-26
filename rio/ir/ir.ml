(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include Instr

(* See [ir.mli] for the documented forms of these. *)
module Decorated = struct
  type t =
    | FIFO of vpifo * clss
    | UNION of vpifo * (step * t) list
    | SP of vpifo * (step * t) list
    | RR of vpifo * (step * t) list
    | WFQ of vpifo * (step * t * float) list
end

type compiled = {
  prog : program;
  decorated : Decorated.t;
}

(* Starting IDs for the two ID spaces. *)
let vpifo_start = 100
let step_start = 1000

(* A counter starting at [start]: returns a thunk that hands out fresh
   integers, the first being [start]. *)
let make_counter ~start =
  let n = ref (start - 1) in
  fun () ->
    incr n;
    !n

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

(* Flatten a [frag] into the canonical IR program ordering *)
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
let compile_FIFO ~v ~depth c : frag * Decorated.t =
  ( {
      spawns = [ Spawn (v, depth) ];
      assocs = [ Assoc (v, c) ];
      root_v = v;
      classes = [ c ];
      adopts = [];
      maps = [];
      change_pols = [];
      change_weights = [];
    },
    Decorated.FIFO (v, c) )

(* Compile a [Frontend.Policy.t] subtree at [depth]. Returns both the
   instruction fragment and the decorated-tree decoration that records the
   vPIFO/step IDs assigned to this subtree. PE assignment is depth-based —
   every node at depth [d] lives on PE [d]. Pure dispatcher: variant
   selection and SP weight synthesis happen here, and each variant wraps
   [compile_arm]'s edges in the matching [decorated] constructor. *)
let rec compile_subtree ~fresh_v ~fresh_s ~depth (p : Frontend.Policy.t) :
    frag * Decorated.t =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO c -> compile_FIFO ~v:(fresh_v ()) ~depth c
  | P.UNION children ->
      let frag, edges =
        compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty:UNION ~weights:[] children
      in
      (frag, Decorated.UNION (frag.root_v, edges))
  | P.RR children ->
      let frag, edges =
        compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty:RR ~weights:[] children
      in
      (frag, Decorated.RR (frag.root_v, edges))
  | P.SP children ->
      (* Strict priority: first child has priority 1.0 (highest), then 2.0, 3.0, … *)
      let weights = List.mapi (fun i _ -> float_of_int (i + 1)) children in
      let frag, edges =
        compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty:SP ~weights children
      in
      (frag, Decorated.SP (frag.root_v, edges))
  | P.WFQ (children, ws) ->
      let frag, edges =
        compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty:WFQ ~weights:ws children
      in
      let weighted = List.map2 (fun (s, d) w -> (s, d, w)) edges ws in
      (frag, Decorated.WFQ (frag.root_v, weighted))

(* Returns [(frag, edges)] where [edges] pairs each adopt-step with its
   child's decorated subtree, in source order. [weights] is empty for 
   UNION/RR (they don't carry weights) and one-per-arm for SP/WFQ. 
   List length parity with [children] is the caller's responsibility. *)
and compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty ~weights children :
    frag * (step * Decorated.t) list =
  (* Spawn self first, so that we get a lower ID number than the kids. *)
  let v = fresh_v () in
  let local_spawns = [ Spawn (v, depth) ] in
  (* Recurse on each child; List.map is left-to-right in the stdlib so vPIFO
     IDs come out in source order. *)
  let child_results =
    List.map
      (fun child -> compile_subtree ~fresh_v ~fresh_s ~depth:(depth + 1) child)
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

(* --- Decorated-tree helpers used by [patch]. ------------------------------ *)

(* Erase IR decorations from a decorated tree, recovering the source
   [Frontend.Policy.t]. Used by [patch] to feed [Rio_compare.Compare.analyze]
   without storing the policy alongside the decorated form. *)
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

(* Walk the decorated tree along [path] and return the subtree at that
   position. [[]] is the tree itself; [[i]] is the i-th child of the root. *)
let rec walk_to_decorated (d : Decorated.t) path =
  match (path, d) with
  | [], _ -> d
  | ( i :: rest,
      ( Decorated.UNION (_, edges)
      | Decorated.SP (_, edges)
      | Decorated.RR (_, edges) ) ) ->
      let _, child = List.nth edges i in
      walk_to_decorated child rest
  | i :: rest, Decorated.WFQ (_, edges) ->
      let _, child, _ = List.nth edges i in
      walk_to_decorated child rest
  | _ :: _, Decorated.FIFO _ ->
      failwith "Ir.patch: path goes through a FIFO leaf"

(* Inspect the decorated parent of a OneArmAppended splice point: returns
   the parent's vPIFO, current arity, and IR-side pol_ty in one shot. *)
let parent_info = function
  | Decorated.FIFO _ ->
      failwith "Ir.patch: OneArmAppended reported a FIFO as the parent"
  | Decorated.UNION (v, edges) -> (v, List.length edges, UNION)
  | Decorated.SP (v, edges) -> (v, List.length edges, SP)
  | Decorated.RR (v, edges) -> (v, List.length edges, RR)
  | Decorated.WFQ (v, edges) -> (v, List.length edges, WFQ)

(* Rebuild [d] so that the parent at [path] gains [(new_step, new_child)] as
   its new last child. WFQ-at-splice-point is unreachable under
   OneArmAppended (Compare doesn't generate it for WFQ), but WFQ in the path
   itself is allowed and preserved. *)
let rec splice_at_path (d : Decorated.t) path new_step new_child : Decorated.t =
  match path with
  | [] -> append_at_root d new_step new_child
  | i :: rest -> recurse_into d i rest new_step new_child

and append_at_root (d : Decorated.t) new_step new_child : Decorated.t =
  match d with
  | Decorated.UNION (v, edges) ->
      Decorated.UNION (v, edges @ [ (new_step, new_child) ])
  | Decorated.SP (v, edges) ->
      Decorated.SP (v, edges @ [ (new_step, new_child) ])
  | Decorated.RR (v, edges) ->
      Decorated.RR (v, edges @ [ (new_step, new_child) ])
  | Decorated.WFQ _ ->
      failwith "Ir.patch: WFQ-at-splice-point unreachable under OneArmAppended"
  | Decorated.FIFO _ -> failwith "Ir.patch: cannot splice into a FIFO"

and recurse_into (d : Decorated.t) i rest new_step new_child : Decorated.t =
  let update_at_i edges =
    List.mapi
      (fun j (s, c) ->
        if j = i then (s, splice_at_path c rest new_step new_child) else (s, c))
      edges
  in
  let update_at_i_wfq edges =
    List.mapi
      (fun j (s, c, w) ->
        if j = i then (s, splice_at_path c rest new_step new_child, w)
        else (s, c, w))
      edges
  in
  match d with
  | Decorated.UNION (v, edges) -> Decorated.UNION (v, update_at_i edges)
  | Decorated.SP (v, edges) -> Decorated.SP (v, update_at_i edges)
  | Decorated.RR (v, edges) -> Decorated.RR (v, update_at_i edges)
  | Decorated.WFQ (v, edges) -> Decorated.WFQ (v, update_at_i_wfq edges)
  | Decorated.FIFO _ -> failwith "Ir.patch: path goes through a FIFO leaf"

(* Count vPIFOs in [d] — one per node. Used by [patch] to seed its [fresh_v]
   counter so newly-allocated vPIFOs don't collide with any already in [d]. *)
let rec count_vpifos : Decorated.t -> int = function
  | Decorated.FIFO _ -> 1
  | Decorated.UNION (_, edges)
  | Decorated.SP (_, edges)
  | Decorated.RR (_, edges) ->
      1 + List.fold_left (fun acc (_, c) -> acc + count_vpifos c) 0 edges
  | Decorated.WFQ (_, edges) ->
      1 + List.fold_left (fun acc (_, c, _) -> acc + count_vpifos c) 0 edges

(* Count steps in [d] — one per parent→child edge. Used by [patch] to seed its
   [fresh_s] counter so newly-allocated step IDs don't collide with any
   already in [d]. *)
let rec count_steps : Decorated.t -> int = function
  | Decorated.FIFO _ -> 0
  | Decorated.UNION (_, edges)
  | Decorated.SP (_, edges)
  | Decorated.RR (_, edges) ->
      List.length edges
      + List.fold_left (fun acc (_, c) -> acc + count_steps c) 0 edges
  | Decorated.WFQ (_, edges) ->
      List.length edges
      + List.fold_left (fun acc (_, c, _) -> acc + count_steps c) 0 edges

(* --- Public entry points. ------------------------------------------------- *)

let of_policy (p : Frontend.Policy.t) : compiled =
  let fresh_v = make_counter ~start:vpifo_start in
  let fresh_s = make_counter ~start:step_start in
  let frag, decorated = compile_subtree ~fresh_v ~fresh_s ~depth:0 p in
  { prog = frag_to_program frag; decorated }

let patch ~prev ~(next : Frontend.Policy.t) : compiled option =
  let open Rio_compare.Compare in
  let prev_policy = policy_of_decorated prev.decorated in
  match analyze prev_policy next with
  | Same ->
      (* Nothing structural to do — return an empty delta. The decorated
         tree is immutable, so we hand back the same reference. *)
      Some { prog = []; decorated = prev.decorated }
  | Change (_, (VeryDifferent | SuperPol | ArmsAdded _)) -> None
  | Change (path, OneArmAppended arm) ->
      let parent = walk_to_decorated prev.decorated path in
      let parent_v, old_arity, pol_ty = parent_info parent in
      (* Seed the fresh-ID counters past whatever's already in [prev]. We
         derive these from the decorated tree on each call rather than
         caching them on [compiled] — [patch] is interactive (not a hot
         loop) and the walks are the same complexity class as the splice
         below, so the cost is noise. *)
      let fresh_v =
        make_counter ~start:(vpifo_start + count_vpifos prev.decorated)
      in
      let fresh_s =
        make_counter ~start:(step_start + count_steps prev.decorated)
      in
      (* Compile the new arm first so its internal vPIFO/step IDs land
         lower than the parent's new adopt-step ID — mirroring the
         pre-order numbering [of_policy]/[compile_arm] use. *)
      let arm_depth = List.length path + 1 in
      let arm_frag, arm_decorated =
        compile_subtree ~fresh_v ~fresh_s ~depth:arm_depth arm
      in
      let new_step = fresh_s () in
      let new_arity = old_arity + 1 in
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
          change_weights =
            (match pol_ty with
            | SP ->
                (* Strict priority: the new arm goes to the back of the
                   line, so its positional weight is the new arity. *)
                [ Change_weight (parent_v, new_step, float_of_int new_arity) ]
            | _ -> []);
          root_v = parent_v;
          classes = arm_frag.classes;
        }
      in
      let new_decorated =
        splice_at_path prev.decorated path new_step arm_decorated
      in
      Some
        {
          prog = frag_to_program (combine_frags local [ arm_frag ]);
          decorated = new_decorated;
        }

(* Re-export the per-program / per-instruction JSON exporters as a submodule
   so consumers say [Ir.Json.from_program]. The decorated tree on [compiled]
   is intentionally not serialized — it's runtime state for [patch], not
   part of the IR's external surface. *)
module Json = Json

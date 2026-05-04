(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include Instr

type compiled = {
  prog : program;
  decorated : Decorated.t;
  pes : pe list;
}

(* ------------------------------------------------------------------ *)
(* Generic helpers.                                                   *)
(* ------------------------------------------------------------------ *)

let list_foot xs =
  match List.rev xs with
  | [] -> invalid_arg "list_foot: empty list"
  | last :: rev_init -> (List.rev rev_init, last)

let make_counter ~start =
  let n = ref (start - 1) in
  fun () ->
    incr n;
    !n

let rec policy_depth (p : Frontend.Policy.t) : int =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO _ -> 0
  | P.UNION ps | P.SP ps | P.RR ps ->
      1 + List.fold_left max 0 (List.map policy_depth ps)
  | P.WFQ (ps, _) -> 1 + List.fold_left max 0 (List.map policy_depth ps)

(* ------------------------------------------------------------------ *)
(* ID-space and fake-root constants.                                  *)
(* ------------------------------------------------------------------ *)

(* Starting IDs for the two ID spaces. *)
let vpifo_start = 100
let step_start = 1000

(* The fake root sits one level above every real root, on PE -1. It exists so
   the real root always has an editable parent classifier, which means that we can handle whole-tree replacement, [SuperPol], and [SubPol] as ordinary
   parent-side edits. Reserved IDs below [vpifo_start]/[step_start] keep
   real-node numbering intact. The simulator never sees the fake root as a
   runtime node — [of_policy] just emits the wiring. *)
let fake_root_v : vpifo = 99
let fake_root_step : step = 999
let fake_root_pe : pe = -1

(* The fake-root chain has length 1: a single hop down to the real root. Reused
   wherever a slot-replacement helper needs an "ancestor chain" but the slot is
   the whole tree. *)
let fake_chain = [ (fake_root_v, fake_root_step) ]

(* ------------------------------------------------------------------ *)
(* Compile.                                                           *)
(* ------------------------------------------------------------------ *)

let compile_FIFO ~v ~pe c : Frag.t * Decorated.t =
  ( {
      (Frag.empty ~root_v:v ~classes:[ c ]) with
      spawns = [ Spawn (v, pe) ];
      assocs = [ Assoc (v, c) ];
    },
    Decorated.FIFO (v, c) )

(* Compile a [Frontend.Policy.t] subtree at [depth]. [splice], when supplied,
   names a path inside this subtree at which an already-installed [Decorated.t]
   should be grafted in instead of compiled afresh. *)
let rec compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth ?splice
    (p : Frontend.Policy.t) : Frag.t * Decorated.t =
  match splice with
  | Some ([], prev_d) -> (Frag.stub prev_d, prev_d)
  | _ -> (
      let module P = Frontend.Policy in
      let arm ~pol_ty ~weights children =
        compile_arm ~fresh_v ~fresh_s ~pe_of_depth ~depth ~pol_ty ~weights
          ?splice children
      in
      match p with
      | P.FIFO c -> compile_FIFO ~v:(fresh_v ()) ~pe:(pe_of_depth depth) c
      | P.UNION children ->
          let frag, edges = arm ~pol_ty:UNION ~weights:[] children in
          (frag, Decorated.UNION (frag.root_v, edges))
      | P.RR children ->
          let frag, edges = arm ~pol_ty:RR ~weights:[] children in
          (frag, Decorated.RR (frag.root_v, edges))
      | P.SP children ->
          (* Strict priority: positional weights 1.0, 2.0, … *)
          let weights = List.mapi (fun i _ -> float_of_int (i + 1)) children in
          let frag, edges = arm ~pol_ty:SP ~weights children in
          (frag, Decorated.SP (frag.root_v, edges))
      | P.WFQ (children, ws) ->
          let frag, edges = arm ~pol_ty:WFQ ~weights:ws children in
          let weighted = List.map2 (fun (s, d) w -> (s, d, w)) edges ws in
          (frag, Decorated.WFQ (frag.root_v, weighted)))

(* Returns [(frag, edges)] where [edges] pairs each adopt-step with its child's
   decorated subtree, in source order. [weights] is empty for UNION/RR, one
   per arm for SP/WFQ. [splice], when [Some (i :: rest, prev_d)], hands the
   splice down to the [i]-th child with its head consumed. *)
and compile_arm ~fresh_v ~fresh_s ~pe_of_depth ~depth ~pol_ty ~weights ?splice
    children : Frag.t * (step * Decorated.t) list =
  (* Self first, so that we get a lower vPIFO ID than the kids. *)
  let v = fresh_v () in
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
  let adoption_records =
    List.map
      (fun (cf : Frag.t) ->
        let s = fresh_s () in
        (Adopt (s, v, cf.root_v), s, cf))
      child_frags
  in
  let all_classes =
    List.concat_map (fun (cf : Frag.t) -> cf.classes) child_frags
  in
  let local : Frag.t =
    {
      spawns = [ Spawn (v, pe_of_depth depth) ];
      adopts = List.map (fun (a, _, _) -> a) adoption_records;
      assocs = List.map (fun c -> Assoc (v, c)) all_classes;
      maps =
        List.concat_map
          (fun (_, s, (cf : Frag.t)) ->
            List.map (fun c -> Map (v, c, s)) cf.classes)
          adoption_records;
      change_pols = [ Change_pol (v, pol_ty, List.length children) ];
      change_weights =
        (match weights with
        | [] -> []
        | ws ->
            List.map2
              (fun (_, s, _) w -> Change_weight (v, s, w))
              adoption_records ws);
      root_v = v;
      classes = all_classes;
    }
  in
  let edges =
    List.map2 (fun (_, s, _) d -> (s, d)) adoption_records child_decorated
  in
  (Frag.combine local child_frags, edges)

let of_policy (p : Frontend.Policy.t) : compiled =
  let fresh_v = make_counter ~start:vpifo_start in
  let fresh_s = make_counter ~start:step_start in
  let frag, decorated =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth:(fun d -> d) ~depth:0 p
  in
  (* Wrap the real root in the fake root: a UNION-of-1 carrying every class
     in the real tree, all routed via [fake_root_step]. *)
  let fake_frag : Frag.t =
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
  let pes = List.init (policy_depth p + 1) (fun d -> d) in
  { prog = Frag.to_program (Frag.combine fake_frag [ frag ]); decorated; pes }

(* ------------------------------------------------------------------ *)
(* Patch: helpers shared across the patch_* functions.                *)
(* ------------------------------------------------------------------ *)

let parent_info = function
  | Decorated.FIFO _ -> failwith "Ir.patch: parent is a FIFO leaf"
  | d -> (Decorated.root_vpifo d, Decorated.arity d, Decorated.pol_ty d)

(* Routing-state edits along an ancestor chain. Each ancestor caches an
   [Assoc]/[Map] entry per class in its descendant subtree (see
   [compile_arm]); add/remove these as classes enter or leave. *)
let chain_assocs chain classes =
  List.concat_map (fun (v, _) -> List.map (fun c -> Assoc (v, c)) classes) chain

let chain_maps chain classes =
  List.concat_map
    (fun (v, s) -> List.map (fun c -> Map (v, c, s)) classes)
    chain

let chain_unmaps chain classes =
  List.concat_map
    (fun (v, s) -> List.map (fun c -> Unmap (v, c, s)) classes)
    chain

let chain_deassocs chain classes =
  List.concat_map
    (fun (v, _) -> List.map (fun c -> Deassoc (v, c)) classes)
    chain

let gc_subtree subtree =
  List.map (fun v -> GC v) (Decorated.subtree_vpifos subtree)

let counters_after prev =
  ( make_counter ~start:(vpifo_start + Decorated.count_vpifos prev.decorated),
    make_counter ~start:(step_start + Decorated.count_steps prev.decorated) )

(* Grow [pes] so it covers [target_depth], allocating fresh PEs (above
   anything already in use) for any new layers. *)
let pes_extended_to_depth target_depth pes =
  let cur = List.length pes in
  if target_depth < cur then pes
  else
    let max_pe = List.fold_left max (-1) pes in
    let n = target_depth - cur + 1 in
    let rec extra k cur =
      if k <= 0 then [] else cur :: extra (k - 1) (cur + 1)
    in
    pes @ extra n (max_pe + 1)

(* ------------------------------------------------------------------ *)
(* Patch: arm replacement (covers OneArmReplaced and whole-tree).     *)
(* ------------------------------------------------------------------ *)

(* Replace the subtree [removed] (sitting under ancestor [chain]) with a
   freshly compiled [arm]. Used both for whole-tree replacement (chain =
   [fake_chain], removed = prev.decorated) and for per-arm replacement
   (chain = ancestor_chain prev arm_path, removed = nth_child parent k).

   The parent never adopts the new root directly: [Designate] fuses the old
   and new roots into a super-node that occupies the existing slot, so
   in-flight traffic on the old root drains while new traffic goes to the
   new root via the same step. *)
let replace_at ~prev ~chain ~removed ~arm_depth ~arm ~rewrite_decorated =
  let fresh_v, fresh_s = counters_after prev in
  let new_pes = pes_extended_to_depth (arm_depth + policy_depth arm) prev.pes in
  let pe_of_depth d = List.nth new_pes d in
  let arm_frag, arm_decorated =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:arm_depth arm
  in
  let removed_v = Decorated.root_vpifo removed in
  let removed_classes = Decorated.subtree_classes removed in
  let prog =
    List.concat
      [
        Frag.to_program arm_frag;
        [ Designate (removed_v, arm_frag.root_v) ];
        chain_unmaps chain removed_classes;
        chain_deassocs chain removed_classes;
        chain_assocs chain arm_frag.classes;
        chain_maps chain arm_frag.classes;
        gc_subtree removed;
      ]
  in
  Some { prog; decorated = rewrite_decorated arm_decorated; pes = new_pes }

let patch_one_arm_replaced ~prev ~arm_path ~arm =
  let parent_path, k = list_foot arm_path in
  let parent = Decorated.walk prev.decorated parent_path in
  ignore (parent_info parent);
  replace_at ~prev ~chain:(Decorated.ancestor_chain prev.decorated arm_path)
    ~removed:(Decorated.nth_child parent k)
    ~arm_depth:(List.length arm_path) ~arm ~rewrite_decorated:(fun arm_d ->
      Decorated.rewrite_at prev.decorated parent_path
        (Decorated.replace_arm k arm_d))

let patch_whole_tree_replace ~prev ~next =
  replace_at ~prev ~chain:fake_chain ~removed:prev.decorated ~arm_depth:0
    ~arm:next ~rewrite_decorated:(fun d -> d)

(* ------------------------------------------------------------------ *)
(* Patch: weight change (WFQ-only, structurally a no-op).             *)
(* ------------------------------------------------------------------ *)

let patch_weight_changed ~prev ~path ~new_weight =
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

(* ------------------------------------------------------------------ *)
(* Patch: arm add / remove.                                           *)
(* ------------------------------------------------------------------ *)

let sp_edges = function
  | Decorated.SP (_, es) -> es
  | _ -> failwith "Ir.patch: expected an SP parent"

(* SP weights are positional: arm at index [j] carries weight [j+1]. Inserting
   at [k] shifts every existing arm at [j ≥ k] up by one slot; removing at [k]
   shifts every arm at [j > k] down by one. Other policy types carry no
   per-arm weights here (WFQ doesn't reach add/remove). *)
let sp_inserted_weight_shifts ~parent_v ~parent ~k ~new_step =
  let shifted =
    List.filter_map
      (fun (j, (s, _)) ->
        if j >= k then Some (Change_weight (parent_v, s, float_of_int (j + 2)))
        else None)
      (List.mapi (fun j e -> (j, e)) (sp_edges parent))
  in
  Change_weight (parent_v, new_step, float_of_int (k + 1)) :: shifted

let sp_removed_weight_shifts ~parent_v ~parent ~k =
  List.filter_map
    (fun (j, (s, _)) ->
      if j > k then Some (Change_weight (parent_v, s, float_of_int j)) else None)
    (List.mapi (fun j e -> (j, e)) (sp_edges parent))

let patch_one_arm_added ~prev ~arm_path ~arm =
  let parent_path, k = list_foot arm_path in
  let parent = Decorated.walk prev.decorated parent_path in
  let parent_v, old_arity, pol_ty = parent_info parent in
  let fresh_v, fresh_s = counters_after prev in
  let arm_depth = List.length arm_path in
  let new_pes = pes_extended_to_depth (arm_depth + policy_depth arm) prev.pes in
  let pe_of_depth d = List.nth new_pes d in
  (* Compile the arm before allocating [new_step] so its internal IDs land
     lower — mirroring [of_policy]'s pre-order numbering. *)
  let arm_frag, arm_decorated =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:arm_depth arm
  in
  let new_step = fresh_s () in
  let change_weights =
    match pol_ty with
    | SP -> sp_inserted_weight_shifts ~parent_v ~parent ~k ~new_step
    | _ -> []
  in
  (* Strict ancestors above [parent_v] reuse their existing step toward
     [parent_v]; [parent_v] itself uses the freshly minted [new_step]. *)
  let chain =
    Decorated.ancestor_chain prev.decorated parent_path
    @ [ (parent_v, new_step) ]
  in
  let local : Frag.t =
    {
      spawns = [];
      adopts = [ Adopt (new_step, parent_v, arm_frag.root_v) ];
      assocs = chain_assocs chain arm_frag.classes;
      maps = chain_maps chain arm_frag.classes;
      change_pols = [ Change_pol (parent_v, pol_ty, old_arity + 1) ];
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
      prog = Frag.to_program (Frag.combine local [ arm_frag ]);
      decorated = new_decorated;
      pes = new_pes;
    }

let patch_one_arm_removed ~prev ~arm_path =
  let parent_path, k = list_foot arm_path in
  let parent = Decorated.walk prev.decorated parent_path in
  let parent_v, old_arity, pol_ty = parent_info parent in
  let removed = Decorated.nth_child parent k in
  let removed_v = Decorated.root_vpifo removed in
  let step_k = Decorated.nth_step parent k in
  let change_weights =
    match pol_ty with
    | SP -> sp_removed_weight_shifts ~parent_v ~parent ~k
    | _ -> []
  in
  let chain = Decorated.ancestor_chain prev.decorated arm_path in
  let removed_classes = Decorated.subtree_classes removed in
  let prog =
    List.concat
      [
        change_weights;
        [ Change_pol (parent_v, pol_ty, old_arity - 1) ];
        chain_unmaps chain removed_classes;
        chain_deassocs chain removed_classes;
        [ Emancipate (step_k, parent_v, removed_v) ];
        gc_subtree removed;
      ]
  in
  let new_decorated =
    Decorated.rewrite_at prev.decorated parent_path (Decorated.drop_arm k)
  in
  (* Removal can leave [pes] over-long if the dropped arm was the deepest
     in the tree, but extra trailing entries are harmless. *)
  Some { prog; decorated = new_decorated; pes = prev.pes }

(* ------------------------------------------------------------------ *)
(* Patch: super- and sub-policy.                                      *)
(* ------------------------------------------------------------------ *)

(* [prev]'s policy sits inside [next] at [path]: compile only the new structure
   surrounding [prev] and graft [prev]'s installed root in at the splice via
   [Adopt] (handled inside [compile_subtree]'s [splice] argument). Then repoint
   the fake root's single step from prev's old real root to next's new top. *)
let patch_super_pol ~prev ~next ~path =
  let len = List.length path in
  let prev_max_depth = List.length prev.pes - 1 in
  let next_max_depth = policy_depth next in
  (* New PE assignment: layers above [prev] get fresh PEs, layers occupied
     by [prev] reuse [prev.pes] (so already-installed nodes keep their PEs),
     layers deeper than [prev] get more fresh PEs. *)
  let pe_counter = make_counter ~start:(List.fold_left max (-1) prev.pes + 1) in
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
  let fresh_v, fresh_s = counters_after prev in
  let frag, decorated =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:0
      ~splice:(path, prev.decorated) next
  in
  let old_real_root_v = Decorated.root_vpifo prev.decorated in
  let rewire =
    [
      Emancipate (fake_root_step, fake_root_v, old_real_root_v);
      Adopt (fake_root_step, fake_root_v, frag.root_v);
    ]
  in
  Some { prog = Frag.to_program frag @ rewire; decorated; pes = new_pes }

(* [next] sits inside [prev] at [path]: re-root the tree to that existing
   subtree by detaching it from its parent and repointing the fake root.
   Discarded ancestors are best-effort cleaned via [Emancipate]/[GC]. *)
let patch_sub_pol ~prev ~path =
  let parent_path, k = list_foot path in
  let parent = Decorated.walk prev.decorated parent_path in
  let parent_v = Decorated.root_vpifo parent in
  let step_k = Decorated.nth_step parent k in
  let new_root = Decorated.nth_child parent k in
  let new_root_v = Decorated.root_vpifo new_root in
  let old_real_root_v = Decorated.root_vpifo prev.decorated in
  let kept_set = Decorated.subtree_vpifos new_root in
  let to_gc =
    List.filter
      (fun v -> not (List.mem v kept_set))
      (Decorated.subtree_vpifos prev.decorated)
  in
  let kept_classes = Decorated.subtree_classes new_root in
  let dropped_classes =
    List.filter
      (fun c -> not (List.mem c kept_classes))
      (Decorated.subtree_classes prev.decorated)
  in
  let prog =
    Emancipate (step_k, parent_v, new_root_v)
    :: Emancipate (fake_root_step, fake_root_v, old_real_root_v)
    :: Adopt (fake_root_step, fake_root_v, new_root_v)
    :: (chain_unmaps fake_chain dropped_classes
       @ chain_deassocs fake_chain dropped_classes
       @ List.map (fun v -> GC v) to_gc)
  in
  let new_pes = List.filteri (fun i _ -> i >= List.length path) prev.pes in
  Some { prog; decorated = new_root; pes = new_pes }

(* ------------------------------------------------------------------ *)
(* Patch: top-level dispatch.                                         *)
(* ------------------------------------------------------------------ *)

let patch ~prev ~(next : Frontend.Policy.t) : compiled option =
  let open Rio_compare.Compare in
  match analyze (Decorated.to_policy prev.decorated) next with
  | Same -> Some { prog = []; decorated = prev.decorated; pes = prev.pes }
  | OneArmReplaced { path = []; _ } -> patch_whole_tree_replace ~prev ~next
  | OneArmReplaced { path; arm } ->
      patch_one_arm_replaced ~prev ~arm_path:path ~arm
  | OneArmAdded { path; arm } -> patch_one_arm_added ~prev ~arm_path:path ~arm
  | OneArmRemoved { path; arm = _ } ->
      patch_one_arm_removed ~prev ~arm_path:path
  | WeightChanged { path; new_weight } ->
      patch_weight_changed ~prev ~path ~new_weight
  | SuperPol [] | SubPol [] -> None
  | SuperPol path -> patch_super_pol ~prev ~next ~path
  | SubPol path -> patch_sub_pol ~prev ~path

module Decorated = Decorated
module Json = Json

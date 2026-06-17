(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include Instr

type compiled = {
  commit : commit;
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

let rec policy_depth (p : Rio_core.Pol.t) : int =
  let module P = Rio_core.Pol in
  match p with
  | P.FIFO _ -> 0
  | P.SP (prs, _) | P.WFQ prs ->
      1 + List.fold_left max 0 (List.map (fun (p, _) -> policy_depth p) prs)
  | P.RR ps -> 1 + List.fold_left max 0 (List.map policy_depth ps)

(* ------------------------------------------------------------------ *)
(* ID-space and fake-root constants.                                  *)
(* ------------------------------------------------------------------ *)

(* Starting IDs for the two ID spaces. *)
let vpifo_start = 100
let step_start = 1000

(* The port root sits one level above every real root, on a reserved PE. It is
   the per-port classifier (per paper/sketch.md, the carrier that survives
   [Graft] and [ChangeRoot] swaps): every output port has a dedicated lPIFO
   whose sole adopted child is the actual tree root. Hosting an editable
   parent classifier means whole-tree replacement, [Graft], and [ChangeRoot]
   all reduce to ordinary parent-side edits. Reserved IDs below
   [vpifo_start]/[step_start] keep real-node numbering intact. *)
let port_root_v : vpifo = 99
let port_root_step : step = 999
let port_root_pe : pe = -1

(* The port-root chain has length 1: a single hop down to the real root. Reused
   wherever a slot-replacement helper needs an "ancestor chain" but the slot is
   the whole tree. *)
let port_chain = [ (port_root_v, port_root_step) ]

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

(* Compile a [Rio_core.Pol.t] subtree at [depth]. [splice], when supplied,
   names a path inside this subtree at which an already-installed [Decorated.t]
   should be grafted in instead of being compiled afresh. *)
let rec compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth ?splice
    (p : Rio_core.Pol.t) : Frag.t * Decorated.t =
  match splice with
  | Some ([], prev_d) -> (Frag.stub prev_d, prev_d)
  | _ -> (
      let module P = Rio_core.Pol in
      let arm ~pol_ty ~weights children =
        compile_arm ~fresh_v ~fresh_s ~pe_of_depth ~depth ~pol_ty ~weights
          ?splice children
      in
      match p with
      | P.FIFO c -> compile_FIFO ~v:(fresh_v ()) ~pe:(pe_of_depth depth) c
      | P.RR children ->
          let frag, edges = arm ~pol_ty:RR ~weights:[] children in
          (frag, Decorated.RR (frag.root_v, edges))
      | P.SP (arms, _) ->
          let children = List.map fst arms in
          let ranks = List.map snd arms in
          let frag, edges = arm ~pol_ty:SP ~weights:ranks children in
          let ranked = List.map2 (fun (s, d) r -> (s, d, r)) edges ranks in
          (frag, Decorated.SP (frag.root_v, ranked))
      | P.WFQ arms ->
          let children = List.map fst arms in
          let ws = List.map snd arms in
          let frag, edges = arm ~pol_ty:WFQ ~weights:ws children in
          let weighted = List.map2 (fun (s, d) w -> (s, d, w)) edges ws in
          (frag, Decorated.WFQ (frag.root_v, weighted)))

(* Returns [(frag, edges)] where [edges] pairs each adopt-step with its child's
   decorated subtree, in source order. [weights] is empty for RR, one per arm
   for SP/WFQ. [splice], when [Some (i :: rest, prev_d)], hands the splice down
   to the [i]-th child with its head consumed. *)
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
  let child_steps = List.map (fun _ -> fresh_s ()) child_frags in
  let all_classes =
    List.concat_map (fun (cf : Frag.t) -> cf.classes) child_frags
  in
  let local : Frag.t =
    {
      spawns = [ Spawn (v, pe_of_depth depth) ];
      adopts =
        List.map2
          (fun s (cf : Frag.t) -> Adopt (s, v, cf.root_v))
          child_steps child_frags;
      assocs = List.map (fun c -> Assoc (v, c)) all_classes;
      maps =
        List.concat
          (List.map2
             (fun s (cf : Frag.t) ->
               List.map (fun c -> Map (v, c, s)) cf.classes)
             child_steps child_frags);
      set_policies = [ Set_policy (v, pol_ty, List.length children) ];
      change_arities = [];
      set_arm_metas =
        (match weights with
        | [] -> []
        | ws -> List.map2 (fun s w -> Set_arm_meta (v, s, w)) child_steps ws);
      root_v = v;
      classes = all_classes;
    }
  in
  let edges = List.map2 (fun s d -> (s, d)) child_steps child_decorated in
  (Frag.combine local child_frags, edges)

let of_policy (p : Rio_core.Pol.t) : compiled =
  let fresh_v = make_counter ~start:vpifo_start in
  let fresh_s = make_counter ~start:step_start in
  let frag, decorated =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth:(fun d -> d) ~depth:0 p
  in
  (* Wrap the real root in the port root: a 1-arity carrier (labeled RR for
     IR purposes; at arity 1 the discipline is degenerate) that holds every
     class in the real tree and routes them all via [port_root_step]. *)
  let port_frag : Frag.t =
    {
      spawns = [ Spawn (port_root_v, port_root_pe) ];
      adopts = [ Adopt (port_root_step, port_root_v, frag.root_v) ];
      assocs = List.map (fun c -> Assoc (port_root_v, c)) frag.classes;
      maps =
        List.map (fun c -> Map (port_root_v, c, port_root_step)) frag.classes;
      set_policies = [ Set_policy (port_root_v, RR, 1) ];
      change_arities = [];
      set_arm_metas = [];
      root_v = port_root_v;
      classes = frag.classes;
    }
  in
  let pes = List.init (policy_depth p + 1) (fun d -> d) in
  { commit = Frag.to_commit (Frag.combine port_frag [ frag ]); decorated; pes }

(* ------------------------------------------------------------------ *)
(* Patch: helpers shared across the patch_* functions.                *)
(* ------------------------------------------------------------------ *)

let parent_info = function
  | Decorated.FIFO _ -> failwith "Ir.patch: parent is a FIFO leaf"
  | d -> (Decorated.root_vpifo d, Decorated.arity d, Decorated.pol_ty d)

(* Routing-state edits along an ancestor chain. Each ancestor caches an
   [Assoc]/[Map] entry per class in its descendant subtree (see
   [compile_arm]); add/remove these as classes enter or leave. Callers pick
   the per-(v, s, c) constructor — typically [Assoc]/[Deassoc] (which ignore
   [s]) or [Map]/[Unmap]. *)
let chain_emit f chain classes =
  List.concat_map (fun (v, s) -> List.map (fun c -> f v s c) classes) chain

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
    pes @ List.init n (fun i -> max_pe + 1 + i)

(* ------------------------------------------------------------------ *)
(* Patch: arm replacement.                                            *)
(* ------------------------------------------------------------------ *)

(* Replace the subtree [removed] (sitting under ancestor [chain]) with a
   freshly compiled [arm]. Used both for whole-tree replacement (chain =
   [port_chain], removed = prev.decorated) and for per-arm replacement
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
  let new_classes = arm_frag.classes in
  (* Classes shared between the removed subtree and the new arm keep their
     existing routing on every ancestor: the chain steps are preserved (the
     bottom-of-chain step survives because [Designate] reuses the parent's
     existing step, and ancestors above are untouched). So the only edits
     needed are on the symmetric difference. *)
  let only_removed =
    List.filter (fun c -> not (List.mem c new_classes)) removed_classes
  in
  let only_added =
    List.filter (fun c -> not (List.mem c removed_classes)) new_classes
  in
  let commit =
    List.concat
      [
        Frag.to_commit arm_frag;
        [ Designate (removed_v, arm_frag.root_v) ];
        chain_emit (fun v s c -> Unmap (v, c, s)) chain only_removed;
        chain_emit (fun v _ c -> Deassoc (v, c)) chain only_removed;
        chain_emit (fun v _ c -> Assoc (v, c)) chain only_added;
        chain_emit (fun v s c -> Map (v, c, s)) chain only_added;
        [ Undesignate removed_v ];
        gc_subtree removed;
      ]
  in
  { commit; decorated = rewrite_decorated arm_decorated; pes = new_pes }

(* [meta] is the new slot metadata supplied by [next]: a weight for a WFQ slot
   when the user changed both arm and weight in the same edit. SP rank-changes
   at a slot are not currently sniffed, so [meta] is None for SP replace. *)
let patch_one_arm_replaced ~prev ~arm_path ~arm ~meta =
  let parent_path, k = list_foot arm_path in
  let parent = Decorated.walk prev.decorated parent_path in
  let weight_instrs, weight_rewrite =
    match meta with
    | None -> ([], Fun.id)
    | Some m ->
        let parent_v = Decorated.root_vpifo parent in
        let step_k = Decorated.nth_step parent k in
        ([ Set_arm_meta (parent_v, step_k, m) ], Decorated.set_meta k m)
  in
  let c =
    replace_at ~prev ~chain:(Decorated.ancestor_chain prev.decorated arm_path)
      ~removed:(Decorated.nth_child parent k)
      ~arm_depth:(List.length arm_path) ~arm ~rewrite_decorated:(fun arm_d ->
        Decorated.rewrite_at prev.decorated parent_path (fun p ->
            p |> Decorated.replace_arm k arm_d |> weight_rewrite))
  in
  { c with commit = c.commit @ weight_instrs }

let patch_whole_tree_replace ~prev ~next =
  replace_at ~prev ~chain:port_chain ~removed:prev.decorated ~arm_depth:0
    ~arm:next ~rewrite_decorated:Fun.id

(* ------------------------------------------------------------------ *)
(* Patch: weight change (WFQ-only, structure of tree unchanged.       *)
(* ------------------------------------------------------------------ *)

let patch_meta_changed ~prev ~path ~new_meta =
  let parent_path, k = list_foot path in
  let parent = Decorated.walk prev.decorated parent_path in
  let parent_v =
    match parent with
    | Decorated.SP (v, _) | Decorated.WFQ (v, _) -> v
    | _ -> failwith "Ir.patch: ChangeMeta parent is not SP or WFQ"
  in
  let step_k = Decorated.nth_step parent k in
  let new_decorated =
    Decorated.rewrite_at prev.decorated parent_path
      (Decorated.set_meta k new_meta)
  in
  {
    commit = [ Set_arm_meta (parent_v, step_k, new_meta) ];
    decorated = new_decorated;
    pes = prev.pes;
  }

(* ------------------------------------------------------------------ *)
(* Patch: arm add / remove.                                           *)
(* ------------------------------------------------------------------ *)

(* Single-arm insertion under [parent]. *)
let patch_one_arm_added ~prev ~arm_path ~arm ~meta =
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
  let set_arm_metas, decorated_update =
    match (pol_ty, meta) with
    | SP, Some r ->
        ( [ Set_arm_meta (parent_v, new_step, r) ],
          Decorated.insert_arm_sp k new_step arm_decorated r )
    | WFQ, Some w ->
        ( [ Set_arm_meta (parent_v, new_step, w) ],
          Decorated.insert_arm_wfq k new_step arm_decorated w )
    | RR, None -> ([], Decorated.insert_arm k new_step arm_decorated)
    | _ ->
        failwith "Ir.patch.patch_one_arm_added: parent pol_ty / meta mismatch"
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
      assocs = chain_emit (fun v _ c -> Assoc (v, c)) chain arm_frag.classes;
      maps = chain_emit (fun v s c -> Map (v, c, s)) chain arm_frag.classes;
      set_policies = [];
      change_arities = [ Change_arity (parent_v, old_arity + 1) ];
      set_arm_metas;
      root_v = parent_v;
      classes = arm_frag.classes;
    }
  in
  let new_decorated =
    Decorated.rewrite_at prev.decorated parent_path decorated_update
  in
  {
    commit = Frag.to_commit (Frag.combine local [ arm_frag ]);
    decorated = new_decorated;
    pes = new_pes;
  }

(* Per-delta lowering for [Delta.Remove { path; arm }]. Emits the structural
   teardown ([Change_arity] shrink, parent-side [Emancipate], [GC] of the
   removed subtree) without touching class-routing tables: those are
   [Quiesce]'s responsibility within the [Retire] idiom, and the planner
   guarantees a [Quiesce(p)] step precedes every [Remove(p, _)] (modulo the
   guard between them). *)
let patch_remove ~prev ~arm_path =
  let parent_path, k = list_foot arm_path in
  let parent = Decorated.walk prev.decorated parent_path in
  let parent_v, old_arity, _ = parent_info parent in
  let removed = Decorated.nth_child parent k in
  let removed_v = Decorated.root_vpifo removed in
  let step_k = Decorated.nth_step parent k in
  let commit =
    List.concat
      [
        [ Change_arity (parent_v, old_arity - 1) ];
        [ Emancipate (step_k, parent_v, removed_v) ];
        gc_subtree removed;
      ]
  in
  let new_decorated =
    Decorated.rewrite_at prev.decorated parent_path (Decorated.drop_arm k)
  in
  (* Removal can leave [pes] over-long if the dropped arm was the deepest
     in the tree, but extra trailing entries are harmless. *)
  { commit; decorated = new_decorated; pes = prev.pes }

(* ------------------------------------------------------------------ *)
(* Patch: super- and sub-policy.                                      *)
(* ------------------------------------------------------------------ *)

(* [prev]'s policy sits inside [next] at [path]: compile only the new structure
   surrounding [prev] and graft [prev]'s installed root in at the splice via
   [Adopt] (handled inside [compile_subtree]'s [splice] argument). Then repoint
   the port root's single step from prev's old real root to next's new top. *)
let patch_graft ~prev ~next ~path =
  let len = List.length path in
  let prev_max_depth = List.length prev.pes - 1 in
  let next_max_depth = policy_depth next in
  (* New PE assignment: layers above [prev] get fresh PEs, layers occupied
     by [prev] reuse [prev.pes] (so already-installed nodes keep their PEs),
     layers deeper than [prev] get more fresh PEs. *)
  let pe_counter = make_counter ~start:(List.fold_left max (-1) prev.pes + 1) in
  let new_pes =
    List.init (next_max_depth + 1) (fun d ->
        if d < len then pe_counter ()
        else if d - len <= prev_max_depth then List.nth prev.pes (d - len)
        else pe_counter ())
  in
  let pe_of_depth d = List.nth new_pes d in
  let fresh_v, fresh_s = counters_after prev in
  let frag, decorated =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:0
      ~splice:(path, prev.decorated) next
  in
  let old_real_root_v = Decorated.root_vpifo prev.decorated in
  let prev_classes = Decorated.subtree_classes prev.decorated in
  let only_added =
    List.filter
      (fun c -> not (List.mem c prev_classes))
      (Decorated.subtree_classes decorated)
  in
  let rewire =
    [
      Emancipate (port_root_step, port_root_v, old_real_root_v);
      Adopt (port_root_step, port_root_v, frag.root_v);
    ]
    @ chain_emit (fun v _ c -> Assoc (v, c)) port_chain only_added
    @ chain_emit (fun v s c -> Map (v, c, s)) port_chain only_added
  in
  { commit = Frag.to_commit frag @ rewire; decorated; pes = new_pes }

(* [next] sits inside [prev] at [path]: re-root the tree to that existing
   subtree by repointing the port root's single step from prev's old real
   root to the new one. No detach from the surviving subtree's old parent is
   needed: the parent and every other discarded ancestor get [GC]'d, which
   already severs the parent-side edge. *)
let patch_change_root ~prev ~path =
  let parent_path, k = list_foot path in
  let parent = Decorated.walk prev.decorated parent_path in
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
  let commit =
    Emancipate (port_root_step, port_root_v, old_real_root_v)
    :: Adopt (port_root_step, port_root_v, new_root_v)
    :: (chain_emit (fun v s c -> Unmap (v, c, s)) port_chain dropped_classes
       @ chain_emit (fun v _ c -> Deassoc (v, c)) port_chain dropped_classes
       @ List.map (fun v -> GC v) to_gc)
  in
  let new_pes = List.filteri (fun i _ -> i >= List.length path) prev.pes in
  { commit; decorated = new_root; pes = new_pes }

(* ------------------------------------------------------------------ *)
(* Patch: per-production lowerings for Designate / Quiesce / Undesignate.
   [Planner.analyze] emits these inside the [Replace] idiom sequence; the
   top-level [Ir.patch] dispatch recognizes the sequence shape and routes
   it through [patch_one_arm_replaced] for decorated-tree threading, but
   the per-helper entry points below remain exposed for direct planner-
   level or test invocation of an individual production.            *)
(* ------------------------------------------------------------------ *)

(* The chain leading to [path]'s subtree, including the slot's own (parent_v,
   step_to_subtree). [Quiesce] and [Undesignate] route their class-level edits
   along this chain. *)
let full_chain_to ~prev path =
  match path with
  | [] -> port_chain
  | _ ->
      let parent_path, k = list_foot path in
      let parent = Decorated.walk prev.decorated parent_path in
      let parent_v = Decorated.root_vpifo parent in
      let step_k = Decorated.nth_step parent k in
      port_chain
      @ Decorated.ancestor_chain prev.decorated parent_path
      @ [ (parent_v, step_k) ]

(* [Designate { path; arm }]: introduce [arm] alongside the subtree at [path]
   under a designated super-node favoring the existing arm. Compile [arm]
   fresh; fuse it to the existing slot's v via [Designate (loser_v, survivor_v)];
   announce [loser_v]'s new role to the substrate via [Set_policy (loser_v,
   SP_star, 2)]; route any classes new to [arm] along [path]'s chain.

   The decorated tree is not updated: PR 9 doesn't model super-nodes in
   [Decorated.t] (planner work, PR 10). [pes] is extended if [arm] reaches
   deeper than [prev]. *)
let patch_designate ~prev ~path ~(arm : Rio_core.Pol.t) =
  let removed = Decorated.walk prev.decorated path in
  let loser_v = Decorated.root_vpifo removed in
  let fresh_v, fresh_s = counters_after prev in
  let arm_depth = List.length path in
  let new_pes = pes_extended_to_depth (arm_depth + policy_depth arm) prev.pes in
  let pe_of_depth d = List.nth new_pes d in
  let arm_frag, _arm_decorated =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:arm_depth arm
  in
  let chain = full_chain_to ~prev path in
  let removed_classes = Decorated.subtree_classes removed in
  let only_added =
    List.filter (fun c -> not (List.mem c removed_classes)) arm_frag.classes
  in
  let commit =
    List.concat
      [
        Frag.to_commit arm_frag;
        [ Designate (loser_v, arm_frag.root_v) ];
        [ Set_policy (loser_v, SP_star, 2) ];
        chain_emit (fun v _ c -> Assoc (v, c)) chain only_added;
        chain_emit (fun v s c -> Map (v, c, s)) chain only_added;
      ]
  in
  { commit; decorated = prev.decorated; pes = new_pes }

(* [Quiesce path]: stop new traffic from arriving at the subtree at [path] by
   tearing down the class-routing entries along its ancestor chain.
   [den(Quiesce) = id]: in-flight packets continue to dequeue from the subtree;
   nothing new lands. Decorated tree is unchanged (pol-invisible). *)
let patch_quiesce ~prev ~path =
  let target = Decorated.walk prev.decorated path in
  let target_classes = Decorated.subtree_classes target in
  let chain = full_chain_to ~prev path in
  let commit =
    chain_emit (fun v s c -> Unmap (v, c, s)) chain target_classes
    @ chain_emit (fun v _ c -> Deassoc (v, c)) chain target_classes
  in
  { commit; decorated = prev.decorated; pes = prev.pes }

(* [Undesignate path]: collapse the designated super-node at [path], letting
   the survivor take over the slot. We don't model super-nodes in
   [Decorated.t], so we emit the bare [Undesignate loser_v] and trust the
   substrate to rewire. A paired [GC loser_v] would normally follow in the
   same commit; the planner (PR 10) sequences that explicitly. *)
let patch_undesignate ~prev ~path =
  let removed = Decorated.walk prev.decorated path in
  let loser_v = Decorated.root_vpifo removed in
  {
    commit = [ Undesignate loser_v ];
    decorated = prev.decorated;
    pes = prev.pes;
  }

(* ------------------------------------------------------------------ *)
(* Patch: top-level dispatch.                                         *)
(* ------------------------------------------------------------------ *)

(* [Ir.patch] folds [Planner.analyze]'s guarded sequence one step at a time.
   Each atomic delta lowers to its own commit fragment via a per-delta helper;
   the running [compiled] state threads through so that downstream steps see
   the post-mutation decorated tree. The Retire idiom expands as [Quiesce ;
   Remove] and decomposes cleanly under the fold (class-routing edits come
   from [Quiesce]; structural teardown comes from [Remove]). PruneDownTo
   decomposes the same way: per-arm Retires followed by a [ChangeRoot] that
   GC's whatever ancestors remain.

   The Replace idiom ([Designate ; Quiesce ; Undesignate], +/- trailing
   [ChangeMeta]) is still recognized as a sequence-level atom and routed to
   [patch_one_arm_replaced] / [patch_whole_tree_replace]; the per-delta
   lowerings for [Designate]/[Undesignate] don't yet thread super-node state
   in [Decorated.t] (planned follow-up). Standalone [Designate]/[Undesignate]
   never appear outside a Replace block, so the fold refuses them. *)
let patch ~prev ~(next : Rio_core.Pol.t) : compiled option =
  let module D = Rio_delta.Delta in
  let module P = Rio_planner.Planner in
  let same_path a b c = a = b && b = c in
  let replace_block_path = function
    | [
        (P.True, D.Designate { path = pd; arm });
        (P.True, D.Quiesce pq);
        (P.Empty pu1, D.Undesignate pu2);
      ]
      when same_path pd pq pu1 && pu1 = pu2 -> Some (pd, arm, None)
    | [
        (P.True, D.Designate { path = pd; arm });
        (P.True, D.Quiesce pq);
        (P.Empty pu1, D.Undesignate pu2);
        (P.Empty pm1, D.ChangeMeta { path = pm2; new_meta });
      ]
      when same_path pd pq pu1 && same_path pu1 pu2 pm1 && pm1 = pm2 ->
        Some (pd, arm, Some new_meta)
    | _ -> None
  in
  let apply_step state (_guard, delta) : compiled =
    match delta with
    | D.Add { path; arm; meta } ->
        patch_one_arm_added ~prev:state ~arm_path:path ~arm ~meta
    | D.Remove { path; arm = _ } -> patch_remove ~prev:state ~arm_path:path
    | D.ChangeMeta { path; new_meta } ->
        patch_meta_changed ~prev:state ~path ~new_meta
    | D.Quiesce path -> patch_quiesce ~prev:state ~path
    | D.Graft path ->
        if path = [] then failwith "Ir.patch: whole-tree Graft is unsupported"
        else patch_graft ~prev:state ~next ~path
    | D.ChangeRoot path ->
        if path = [] then
          failwith "Ir.patch: whole-tree ChangeRoot is unsupported"
        else patch_change_root ~prev:state ~path
    | D.Designate _ | D.Undesignate _ ->
        failwith
          "Ir.patch: standalone Designate/Undesignate not yet supported \
           (handled only inside the Replace idiom)"
  in
  match P.analyze (Decorated.to_policy prev.decorated) next with
  | [] -> Some { commit = []; decorated = prev.decorated; pes = prev.pes }
  | seq -> (
      match replace_block_path seq with
      | Some (path, arm, meta) ->
          if path = [] then Some (patch_whole_tree_replace ~prev ~next)
          else Some (patch_one_arm_replaced ~prev ~arm_path:path ~arm ~meta)
      | None -> (
          try
            let final, all_commits =
              List.fold_left
                (fun (state, acc) step ->
                  let result = apply_step state step in
                  (result, acc @ result.commit))
                (prev, []) seq
            in
            Some { final with commit = all_commits }
          with Failure _ -> None))

module Decorated = Decorated
module Json = Json

(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include Instr

type compiled = {
  steps : (Rio_planner.Planner.guard * commit) list;
  decorated : Decorated.t;
  pes : pe list;
}

(* Helper for the per-delta lowering functions: each produces a single commit
   that fires immediately, so it wraps as a single-element [steps] list under
   the [True] guard. [Ir.patch]'s fold later re-wraps with the planner's
   actual guard. *)
let single ~commit ~decorated ~pes =
  { steps = [ (Rio_planner.Planner.True, commit) ]; decorated; pes }

(* Extract the single-step commit from a [compiled] built by [single]. The
   [failwith] arm is a debug-time invariant assertion: every caller (currently
   just the patch fold at the bottom of this file) holds a [compiled] that
   was constructed via [single] and therefore has exactly one step. *)
let commit_of_single c =
  match c.steps with
  | [ (_, cmt) ] -> cmt
  | _ -> failwith "Ir: expected a single-step compiled result"

let string_of_steps steps =
  let module P = Rio_planner.Planner in
  let guard_to_string = function
    | P.True -> "True"
    | P.Empty p -> Printf.sprintf "Empty %s" (Rio_delta.Delta.path_to_string p)
  in
  steps
  |> List.map (fun (g, c) ->
      Printf.sprintf "@%s:\n%s" (guard_to_string g) (string_of_commit c))
  |> String.concat "\n"

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

(* ------------------------------------------------------------------ *)
(* ID-space and fake-root constants.                                  *)
(* ------------------------------------------------------------------ *)

(* Starting IDs for the two ID spaces. *)
let pifo_start = 100
let step_start = 1000

(* The port root sits one level above every real root, on a reserved PE. It is
   the per-port classifier (per paper/sketch.md, the carrier that survives
   [Graft] and [ChangeRoot] swaps): every output port has a dedicated PIFO
   whose sole adopted child is the actual tree root. Hosting an editable
   parent classifier means whole-tree replacement, [Graft], and [ChangeRoot]
   all reduce to ordinary parent-side edits. Reserved IDs below
   [pifo_start]/[step_start] keep real-node numbering intact. *)
let port_root_v : pifo = 99
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
      | P.SP (arms, designated) ->
          let children = List.map fst arms in
          let ranks = List.map snd arms in
          let frag, edges = arm ~pol_ty:SP ~weights:ranks children in
          let ranked = List.map2 (fun (s, d) r -> (s, d, r)) edges ranks in
          (frag, Decorated.SP (frag.root_v, ranked, designated))
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
  (* Self first, so that we get a lower PIFO ID than the kids. *)
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
  let fresh_v = make_counter ~start:pifo_start in
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
  let pes = List.init (Rio_core.Pol.depth p + 1) (fun d -> d) in
  single
    ~commit:(Frag.to_commit (Frag.combine port_frag [ frag ]))
    ~decorated ~pes

(* ------------------------------------------------------------------ *)
(* Patch: helpers shared across the patch_* functions.                *)
(* ------------------------------------------------------------------ *)

let parent_info = function
  | Decorated.FIFO _ -> failwith "Ir.patch: parent is a FIFO leaf"
  | d -> (Decorated.root_pifo d, Decorated.arity d, Decorated.pol_ty d)

(* Routing-state edits along an ancestor chain. Each ancestor caches an
   [Assoc]/[Map] entry per class in its descendant subtree (see
   [compile_arm]); add/remove these as classes enter or leave. Callers pick
   the per-(v, s, c) constructor — typically [Assoc]/[Deassoc] (which ignore
   [s]) or [Map]/[Unmap]. *)
let chain_emit f chain classes =
  List.concat_map (fun (v, s) -> List.map (fun c -> f v s c) classes) chain

let gc_subtree subtree =
  List.map (fun v -> GC v) (Decorated.subtree_pifos subtree)

let counters_after prev =
  ( make_counter ~start:(pifo_start + Decorated.count_pifos prev.decorated),
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
(* Patch: weight change (WFQ-only, structure of tree unchanged.       *)
(* ------------------------------------------------------------------ *)

let patch_meta_changed ~prev ~path ~new_meta =
  let parent_path, k = list_foot path in
  let parent = Decorated.walk prev.decorated parent_path in
  let parent_v =
    match parent with
    | Decorated.SP (v, _, _) | Decorated.WFQ (v, _) -> v
    | _ -> failwith "Ir.patch: ChangeMeta parent is not SP or WFQ"
  in
  let step_k = Decorated.nth_step parent k in
  let new_decorated =
    Decorated.rewrite_at prev.decorated parent_path
      (Decorated.set_meta k new_meta)
  in
  single
    ~commit:[ Set_arm_meta (parent_v, step_k, new_meta) ]
    ~decorated:new_decorated ~pes:prev.pes

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
  let new_pes =
    pes_extended_to_depth (arm_depth + Rio_core.Pol.depth arm) prev.pes
  in
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
  (* Port root + strict ancestors above [parent_v] reuse their existing step
     toward [parent_v]; [parent_v] itself uses the freshly minted [new_step].
     Including [port_chain] ensures the new class is also routed at the port
     root, mirroring the real root. *)
  let chain =
    port_chain
    @ Decorated.ancestor_chain prev.decorated parent_path
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
  single
    ~commit:(Frag.to_commit (Frag.combine local [ arm_frag ]))
    ~decorated:new_decorated ~pes:new_pes

(* Per-delta lowering for [Delta.Remove path] (paper §6.2). Emits the
   structural teardown ([Change_arity] shrink, parent-side [Emancipate], [GC]
   of the removed subtree) plus the chain-side [Unmap] walk that retires the
   [Map] entries cached on every ancestor for each class in the doomed
   subtree. [Quiesce] has already torn down the matching [Assoc]s (and the
   subtree's interior [Map]s are GC'd along with the subtree); the chain-side
   [Map] entries above are load-bearing for in-flight packets up through the
   drain and must outlive [Quiesce], so they retire here once the subtree is
   empty. *)
let patch_remove ~prev ~arm_path =
  let parent_path, k = list_foot arm_path in
  let parent = Decorated.walk prev.decorated parent_path in
  let parent_v, old_arity, _ = parent_info parent in
  let removed = Decorated.nth_child parent k in
  let removed_v = Decorated.root_pifo removed in
  let step_k = Decorated.nth_step parent k in
  let doomed_classes = Decorated.subtree_classes removed in
  let chain_above =
    port_chain @ Decorated.ancestor_chain prev.decorated arm_path
  in
  let commit =
    List.concat
      [
        [ Change_arity (parent_v, old_arity - 1) ];
        [ Emancipate (step_k, parent_v, removed_v) ];
        chain_emit (fun v s c -> Unmap (v, c, s)) chain_above doomed_classes;
        gc_subtree removed;
      ]
  in
  let new_decorated =
    Decorated.rewrite_at prev.decorated parent_path (Decorated.drop_arm k)
  in
  (* Removal can leave [pes] over-long if the dropped arm was the deepest
     in the tree, but extra trailing entries are harmless. *)
  single ~commit ~decorated:new_decorated ~pes:prev.pes

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
  let next_max_depth = Rio_core.Pol.depth next in
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
  let old_real_root_v = Decorated.root_pifo prev.decorated in
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
  single ~commit:(Frag.to_commit frag @ rewire) ~decorated ~pes:new_pes

(* [next] sits inside [prev] at [path]: re-root the tree to that existing
   subtree by repointing the port root's single step from prev's old real
   root to the new one. No detach from the surviving subtree's old parent is
   needed: the parent and every other discarded ancestor get [GC]'d, which
   already severs the parent-side edge. *)
let patch_change_root ~prev ~path =
  let parent_path, k = list_foot path in
  let parent = Decorated.walk prev.decorated parent_path in
  let new_root = Decorated.nth_child parent k in
  let new_root_v = Decorated.root_pifo new_root in
  let old_real_root_v = Decorated.root_pifo prev.decorated in
  let kept_set = Decorated.subtree_pifos new_root in
  let to_gc =
    List.filter
      (fun v -> not (List.mem v kept_set))
      (Decorated.subtree_pifos prev.decorated)
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
  single ~commit ~decorated:new_root ~pes:new_pes

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
      let parent_v = Decorated.root_pifo parent in
      let step_k = Decorated.nth_step parent k in
      port_chain
      @ Decorated.ancestor_chain prev.decorated parent_path
      @ [ (parent_v, step_k) ]

(* The ancestor chain that routes traffic INTO the slot at [path]: every (v, s)
   pair from the port root down to and including the slot's direct parent's
   (parent_v, step_to_slot). The slot's own pifo is NOT included. Same shape
   as [full_chain_to ~prev path], but used by lowerings that want to address
   "the chain above this slot's pifo". *)
let chain_above_slot ~prev path =
  port_chain @ Decorated.ancestor_chain prev.decorated path

(* Resolve the (parent_v, parent_step) that adopts the slot at [path]. For
   [path = []] this is the port root; otherwise it's [path]'s direct parent. *)
let parent_edge ~prev path =
  if path = [] then (port_root_v, port_root_step)
  else
    let parent_path, k = list_foot path in
    let parent = Decorated.walk prev.decorated parent_path in
    (Decorated.root_pifo parent, Decorated.nth_step parent k)

(* [Designate { path; survivor }]: stand up a fresh SP* super-node at the slot
   holding the existing subtree (the loser). The new [arm] (the survivor)
   compiles fresh and is adopted as the SP*'s child at index 1; the loser sits
   at child index 0. PE placement: SP*, loser, and survivor's root all live on
   PE [pe_of_depth(arm_depth)], so the SP* layer is "free" depth-wise. The
   substrate (Zhiyuan) coalesces all three onto one logical slot.

   Class routing at SP*: loser-only classes route via [loser_step]; the
   survivor's classes (shared and survivor-only alike) route via [surv_step].
   Per paper §3.4.5, a label shared between the old subtree and the survivor
   belongs to the survivor from [Designate]'s firing onward. The favoring of
   the loser per ranks 1.0 < 2.0 still applies to loser-only classes routed
   to [loser_step]. The chain above SP* gains [Assoc]/[Map] only for classes
   new to the survivor — existing-class chain entries already point at
   [parent_step], which we rewire from loser to SP* via [Emancipate]+[Adopt]. *)
let patch_designate ~prev ~path ~(arm : Rio_core.Pol.t) =
  let loser = Decorated.walk prev.decorated path in
  let loser_v = Decorated.root_pifo loser in
  let loser_classes = Decorated.subtree_classes loser in
  let arm_depth = List.length path in
  let new_pes =
    pes_extended_to_depth (arm_depth + Rio_core.Pol.depth arm) prev.pes
  in
  let pe_n = List.nth new_pes arm_depth in
  let pe_of_depth d = List.nth new_pes d in
  let fresh_v, fresh_s = counters_after prev in
  let arm_frag, survivor_d =
    compile_subtree ~fresh_v ~fresh_s ~pe_of_depth ~depth:arm_depth arm
  in
  let survivor_v = arm_frag.root_v in
  let new_classes = arm_frag.classes in
  let only_new =
    List.filter (fun c -> not (List.mem c loser_classes)) new_classes
  in
  let loser_only =
    List.filter (fun c -> not (List.mem c new_classes)) loser_classes
  in
  let sp_v = fresh_v () in
  let loser_step = fresh_s () in
  let surv_step = fresh_s () in
  let parent_v, parent_step = parent_edge ~prev path in
  let chain_above_sp = chain_above_slot ~prev path in
  let commit =
    List.concat
      [
        Frag.to_commit arm_frag;
        [ Spawn (sp_v, pe_n) ];
        [
          Adopt (loser_step, sp_v, loser_v); Adopt (surv_step, sp_v, survivor_v);
        ];
        [ Designate (loser_v, survivor_v) ];
        [
          Emancipate (parent_step, parent_v, loser_v);
          Adopt (parent_step, parent_v, sp_v);
        ];
        List.map (fun c -> Assoc (sp_v, c)) loser_classes;
        List.map (fun c -> Assoc (sp_v, c)) only_new;
        List.map (fun c -> Map (sp_v, c, loser_step)) loser_only;
        List.map (fun c -> Map (sp_v, c, surv_step)) new_classes;
        chain_emit (fun v _ c -> Assoc (v, c)) chain_above_sp only_new;
        chain_emit (fun v s c -> Map (v, c, s)) chain_above_sp only_new;
        [ Set_policy (sp_v, SP_star, 2) ];
        [
          Set_arm_meta (sp_v, loser_step, 1.0);
          Set_arm_meta (sp_v, surv_step, 2.0);
        ];
      ]
  in
  let sp_node =
    Decorated.mk_sp_designated sp_v loser_step loser surv_step survivor_d
  in
  let new_decorated =
    if path = [] then sp_node
    else Decorated.rewrite_at prev.decorated path (fun _ -> sp_node)
  in
  single ~commit ~decorated:new_decorated ~pes:new_pes

(* Per-node Deassoc emits within a subtree (paper sketch.md §3.4.3 / §6.2:
   every leaf in T and every internal node within C@τ must be Deassoc'd in
   addition to the ancestor chain above C@τ). Quiesce emits no Unmap: silenced
   flows' Map entries must outlive the silencing so packets already in flight
   can route through the chain on the way to pop. [Isa_unmap] is paired with
   [Isa_emancipate] inside [Remove], which fires only after the drain. *)
let rec quiesce_interior (d : Decorated.t) : instr list =
  let v = Decorated.root_pifo d in
  match d with
  | Decorated.FIFO (_, c) -> [ Deassoc (v, c) ]
  | _ ->
      let children =
        match d with
        | Decorated.RR (_, es) -> List.map snd es
        | Decorated.SP (_, es, _) -> List.map (fun (_, c, _) -> c) es
        | Decorated.WFQ (_, es) -> List.map (fun (_, c, _) -> c) es
        | Decorated.FIFO _ -> assert false
      in
      let all_classes = List.concat_map Decorated.subtree_classes children in
      let deassocs = List.map (fun c -> Deassoc (v, c)) all_classes in
      let recurse = List.concat_map quiesce_interior children in
      deassocs @ recurse

(* [Quiesce path]: stop new traffic from arriving at the subtree at [path] by
   Deassoc'ing its leaves' classes at every node from the port root down
   through the target's interior to each leaf (paper §3.4.3 / §6.2). Quiesce
   strictly emits Deassoc only, never Unmap: live Map entries are load-bearing
   for in-flight packets that still need to route on the way to pop. The
   matching Unmap fires later inside [Remove]. [den(Quiesce) = id], so the
   decorated tree is unchanged.

   SP*-aware: when [path]'s direct parent is a designated SP, the slot is the
   loser inside a super-node. Above SP*, only loser-only classes are
   Deassoc'd; shared classes are preserved so the survivor still receives
   traffic through them. At SP*, only loser-only classes are Deassoc'd;
   shared-class Map entries already point at [surv_step] (installed by
   [Designate] at firing time per paper §3.4.5), so Quiesce has no Map work
   to do. The interior walk runs in both branches: the loser's interior
   Deassocs every target class top-to-bottom (shared classes included; the
   survivor reaches its own leaves via a separate chain). *)
let patch_quiesce ~prev ~path =
  let target = Decorated.walk prev.decorated path in
  let target_classes = Decorated.subtree_classes target in
  let parent_is_designated_sp =
    match path with
    | [] -> false
    | _ -> (
        let parent_path, _ = list_foot path in
        let p = Decorated.walk prev.decorated parent_path in
        match p with
        | Decorated.SP _ -> Decorated.sp_designated p
        | _ -> false)
  in
  let interior = quiesce_interior target in
  let commit =
    if not parent_is_designated_sp then
      let chain = full_chain_to ~prev path in
      chain_emit (fun v _ c -> Deassoc (v, c)) chain target_classes @ interior
    else
      let parent_path, k = list_foot path in
      let sp_parent = Decorated.walk prev.decorated parent_path in
      let sp_v = Decorated.root_pifo sp_parent in
      let surv_idx = 1 - k in
      let survivor_classes =
        Decorated.subtree_classes (Decorated.nth_child sp_parent surv_idx)
      in
      let only_loser =
        List.filter (fun c -> not (List.mem c survivor_classes)) target_classes
      in
      let chain_up = chain_above_slot ~prev parent_path in
      chain_emit (fun v _ c -> Deassoc (v, c)) chain_up only_loser
      @ List.map (fun c -> Deassoc (sp_v, c)) only_loser
      @ interior
  in
  single ~commit ~decorated:prev.decorated ~pes:prev.pes

(* [Undesignate path]: collapse the designated SP* at [path]. The loser subtree
   has drained (the planner gates this on [Empty (path ++ [0])]); the survivor
   at child index 1 takes over the slot. Emits [Undesignate loser_v] as the
   §6-ISA-facing marker and GCs SP* + the loser subtree. The parent rewire is
   implicit: paper §6.1 specifies that [Isa_undesignate(v)] collapses the
   super-node so the parent index that pointed at [{v -> surv}] now points
   directly at [surv], and the same-PE invariant on (sp_v, loser-root,
   survivor-root) means the substrate can perform that rewire locally without
   an extra [Emancipate]/[Adopt] pair from us. *)
let patch_undesignate ~prev ~path =
  let sp_node = Decorated.walk prev.decorated path in
  let sp_v = Decorated.root_pifo sp_node in
  let loser_d = Decorated.nth_child sp_node 0 in
  let loser_v = Decorated.root_pifo loser_d in
  let survivor_d = Decorated.nth_child sp_node 1 in
  let commit = [ Undesignate loser_v; GC sp_v ] @ gc_subtree loser_d in
  let new_decorated =
    if path = [] then survivor_d
    else Decorated.rewrite_at prev.decorated path (fun _ -> survivor_d)
  in
  single ~commit ~decorated:new_decorated ~pes:prev.pes

(* ------------------------------------------------------------------ *)
(* Patch: top-level dispatch.                                         *)
(* ------------------------------------------------------------------ *)

(* [Ir.patch] folds [Planner.analyze]'s guarded sequence one step at a time.
   Each atomic delta lowers to its own commit fragment via a per-delta helper;
   the running [compiled] state threads through so that downstream steps see
   the post-mutation decorated tree. Every paper-side idiom decomposes
   mechanically: Retire = [Quiesce ; Remove]; Replace = [Designate ; Quiesce
   ; Undesignate ; (ChangeMeta)?]; PruneDownTo = per-arm Retires followed by
   a [ChangeRoot] that GC's the remaining ancestors. *)
let patch ~prev ~(next : Rio_core.Pol.t) : compiled option =
  let module D = Rio_delta.Delta in
  let apply_step state (_guard, delta) : compiled =
    match delta with
    | D.Add { path; arm; meta } ->
        patch_one_arm_added ~prev:state ~arm_path:path ~arm ~meta
    | D.Remove path -> patch_remove ~prev:state ~arm_path:path
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
    | D.Designate { path; survivor } ->
        patch_designate ~prev:state ~path ~arm:survivor
    | D.Undesignate path -> patch_undesignate ~prev:state ~path
  in
  match
    Rio_planner.Planner.analyze (Decorated.to_policy prev.decorated) next
  with
  | [] -> Some { steps = []; decorated = prev.decorated; pes = prev.pes }
  | seq -> (
      try
        let final, rev_steps =
          List.fold_left
            (fun (state, acc) ((guard, _delta) as step) ->
              let result = apply_step state step in
              let step_commit = commit_of_single result in
              (result, (guard, step_commit) :: acc))
            (prev, []) seq
        in
        Some { final with steps = List.rev rev_steps }
      with Failure _ -> None)

module Decorated = Decorated
module Json = Json

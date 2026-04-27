(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include Instr

(* See [ir.mli] for the documented forms of these. *)
type node_id = int list

type identities = {
  vpifos : (node_id, vpifo) Hashtbl.t;
  steps : (node_id * int, step) Hashtbl.t;
}

type compiled = {
  prog : program;
  policy : Frontend.Policy.t;
  identities : identities;
  next_vpifo : int;
  next_step : int;
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

(* Split a non-empty list into its prefix and its last element.
   E.g. [list_foot [1; 2; 3]] = ([1; 2], 3). Raises on empty input. *)
let list_foot xs =
  match List.rev xs with
  | [] -> invalid_arg "list_foot: empty list"
  | last :: rev_init -> (List.rev rev_init, last)

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

(** Complete all the necessary steps to stand up the FIFO. *)
let compile_FIFO ~v ~depth ~path ~identities c =
  Hashtbl.add identities.vpifos path v;
  {
    spawns = [ Spawn (v, depth) ];
    assocs = [ Assoc (v, c) ];
    root_v = v;
    classes = [ c ];
    adopts = [];
    maps = [];
    change_pols = [];
    change_weights = [];
  }

(* Compile a [Frontend.Policy.t] subtree rooted at [depth]. [path] is the tree
   position of this subtree's root within the top-level policy, used to key
   the identity tables. PE assignment is depth-based — every node at depth
   [d] lives on [pe d]. This function is just a dispatcher: it picks the
   right helper for the variant and synthesizes weights for SP. *)
let rec compile_subtree ~fresh_v ~fresh_s ~depth ~path ~identities
    (p : Frontend.Policy.t) : frag =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO c -> compile_FIFO ~v:(fresh_v ()) ~depth ~path ~identities c
  | P.UNION children ->
      compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty:UNION
        ~weights:[] children
  | P.RR children ->
      compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty:RR
        ~weights:[] children
  | P.SP children ->
      (* Strict priority: first child has priority 1.0 (highest), then 2.0, 3.0, … *)
      let weights = List.mapi (fun i _ -> float_of_int (i + 1)) children in
      compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty:SP ~weights
        children
  | P.WFQ (children, ws) ->
      compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty:WFQ
        ~weights:ws children

(* [weights] is empty for UNION/RR (they don't carry weights) and
   one-per-arm for SP/WFQ. List length parity with [children] is the
   caller's responsibility — [List.map2] below will raise on mismatch. *)
and compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty ~weights
    children =
  (* Spawn self first, so that we get a lower ID number than the kids. *)
  let v = fresh_v () in
  Hashtbl.add identities.vpifos path v;
  let local_spawns = [ Spawn (v, depth) ] in
  (* Recurse on each child first; List.mapi is left-to-right in the stdlib so
     vpifo IDs come out in source order, and [i] gives us the child's index
     for both the path extension and the (parent_path, child_index) step
     key. *)
  let child_frags =
    List.mapi
      (fun i child ->
        compile_subtree ~fresh_v ~fresh_s ~depth:(depth + 1)
          ~path:(path @ [ i ]) ~identities child)
      children
  in
  (* Adopt each child, and store the fresh step ID that we get at the moment
     we adopt it. We register every step ID in [identities.steps] keyed by
     (this node's path, the child's index). *)
  let adoption_records =
    List.mapi
      (fun i cf ->
        let s = fresh_s () in
        Hashtbl.add identities.steps (path, i) s;
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
  combine_frags local child_frags

let of_policy (p : Frontend.Policy.t) : compiled =
  let fresh_v = make_counter ~start:vpifo_start in
  let fresh_s = make_counter ~start:step_start in
  (* Identity tables are created here and mutated in place as
     [compile_subtree]/[compile_arm] register each spawn and adopt. *)
  let identities = { vpifos = Hashtbl.create 16; steps = Hashtbl.create 16 } in
  let frag =
    compile_subtree ~fresh_v ~fresh_s ~depth:0 ~path:[] ~identities p
  in
  (* Each [Hashtbl.add] in [compile_FIFO]/[compile_arm] corresponds to
     exactly one tick of the counter, so the table sizes tell us how many
     IDs were handed out and therefore what the next one would be. *)
  {
    prog = frag_to_program frag;
    policy = p;
    identities;
    next_vpifo = vpifo_start + Hashtbl.length identities.vpifos;
    next_step = step_start + Hashtbl.length identities.steps;
  }

(* Shallow copy of the identity tables. The values are ints, so a shallow
   copy is sufficient to ensure later mutations on the result don't leak
   back into the source. *)
let clone_identities { vpifos; steps } =
  { vpifos = Hashtbl.copy vpifos; steps = Hashtbl.copy steps }

(* Walk a policy tree along [path] and return the subtree at that
   position. Used to find the parent constructor (and arity) at the
   location reported by [Compare.analyze]. *)
let rec walk_to (p : Frontend.Policy.t) path =
  let module P = Frontend.Policy in
  match (path, p) with
  | [], _ -> p
  | i :: rest, (P.UNION ps | P.SP ps | P.RR ps) -> walk_to (List.nth ps i) rest
  | i :: rest, P.WFQ (ps, _) -> walk_to (List.nth ps i) rest
  | _ :: _, P.FIFO _ -> failwith "Ir.patch: path goes through a FIFO leaf"

(* Translate a [Frontend.Policy.t] variant to its IR [pol_ty] counterpart.
   Used in [patch] where the variant has been recovered via [walk_to] and
   we need its IR-side label. [compile_subtree] doesn't go through this —
   its surrounding pattern match makes the correspondence visible at a
   glance. *)
let pol_ty_of_policy (p : Frontend.Policy.t) : pol_ty =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO _ -> FIFO
  | P.UNION _ -> UNION
  | P.SP _ -> SP
  | P.RR _ -> RR
  | P.WFQ _ -> WFQ

let patch ~prev ~(next : Frontend.Policy.t) : compiled option =
  let open Rio_compare.Compare in
  match analyze prev.policy next with
  | Same ->
      (* Nothing structural to do — return an empty delta. We still hand
         back a fresh identities clone so the caller can mutate the
         result without affecting [prev]. *)
      Some
        {
          prog = [];
          policy = next;
          identities = clone_identities prev.identities;
          next_vpifo = prev.next_vpifo;
          next_step = prev.next_step;
        }
  | VeryDifferent _
  | SuperPol _
  | SubPol _
  | ArmsAdded _
  | ArmsRemoved _
  | WeightChanged _
  | OneArmReplaced _ -> None
  | OneArmAppended { path = arm_path; arm; weight = _ } ->
      let parent_path, old_arity = list_foot arm_path in
      let parent_pol = walk_to prev.policy parent_path in
      let pol_ty = pol_ty_of_policy parent_pol in
      let parent_v =
        try Hashtbl.find prev.identities.vpifos parent_path
        with Not_found ->
          failwith "Ir.patch: parent path missing from identity table"
      in
      let identities = clone_identities prev.identities in
      let fresh_v = make_counter ~start:prev.next_vpifo in
      let fresh_s = make_counter ~start:prev.next_step in
      (* Compile the new arm first so its internal vPIFO/step IDs land
         lower than the parent's new adopt-step ID — mirroring the
         pre-order numbering [of_policy]/[compile_arm] use. *)
      let arm_depth = List.length arm_path in
      let arm_frag =
        compile_subtree ~fresh_v ~fresh_s ~depth:arm_depth ~path:arm_path
          ~identities arm
      in
      let new_step = fresh_s () in
      Hashtbl.add identities.steps (parent_path, old_arity) new_step;
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
      Some
        {
          prog = frag_to_program (combine_frags local [ arm_frag ]);
          policy = next;
          identities;
          next_vpifo = vpifo_start + Hashtbl.length identities.vpifos;
          next_step = step_start + Hashtbl.length identities.steps;
        }

(* Re-export the per-program / per-instruction JSON exporters as a submodule
   so consumers say [Ir.Json.from_program]. The [identities] / counter
   metadata on [compiled] is intentionally not serialized — it's runtime
   state for [patch], not part of the IR's external surface. *)
module Json = Json

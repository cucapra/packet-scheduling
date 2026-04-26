(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include Instr

(* See [ir.mli] for the documented forms of these. *)
type path = int list

type compiled = {
  prog : program;
  policy : Frontend.Policy.t;
}

(* Encode a list of base-10 digits as an integer with the given leading
   sentinel digit. E.g. [encode_path ~sentinel:1 [0; 2]] = [102]. *)
let encode_path ~sentinel digits =
  List.fold_left
    (fun acc d ->
      if d < 0 || d > 9 then
        failwith
          "Ir: path component out of base-10 range (max arity per level: 10)";
      (acc * 10) + d)
    sentinel digits

(* vPIFO ID for the node at [path]. *)
let vpifo_of_path (path : path) : vpifo = encode_path ~sentinel:1 path

(* Step ID that the node at [parent_path] uses to reach its [child_index]th child. *)
let step_of_path (parent_path : path) (child_index : int) : step =
  encode_path ~sentinel:2 (parent_path @ [ child_index ])

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
let compile_FIFO ~depth ~path c =
  let v = vpifo_of_path path in
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
   position of this subtree's root within the top-level policy; vPIFO and
   step IDs are derived from [path] via [vpifo_of_path] / [step_of_path].
   PE assignment is depth-based — every node at depth [d] lives on [pe d].
   This function is just a dispatcher: it picks the right helper for the
   variant and synthesizes weights for SP. *)
let rec compile_subtree ~depth ~path (p : Frontend.Policy.t) : frag =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO c -> compile_FIFO ~depth ~path c
  | P.UNION children ->
      compile_arm ~depth ~path ~pol_ty:UNION ~weights:[] children
  | P.RR children -> compile_arm ~depth ~path ~pol_ty:RR ~weights:[] children
  | P.SP children ->
      (* Strict priority: first child has priority 1.0 (highest), then 2.0, 3.0, … *)
      let weights = List.mapi (fun i _ -> float_of_int (i + 1)) children in
      compile_arm ~depth ~path ~pol_ty:SP ~weights children
  | P.WFQ (children, ws) ->
      compile_arm ~depth ~path ~pol_ty:WFQ ~weights:ws children

(* [weights] is empty for UNION/RR (they don't carry weights) and
   one-per-arm for SP/WFQ. List length parity with [children] is the
   caller's responsibility — [List.map2] below will raise on mismatch. *)
and compile_arm ~depth ~path ~pol_ty ~weights children =
  let v = vpifo_of_path path in
  let local_spawns = [ Spawn (v, depth) ] in
  (* Recurse on each child first; List.mapi is left-to-right in the stdlib so
     the recursive walk visits children in source order. *)
  let child_frags =
    List.mapi
      (fun i child ->
        compile_subtree ~depth:(depth + 1) ~path:(path @ [ i ]) child)
      children
  in
  (* Adopt each child. The step ID for the [i]-th adoption is [step_of_path path i]. *)
  let adoption_records =
    List.mapi
      (fun i cf ->
        let s = step_of_path path i in
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
  let frag = compile_subtree ~depth:0 ~path:[] p in
  { prog = frag_to_program frag; policy = p }

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
  let module P = Frontend.Policy in
  let open Rio_compare.Compare in
  match analyze prev.policy next with
  | Same -> Some { prog = []; policy = next }
  | Change (_, (VeryDifferent | SuperPol | ArmsAdded _)) -> None
  | Change (path, OneArmAppended arm) ->
      let parent_pol = walk_to prev.policy path in
      let old_arity =
        match parent_pol with
        | P.UNION ps | P.SP ps | P.RR ps -> List.length ps
        | P.WFQ (ps, _) -> List.length ps
        | P.FIFO _ ->
            failwith "Ir.patch: OneArmAppended reported a FIFO as the parent"
      in
      let pol_ty = pol_ty_of_policy parent_pol in
      let parent_v = vpifo_of_path path in
      let arm_path = path @ [ old_arity ] in
      let arm_depth = List.length path + 1 in
      let arm_frag = compile_subtree ~depth:arm_depth ~path:arm_path arm in
      let new_step = step_of_path path old_arity in
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
        }

(* Re-export the per-program / per-instruction JSON exporters as a submodule
   so consumers say [Ir.Json.from_program]. *)
module Json = Json

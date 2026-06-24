(** The planner sits between [Rio_core.Pol] and [Rio_ir]. Its job is to take a
    pair of policies [(p1, p2)] and emit a guarded sequence of atomic
    [Rio_delta.Delta] productions whose composed effect carries [p1] to [p2].
    Each step in the sequence is a pair [(guard, delta)]: the guard is a
    predicate on live runtime state (paper sketch.md sec4), and the delta is one
    atomic production from the [Delta] grammar.

    The guard alphabet is intentionally tiny:
    - [True]: fire immediately, no precondition.
    - [Empty path]: fire once the subtree at [path] in [p1] has drained. *)

open Rio_core.Pol
module Delta = Rio_delta.Delta

type guard =
  | True
  | Empty of Delta.path

type step = guard * Delta.t
type t = step list

(* -------- list helpers --------------------------------------- *)

(* If [next] embeds [prev] as a strict sub-sequence, return the indices in
   [next]'s frame at which the extra arms sit (in ascending order). Used
   by the length-differing branches of the comparators to recognize a
   multi-arm Add (with [prev]=ps1, [next]=ps2) or a multi-arm Retire (with
   the arguments swapped). [None] signals "not a sub-sequence" and the
   caller falls back to label-set alignment. *)
let insertions prev next =
  let rec loop i prev next acc =
    match (prev, next) with
    | [], [] -> Some (List.rev acc)
    | [], x :: t -> loop (i + 1) [] t ((i, x) :: acc)
    | x1 :: t1, x2 :: t2 ->
        if x1 = x2 then loop (i + 1) t1 t2 acc
        else loop (i + 1) prev t2 ((i, x2) :: acc)
    | _ :: _, [] -> None
  in
  loop 0 prev next []

(* The set of class labels reachable from a policy: each [FIFO] leaf
   contributes its class; SP/WFQ/RR fold up their children's labels. A
   well-formed policy keeps these distinct across arms; the planner's
   [compare_children] fallback below treats label overlap between two arms
   as evidence that they denote the same flow that morphed structurally
   (paper sketch.md sec5.2 case 8, sec3.2 leaf-partition validity). *)
let rec arm_labels = function
  | FIFO c -> [ c ]
  | SP (prs, _) | WFQ prs -> List.concat_map (fun (p, _) -> arm_labels p) prs
  | RR ps -> List.concat_map arm_labels ps

let labels_overlap a b =
  let lb = arm_labels b in
  List.exists (fun x -> List.mem x lb) (arm_labels a)

(* Bidirectional label-set alignment, used by the comparators' fallback
   branches when [insertions]'s strict subsequence check has failed. Pairs
   each [ps1] arm with its unique label-overlapping [ps2] arm; unmatched
   arms on either side flow through as [ps1_only] (retires) and [ps2_only]
   (adds). The same call therefore handles pure-add, pure-retire, and
   mixed cases (e.g. [RR(A, B) -> RR(A, C, D)]: matches A-A, retires B,
   adds C and D). Returns [None] when an arm overlaps multiple partners,
   when matched [ps2] indices fail to strictly increase, or when no arms
   match at all (caller falls back to a slot-level [Replace]). The
   strict-increasing check is automatic post-[Pol.normalize]. *)
let align_by_labels_bidir ps1 ps2 =
  let ps2_indexed = List.mapi (fun j a -> (j, a)) ps2 in
  let rec walk i matches_acc ps1_only_acc last_j = function
    | [] -> Some (List.rev matches_acc, List.rev ps1_only_acc)
    | a1 :: t -> (
        match List.filter (fun (_, a2) -> labels_overlap a1 a2) ps2_indexed with
        | [] -> walk (i + 1) matches_acc ((i, a1) :: ps1_only_acc) last_j t
        | [ (j, a2) ] when j > last_j ->
            walk (i + 1) ((i, j, a1, a2) :: matches_acc) ps1_only_acc j t
        | _ -> None)
  in
  match walk 0 [] [] (-1) ps1 with
  | Some ((_ :: _ as matches), ps1_only) ->
      let matched_j = List.map (fun (_, j, _, _) -> j) matches in
      let ps2_only =
        List.filter (fun (j, _) -> not (List.mem j matched_j)) ps2_indexed
      in
      Some (matches, ps1_only, ps2_only)
  | _ -> None

(* -------- sequence helpers ---------------------------------- *)

let prepend_guard i = function
  | True -> True
  | Empty p -> Empty (i :: p)

let prepend_step i (g, d) = (prepend_guard i g, Delta.prepend_path i d)
let prepend_seq i seq = List.map (prepend_step i) seq

(* The paper's [Replace] idiom: swap in [next] at the current level. Expands
   into [Designate(p, next) ; Quiesce(p ++ [0]) ; (Empty (p ++ [0])) Undesignate(p)],
   where [p] is the slot's path. After [Designate] fires, the slot becomes a
   designated SP* with the loser at child index 0 and the survivor at child
   index 1; [Quiesce] tears down the loser's class routing, and [Undesignate]
   waits for the loser to drain before collapsing the SP* down to the
   survivor. When the swap also rebinds the slot's per-arm meta (an SP rank or
   WFQ weight changing in lockstep with the arm), a final [ChangeMeta] step
   fires immediately after [Undesignate] (sequential dependency suffices: the
   loser has drained by the time [Undesignate]'s guard fires, and [ChangeMeta]
   sits in the next step slot).

   Also serves as [analyze]'s give-up sequence: when no smaller atomic edit
   applies, [Replace] fires at the divergent slot.

   All paths are emitted as [[]] at this level; bubble-up via [prepend_seq]
   pins them to the right slot. *)
let replace ~next ?meta () =
  let base =
    [
      (True, Delta.Designate { path = []; survivor = next });
      (True, Delta.Quiesce [ 0 ]);
      (Empty [ 0 ], Delta.Undesignate []);
    ]
  in
  match meta with
  | None -> base
  | Some m -> base @ [ (True, Delta.ChangeMeta { path = []; new_meta = m }) ]

(* The paper's [Retire] idiom: quiesce the subtree at the current level, then
   structurally remove its arm once it has drained. Paths are emitted as [[]]
   at this level; [prepend_seq] pins them when the sequence bubbles up. *)
let retire () = [ (True, Delta.Quiesce []); (Empty [], Delta.Remove []) ]

(* The paper's [PruneDownTo] idiom: collapse [prev] down to the strict subtree
   at [path]. Walks [prev] along [path], retiring off-path siblings at each
   level, then re-roots with a final [ChangeRoot]. Off-path retires within a
   level fire highest-index-first so the lower-index siblings keep their
   emitted paths stable; across levels we go outer-first (the order is
   interchangeable because outer retires don't shift inner paths and vice
   versa, but the natural walk produces outer-first). Once every off-path
   sibling along the route has retired, the survivor is the sole remaining
   child at index 0 at each level, so the final re-root path is all-zeros of
   length [len(path)]. *)
let prune_down_to ~prev ~path () =
  let arms_of = function
    | SP (prs, _) | WFQ prs -> List.map fst prs
    | RR ps -> ps
    | FIFO _ -> failwith "Planner.prune_down_to: path goes through a FIFO leaf"
  in
  let rec walk p path_remaining path_so_far =
    match path_remaining with
    | [] -> []
    | i :: rest ->
        let arms = arms_of p in
        let n = List.length arms in
        let off =
          List.init n (fun j -> j)
          |> List.filter (fun j -> j <> i)
          |> List.sort (fun a b -> compare b a)
        in
        let level_retires =
          List.concat_map
            (fun j ->
              List.fold_right prepend_seq (path_so_far @ [ j ]) (retire ()))
            off
        in
        let kept = List.nth arms i in
        level_retires @ walk kept rest (path_so_far @ [ i ])
  in
  walk prev path [] @ [ (True, Delta.ChangeRoot (List.map (fun _ -> 0) path)) ]

(* Recognize the inner [Replace] shape (pre-bubble-up). Used by the metaed
   comparator to fold a slot's meta change into the trailing [ChangeMeta]
   of a [Replace] when both fire at the same slot. *)
let is_replace_root = function
  | [
      (True, Delta.Designate { path = []; _ });
      (True, Delta.Quiesce [ 0 ]);
      (Empty [ 0 ], Delta.Undesignate []);
    ] -> true
  | _ -> false

(* Emit retires for the [(idx, _)] slots in descending index order, so a
   higher-index retire doesn't shift the lower-index paths still pending.
   Shared by the multi-arm retire branches and the [emit_bidir_*]
   helpers. *)
let retires_descending entries =
  let descending = List.sort (fun (a, _) (b, _) -> compare b a) entries in
  List.concat_map (fun (i, _) -> prepend_seq i (retire ())) descending

(* Shared emit shape for an [align_by_labels_bidir] result. Retires fire
   first (in descending [ps1]-frame index), then adds (ascending
   [ps2]-frame index; each add lands in the post-retire intermediate
   frame, which by ascending construction equals the final ps2 frame),
   then per-match arm edits at their [ps2]-frame indices.

   [slot_arm_edit] is the caller's per-slot edit closure (it recurses into
   [analyze_inner]); the unmetaed variant skips equal-arm matches
   outright, the metaed variant additionally emits a trailing [ChangeMeta]
   when the meta also changed. *)
let emit_bidir_unmetaed ~slot_arm_edit matches ps1_only ps2_only =
  let adds =
    List.map
      (fun (j, arm) -> (True, Delta.Add { path = [ j ]; arm; meta = None }))
      ps2_only
  in
  let matched =
    List.concat_map
      (fun (_, j, arm1, arm2) ->
        if arm1 = arm2 then [] else slot_arm_edit j arm1 arm2)
      matches
  in
  retires_descending ps1_only @ adds @ matched

let emit_bidir_metaed ~slot_arm_edit ~ms1 ~ms2 matches ps1_only ps2_only =
  let adds =
    List.map
      (fun (j, arm) ->
        (True, Delta.Add { path = [ j ]; arm; meta = Some (List.nth ms2 j) }))
      ps2_only
  in
  let edit_match (i1, j2, arm1, arm2) =
    let m1 = List.nth ms1 i1 in
    let m2 = List.nth ms2 j2 in
    let meta = if m1 = m2 then None else Some m2 in
    match (arm1 = arm2, meta) with
    | true, None -> []
    | true, Some new_meta ->
        [ (True, Delta.ChangeMeta { path = [ j2 ]; new_meta }) ]
    | false, _ -> slot_arm_edit j2 arm1 arm2 ?meta ()
  in
  let matched = List.concat_map edit_match matches in
  retires_descending ps1_only @ adds @ matched

(* Fallback combinator for [compare_*]: try bidirectional label-set
   alignment, emit it if found, otherwise give up. [emit] curries the
   relevant [emit_bidir_*]'s shared arguments. *)
let bidir_or ~emit ~give_up ps1 ps2 =
  match align_by_labels_bidir ps1 ps2 with
  | None -> give_up
  | Some (matches, ps1_only, ps2_only) -> emit matches ps1_only ps2_only

(* Same-length entry point: trigger bidir only when its alignment reveals
   a genuine cross-slot match ([i1 <> j2] for some pair); when every match
   aligns at the same slot, the per-slot walk is at least as good and
   runs instead. *)
let try_bidir_cross_slot ~emit ~per_slot ps1 ps2 =
  match align_by_labels_bidir ps1 ps2 with
  | Some (matches, ps1_only, ps2_only)
    when List.exists (fun (i1, j2, _, _) -> i1 <> j2) matches ->
      emit matches ps1_only ps2_only
  | _ -> per_slot ()

(* -------- sub-policy test ----------------------------------- *)

let rec is_sub_policy p1 p2 =
  if p1 = p2 then Some []
  else
    let scan children ~arm =
      let rec loop i = function
        | [] -> None
        | x :: t -> (
            match is_sub_policy p1 (arm x) with
            | Some path -> Some (i :: path)
            | None -> loop (i + 1) t)
      in
      loop 0 children
    in
    match p2 with
    | FIFO _ -> None
    | SP (prs, _) | WFQ prs -> scan prs ~arm:fst
    | RR ps -> scan ps ~arm:Fun.id

let rec compare_children ~next:p2 ps1 ps2 =
  let give_up = replace ~next:p2 () in
  (* [analyze_inner] never returns a [PruneDownTo] (top-level only; see
     [analyze]), so the inner sequence always bubbles cleanly via
     [prepend_seq]. *)
  let slot_arm_edit i arm1 arm2 = prepend_seq i (analyze_inner arm1 arm2) in
  let emit = emit_bidir_unmetaed ~slot_arm_edit in
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      (* Per-slot walk pairs arms at the same index; bidir picks up
         cross-slot morphs that [Pol.normalize]'s sort exposed. *)
      let per_slot () =
        List.concat
          (List.mapi
             (fun i (arm1, arm2) ->
               if arm1 = arm2 then [] else slot_arm_edit i arm1 arm2)
             (List.combine ps1 ps2))
      in
      try_bidir_cross_slot ~emit ~per_slot ps1 ps2
  | -1 ->
      (* Multi-arm add: every surplus entry in [ps2] is a pure addition (the
         arms of [ps1] appear in-order as a subsequence of [ps2]). One [Add]
         per extra in ascending index order; bidir fallback when the strict
         subsequence fails. *)
      begin match insertions ps1 ps2 with
      | Some adds ->
          List.map
            (fun (i, arm) ->
              (True, Delta.Add { path = [ i ]; arm; meta = None }))
            adds
      | None -> bidir_or ~emit ~give_up ps1 ps2
      end
  | 1 ->
      (* Multi-arm retire: symmetric to [-1]. *)
      begin match insertions ps2 ps1 with
      | Some retires -> retires_descending retires
      | None -> bidir_or ~emit ~give_up ps1 ps2
      end
  | _ -> failwith "Can't get here"

(* SP and WFQ share shape [(t * float) list]. Same-length parents get a
   per-slot walk; length-differing parents align via the arm projection's
   subsequence-embedding and compare metas at shared arms afterwards.
   Slot-level edits at distinct indices don't interfere, so they
   concatenate freely. *)
and compare_metaed_children ~next:p2 pms1 pms2 =
  let give_up = replace ~next:p2 () in
  let ps1 = List.map fst pms1 in
  let ps2 = List.map fst pms2 in
  let ms1 = List.map snd pms1 in
  let ms2 = List.map snd pms2 in
  (* Emit a slot-level edit at index [i], carrying [arm1] to [arm2] and
     optionally rebinding the slot's meta. A [Replace]-root inner absorbs
     any meta change into its own trailing [ChangeMeta]; otherwise the
     inner sequence bubbles via [prepend_seq] with a separate [ChangeMeta]
     for the meta change. *)
  let slot_arm_edit i arm1 arm2 ?meta () =
    let inner = analyze_inner arm1 arm2 in
    match (meta, is_replace_root inner) with
    | Some m, true ->
        let survivor =
          match inner with
          | (True, Delta.Designate { survivor; _ }) :: _ -> survivor
          | _ -> assert false
        in
        prepend_seq i (replace ~next:survivor ~meta:m ())
    | Some m, false ->
        prepend_seq i inner
        @ [ (True, Delta.ChangeMeta { path = [ i ]; new_meta = m }) ]
    | None, _ -> prepend_seq i inner
  in
  let emit = emit_bidir_metaed ~slot_arm_edit ~ms1 ~ms2 in
  (* Per-slot row at a same-length parent: arm difference -> slot_arm_edit
     (with meta when the meta also differs); meta-only difference ->
     [ChangeMeta]; both agree -> nothing. *)
  let per_slot_row i (arm1, m1) (arm2, m2) =
    match (arm1 <> arm2, m1 <> m2) with
    | false, false -> []
    | false, true ->
        [ (True, Delta.ChangeMeta { path = [ i ]; new_meta = m2 }) ]
    | true, false -> slot_arm_edit i arm1 arm2 ()
    | true, true -> slot_arm_edit i arm1 arm2 ~meta:m2 ()
  in
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      (* Permutation pre-pass: when [ps1] and [ps2] hold the same arm
         multiset in different positions, the difference is purely a meta
         shuffle exposed by [Pol.normalize]'s sort. Emit one [ChangeMeta]
         per slot whose meta moved; leaf-partition validity makes the
         per-slot lookup unique. *)
      let arms_only = List.sort compare in
      if ps1 <> ps2 && arms_only ps1 = arms_only ps2 then
        let indexed_ps2 = List.mapi (fun j a -> (j, a)) ps2 in
        List.concat
          (List.mapi
             (fun i arm1 ->
               let j, _ = List.find (fun (_, a) -> a = arm1) indexed_ps2 in
               let new_meta = List.nth ms2 j in
               if List.nth ms1 i = new_meta then []
               else [ (True, Delta.ChangeMeta { path = [ i ]; new_meta }) ])
             ps1)
      else
        let per_slot () =
          List.concat
            (List.mapi
               (fun i ((arm1, m1), (arm2, m2)) ->
                 per_slot_row i (arm1, m1) (arm2, m2))
               (List.combine (List.combine ps1 ms1) (List.combine ps2 ms2)))
        in
        try_bidir_cross_slot ~emit ~per_slot ps1 ps2
  | -1 ->
      (* [ps1] embeds in [ps2] as a subsequence: surplus arms become [Add]s
         carrying their metas (ascending [ps2] index), and shared arms whose
         meta differs contribute a trailing [ChangeMeta] in [ps2]'s frame. *)
      begin match insertions ps1 ps2 with
      | Some adds ->
          let add_indices = List.map fst adds in
          let add_steps =
            List.map
              (fun (i, arm) ->
                ( True,
                  Delta.Add { path = [ i ]; arm; meta = Some (List.nth ms2 i) }
                ))
              adds
          in
          let change_meta_steps =
            List.filter_map
              (fun j ->
                if List.mem j add_indices then None
                else
                  let i1 =
                    j - List.length (List.filter (fun k -> k < j) add_indices)
                  in
                  let m1 = List.nth ms1 i1 in
                  let m2 = List.nth ms2 j in
                  if m1 = m2 then None
                  else
                    Some (True, Delta.ChangeMeta { path = [ j ]; new_meta = m2 }))
              (List.init (List.length ps2) (fun j -> j))
          in
          add_steps @ change_meta_steps
      | None -> bidir_or ~emit ~give_up ps1 ps2
      end
  | 1 ->
      (* Symmetric to [-1]: surplus [ps1] arms retire (descending so
         lower-index paths stay stable); shared arms whose metas differ
         take a [ChangeMeta] in [ps2]'s post-Retire frame. *)
      begin match insertions ps2 ps1 with
      | Some retires ->
          let retire_indices = List.map fst retires in
          let ps1_matched =
            List.filter
              (fun i -> not (List.mem i retire_indices))
              (List.init (List.length ps1) (fun i -> i))
          in
          let change_meta_steps =
            List.filter_map
              (fun (j, i1) ->
                let m1 = List.nth ms1 i1 in
                let m2 = List.nth ms2 j in
                if m1 = m2 then None
                else
                  Some (True, Delta.ChangeMeta { path = [ j ]; new_meta = m2 }))
              (List.mapi (fun j i1 -> (j, i1)) ps1_matched)
          in
          retires_descending retires @ change_meta_steps
      | None -> bidir_or ~emit ~give_up ps1 ps2
      end
  | _ -> failwith "Can't get here"

(* Inner recursion entry: same as [analyze] but skips the [is_sub_policy]
   check. [PruneDownTo]'s tail is [ChangeRoot], a whole-tree edit whose
   path names a position in the *root* control; it cannot be retargeted
   into a parent slot by [prepend_seq] (paper sketch.md sec5.2). So inner
   recursions never emit [PruneDownTo]: a sub-policy embedding at a
   deeper level falls through to the constructor cases and emits a
   slot-level [Replace], which is the same observable behavior as the
   previous "demote to give-up" rule. *)
and analyze_inner p1 p2 =
  if p1 = p2 then []
  else
    match (p1, p2) with
    | SP (pms1, _), SP (pms2, _) | WFQ pms1, WFQ pms2 ->
        compare_metaed_children ~next:p2 pms1 pms2
    | RR ps1, RR ps2 -> compare_children ~next:p2 ps1 ps2
    | _ ->
        (* FIFO->FIFO with different class, or any constructor mismatch
           (FIFO<->SP, SP<->RR, etc.): whole-slot give-up at this level.
           The leaf-level paths are empty; [prepend_seq] pins them when
           this bubbles up. *)
        replace ~next:p2 ()

let analyze p1 p2 =
  if p1 = p2 then []
  else
    match is_sub_policy p2 p1 with
    | Some path -> prune_down_to ~prev:p1 ~path ()
    | None -> analyze_inner p1 p2

(* -------- pretty-printing (test output only) ---------------- *)

let string_of_guard = function
  | True -> "True"
  | Empty p -> Printf.sprintf "Empty %s" (Delta.path_to_string p)

let string_of_step (g, d) =
  Printf.sprintf "(%s, %s)" (string_of_guard g) (Delta.to_string d)

let to_string seq = "[" ^ String.concat "; " (List.map string_of_step seq) ^ "]"

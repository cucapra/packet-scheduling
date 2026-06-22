(** The planner sits between [Rio_core.Pol] and [Rio_ir]. Its job is to take a
    pair of policies [(p1, p2)] and emit a guarded sequence of atomic
    [Rio_delta.Delta] productions whose composed effect carries [p1] to [p2].
    Each step in the sequence is a pair [(guard, delta)]: the guard is a
    predicate on live runtime state (paper sketch.md sec4), and the delta is one
    atomic production from the [Delta] grammar.

    Today's guard alphabet is intentionally tiny:
    - [True]: fire immediately, no precondition.
    - [Empty path]: fire once the subtree at [path] in [p1] has drained. *)

open Rio_core.Pol
module Delta = Rio_delta.Delta

type guard =
  | True
  | Empty of Delta.path

type step = guard * Delta.t
type t = step list

(* -------- list helpers (moved from compare/delta) ----------- *)

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
   (paper many-arms.md). *)
let rec arm_labels = function
  | FIFO c -> [ c ]
  | SP (prs, _) | WFQ prs -> List.concat_map (fun (p, _) -> arm_labels p) prs
  | RR ps -> List.concat_map arm_labels ps

let labels_overlap a b =
  let lb = arm_labels b in
  List.exists (fun x -> List.mem x lb) (arm_labels a)

(* Greedy label-set alignment for [compare_children]'s length-differing
   branches: when [insertions] fails because a matched arm has morphed
   (lengths differ AND a shared arm changed structurally), line up [short]
   inside [long] by overlapping label sets instead of by structural
   equality. Walks both lists once: pairs the heads if their label sets
   overlap, otherwise consumes the long head as a pure surplus (an [Add] in
   the [-1] branch, a [Retire] in the [1] branch). If [short] still has
   arms after [long] is exhausted, no alignment was found; the caller falls
   through to give-up. Returns [(matches, surplus)] where each match is
   [(long_idx, short_arm, long_arm)] in [long]'s frame and each surplus is
   [(long_idx, long_arm)]. Greedy left-to-right; suffices for the cases
   currently in the test suite, and the cost-model question of which
   alignment to prefer when several are admissible is parked in
   [paper/sketch.md] §5.4. *)
let align_by_labels short long_ =
  let rec loop short long_ i =
    match (short, long_) with
    | [], [] -> Some ([], [])
    | [], r :: rt ->
        Option.map
          (fun (matches, surplus) -> (matches, (i, r) :: surplus))
          (loop [] rt (i + 1))
    | _ :: _, [] -> None
    | l :: lt, r :: rt ->
        if labels_overlap l r then
          Option.map
            (fun (matches, surplus) -> ((i, l, r) :: matches, surplus))
            (loop lt rt (i + 1))
        else
          Option.map
            (fun (matches, surplus) -> (matches, (i, r) :: surplus))
            (loop short rt (i + 1))
  in
  loop short long_ 0

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

   Also serves as the sniffer's give-up sequence: when [analyze] can't find a
   smaller atomic edit, it emits [Replace] at the divergent slot.

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

(* The paper's [SlowRetire] idiom: wait for the subtree at the current level
   to drain naturally (no [Quiesce] gate cutting off enqueues), then
   structurally remove its arm. Useful when upstream classifiers have already
   stopped routing traffic into the subtree and we want to avoid the extra
   [Quiesce] hop. [analyze] never emits this on its own: from a policy pair
   there's no signal distinguishing "drain aggressively" from "drain lazily",
   so it stays a planner-public helper for callers that know which they want
   (e.g. the imperative-mode surface). *)
let slow_retire () = [ (Empty [], Delta.Remove []) ]

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

(* Sequence-shape introspection for the comparators' demotion paths:
   [compare_children] and [compare_metaed_children] each call into [analyze]
   recursively on children, then need to recognize "this inner sequence
   describes a relationship between children that doesn't bubble up to the
   outer slot" and demote to a slot-level give-up. The two predicates below
   are the only such recognizers. *)

(* Recognize a [PruneDownTo] sequence's tail: any sequence whose last step is
   [ChangeRoot]. Used by [compare_children]'s demotion match. *)
let ends_in_change_root seq =
  match List.rev seq with
  | (_, Delta.ChangeRoot _) :: _ -> true
  | _ -> false

(* Recognize the inner [Replace] shape (pre-bubble-up). Used by the metaed
   comparator to gate "slot-replace-with-meta" rewrites. *)
let is_replace_root = function
  | [
      (True, Delta.Designate { path = []; _ });
      (True, Delta.Quiesce [ 0 ]);
      (Empty [ 0 ], Delta.Undesignate []);
    ] -> true
  | _ -> false

(* -------- sub-policy / sniffer ------------------------------ *)

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
  (* Emit a slot-level edit at index [i] carrying [arm1] to [arm2]. A
     non-bubbleable inner shape (nested [Graft] or [PruneDownTo]) demotes
     to a slot-level [Replace] at this level; any other inner sequence
     bubbles via [prepend_seq]. *)
  let slot_arm_edit i arm1 arm2 =
    let inner = analyze arm1 arm2 in
    let non_bubbleable =
      match inner with
      | [ (True, Delta.Graft _) ] -> true
      | _ -> ends_in_change_root inner
    in
    if non_bubbleable then prepend_seq i (replace ~next:arm2 ())
    else prepend_seq i inner
  in
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      (* Per-slot walk: each index whose arm differs contributes its own
         independent edit. Distinct-index emissions concatenate freely. *)
      List.concat
        (List.mapi
           (fun i (arm1, arm2) ->
             if arm1 = arm2 then [] else slot_arm_edit i arm1 arm2)
           (List.combine ps1 ps2))
  | -1 ->
      (* Multi-arm add: every surplus entry in [ps2] is a pure addition (the
         arms of [ps1] appear in-order as a subsequence). One [Add] per extra
         in ascending index order; each subsequent path is into the structure
         as the earlier [Add]s grew it (and [insertions] already returns
         indices in that frame).

         When the strict subsequence fails ([insertions] returns [None]) but
         the divergence is "extra pure-add arms plus one or more shared arms
         that morphed structurally", fall back to label-set alignment: pair
         arms by overlapping class labels, recurse on each pair, and treat
         unmatched [ps2] arms as pure adds (paper many-arms.md). The matched-
         pair edits fire after the [Add]s have grown the structure, so their
         slot indices are in [ps2]'s frame. *)
      begin match insertions ps1 ps2 with
      | Some adds ->
          List.map
            (fun (i, arm) ->
              (True, Delta.Add { path = [ i ]; arm; meta = None }))
            adds
      | None -> (
          match align_by_labels ps1 ps2 with
          | None -> give_up
          | Some (matches, adds) ->
              let add_steps =
                List.map
                  (fun (i, arm) ->
                    (True, Delta.Add { path = [ i ]; arm; meta = None }))
                  adds
              in
              let match_steps =
                List.concat_map
                  (fun (i, arm1, arm2) ->
                    if arm1 = arm2 then [] else slot_arm_edit i arm1 arm2)
                  matches
              in
              add_steps @ match_steps)
      end
  | 1 ->
      (* Multi-arm retire: symmetric to the [-1] branch. Descending index
         order so retiring a higher slot doesn't shift the lower-index paths
         still waiting to fire (mirrors [prune_down_to]'s within-level
         discipline).

         Label-set fallback mirrors the [-1] case: when the strict
         subsequence fails, pair arms by overlapping labels and recurse on
         each pair. Retires fire first (descending [ps1]-frame indices),
         then matched-pair edits in [ps2]'s post-retire frame. *)
      begin match insertions ps2 ps1 with
      | Some retires ->
          let descending =
            List.sort (fun (a, _) (b, _) -> compare b a) retires
          in
          List.concat_map (fun (i, _) -> prepend_seq i (retire ())) descending
      | None -> (
          match align_by_labels ps2 ps1 with
          | None -> give_up
          | Some (matches, retires) ->
              let retire_indices = List.map fst retires in
              let descending =
                List.sort (fun (a, _) (b, _) -> compare b a) retires
              in
              let retire_steps =
                List.concat_map
                  (fun (i, _) -> prepend_seq i (retire ()))
                  descending
              in
              let match_steps =
                List.concat_map
                  (fun (ps1_idx, ps2_arm, ps1_arm) ->
                    if ps1_arm = ps2_arm then []
                    else
                      let ps2_idx =
                        ps1_idx
                        - List.length
                            (List.filter (fun k -> k < ps1_idx) retire_indices)
                      in
                      slot_arm_edit ps2_idx ps1_arm ps2_arm)
                  matches
              in
              retire_steps @ match_steps)
      end
  | _ -> failwith "Can't get here"

(* SP and WFQ share shape [(t * float) list]. The recognizer walks each
   parent's children independently of any lockstep gate: same-length parents
   get a per-slot walk; length-differing parents align via the arm
   projection's subsequence-embedding and compare metas at shared arms
   afterwards. Slot-level edits at distinct indices don't interfere, so they
   concatenate freely. *)
and compare_metaed_children ~next:p2 pms1 pms2 =
  let give_up = replace ~next:p2 () in
  let ps1 = List.map fst pms1 in
  let ps2 = List.map fst pms2 in
  let ms1 = List.map snd pms1 in
  let ms2 = List.map snd pms2 in
  (* Emit a slot-level edit at index [i], carrying [arm1] to [arm2] and
     optionally rebinding the slot's meta. Three sub-cases by inner shape:
     a non-bubbleable inner (nested [Graft] or [PruneDownTo]) demotes to a
     slot-level [Replace] (with meta); a [Replace]-root inner takes the
     meta in its trailing [ChangeMeta]; any other clean inner edit bubbles
     via [prepend_seq] with a separate [ChangeMeta] for the meta change. *)
  let slot_arm_edit i arm1 arm2 ?meta () =
    let inner = analyze arm1 arm2 in
    let non_bubbleable =
      match inner with
      | [ (True, Delta.Graft _) ] -> true
      | _ -> ends_in_change_root inner
    in
    if non_bubbleable then prepend_seq i (replace ~next:arm2 ?meta ())
    else
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
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      (* Per-slot walk: each index whose arm or meta differs contributes its
         own independent edit. *)
      let triples =
        List.combine (List.combine ps1 ms1) (List.combine ps2 ms2)
      in
      List.concat
        (List.mapi
           (fun i ((arm1, m1), (arm2, m2)) ->
             match (arm1 <> arm2, m1 <> m2) with
             | false, false -> []
             | false, true ->
                 [ (True, Delta.ChangeMeta { path = [ i ]; new_meta = m2 }) ]
             | true, false -> slot_arm_edit i arm1 arm2 ()
             | true, true -> slot_arm_edit i arm1 arm2 ~meta:m2 ())
           triples)
  | -1 ->
      (* [ps1] must embed in [ps2] as a subsequence. Each surplus [ps2] arm
         becomes an [Add] carrying that arm's meta; each [ps2] index whose
         arm came from [ps1] contributes a [ChangeMeta] when the metas
         differ. All [Add]s fire first, in ascending [ps2]-index order, so
         each trailing [ChangeMeta]'s path lands in [ps2]'s frame. *)
      begin match insertions ps1 ps2 with
      | None -> give_up
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
      end
  | 1 ->
      (* Symmetric to [-1]: surplus [ps1] arms retire (descending so
         lower-index paths stay stable); shared arms whose metas differ
         take a [ChangeMeta] in [ps2]'s post-Retire frame. *)
      begin match insertions ps2 ps1 with
      | None -> give_up
      | Some retires ->
          let retire_indices = List.map fst retires in
          let descending =
            List.sort (fun (a, _) (b, _) -> compare b a) retires
          in
          let retire_steps =
            List.concat_map (fun (i, _) -> prepend_seq i (retire ())) descending
          in
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
          retire_steps @ change_meta_steps
      end
  | _ -> failwith "Can't get here"

and analyze p1 p2 =
  if p1 = p2 then []
  else
    match (is_sub_policy p1 p2, is_sub_policy p2 p1) with
    | Some _, Some _ -> [] (* unreachable: would mean p1 = p2 *)
    | Some path, None -> [ (True, Delta.Graft path) ]
    | None, Some path -> prune_down_to ~prev:p1 ~path ()
    | _ -> (
        match (p1, p2) with
        | SP (pms1, _), SP (pms2, _) | WFQ pms1, WFQ pms2 ->
            compare_metaed_children ~next:p2 pms1 pms2
        | RR ps1, RR ps2 -> compare_children ~next:p2 ps1 ps2
        | _ ->
            (* FIFO->FIFO with different class, or any constructor mismatch
               (FIFO<->SP, SP<->RR, etc.): whole-slot give-up at this level.
               The leaf-level paths are empty; [prepend_seq] pins them when
               this bubbles up. *)
            replace ~next:p2 ())

(* -------- pretty-printing (test output only) ---------------- *)

let string_of_guard = function
  | True -> "True"
  | Empty p -> Printf.sprintf "Empty %s" (Delta.path_to_string p)

let string_of_step (g, d) =
  Printf.sprintf "(%s, %s)" (string_of_guard g) (Delta.to_string d)

let to_string seq = "[" ^ String.concat "; " (List.map string_of_step seq) ^ "]"

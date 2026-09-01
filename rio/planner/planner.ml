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

(* Project a node's child policies, dropping per-arm metas. [None] for
   [FIFO] (leaves have no arms); [Some arms] for the three branching
   constructors. Used by the planner's structural scans below. *)
let pol_arms = function
  | FIFO _ -> None
  | SP (prs, _) | WFQ prs -> Some (List.map fst prs)
  | RR ps -> Some ps

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
   survivor. When the swap also rebinds the slot's per-arm meta, the caller
   puts a [(True) ChangeMeta] step in front of the bubble-up, so the slot
   already carries the new metadatum when [Designate] hands it over; see
   [slot_arm_edit] for why that is the right end.

   Also serves as [analyze]'s give-up sequence: when no smaller atomic edit
   applies, [Replace] fires at the divergent slot.

   All paths are emitted as [[]] at this level; bubble-up via [prepend_seq]
   pins them to the right slot. *)
let replace ~next () =
  (* After [Designate] fires, the slot becomes a designated SP* with the
     loser at child index 0; [Quiesce] and [Undesignate] target it. *)
  let loser = [ 0 ] in
  [
    (True, Delta.Designate { path = []; survivor = next });
    (True, Delta.Quiesce loser);
    (Empty loser, Delta.Undesignate []);
  ]

(* The paper's [Retire] idiom: quiesce the subtree at the current level, then
   structurally remove its arm once it has drained. Paths are emitted as [[]]
   at this level; [prepend_seq] pins them when the sequence bubbles up. *)
let retire () = [ (True, Delta.Quiesce []); (Empty [], Delta.Remove []) ]

(* Emit retires for the [(idx, _)] slots in descending index order, so a
   higher-index retire doesn't shift the lower-index paths still pending.
   Shared by the multi-arm retire branch and the bidir emit. *)
let retires_descending entries =
  let descending = List.sort (fun (a, _) (b, _) -> compare b a) entries in
  List.concat_map (fun (i, _) -> prepend_seq i (retire ())) descending

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
  let arms_of p =
    match pol_arms p with
    | Some arms -> arms
    | None -> failwith "Planner.prune_down_to: path goes through a FIFO leaf"
  in
  let rec walk p path_remaining path_so_far =
    match path_remaining with
    | [] -> []
    | i :: rest ->
        let arms = arms_of p in
        let off_entries =
          List.mapi (fun j a -> (j, a)) arms
          |> List.filter (fun (j, _) -> j <> i)
        in
        let level_retires =
          List.fold_right prepend_seq path_so_far
            (retires_descending off_entries)
        in
        let kept = List.nth arms i in
        level_retires @ walk kept rest (path_so_far @ [ i ])
  in
  walk prev path [] @ [ (True, Delta.ChangeRoot (List.map (fun _ -> 0) path)) ]

(* Fallback combinator for [compare_children]: try bidirectional label-set
   alignment, emit it if found, otherwise give up. *)
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
    Option.bind (pol_arms p2) (fun arms ->
        List.find_mapi
          (fun i x -> Option.map (fun p -> i :: p) (is_sub_policy p1 x))
          arms)

(* Unified comparator for RR (no metas) and SP/WFQ (per-arm metas).
   Optional [ms] carries [(ms1, ms2)]; RR threads [None]. With [ms = None]
   the meta machinery degenerates to no-op: [meta_diff] always returns
   [None], adds carry [meta = None], and the metaed branches at -1/+1
   contribute no [ChangeMeta]s. Slot-level edits at distinct indices don't
   interfere, so they concatenate freely. *)
let rec compare_children ~next:p2 ?ms ps1 ps2 =
  let give_up = replace ~next:p2 () in
  let meta_for_j j = Option.map (fun (_, ms2) -> List.nth ms2 j) ms in
  let meta_diff i j =
    Option.bind ms (fun (ms1, ms2) ->
        let m1 = List.nth ms1 i in
        let m2 = List.nth ms2 j in
        if m1 = m2 then None else Some m2)
  in
  let change_meta_at j new_meta =
    (True, Delta.ChangeMeta { path = [ j ]; new_meta })
  in
  (* Slot-level edit at index [i], carrying [arm1] to [arm2] and optionally
     rebinding the slot's meta. The inner sequence bubbles via
     [prepend_seq]; a [Some m] meta contributes a separate [ChangeMeta]
     step at the same slot. [analyze_inner] never returns a [PruneDownTo]
     (top level only; see [analyze]), so the inner always bubbles cleanly.

     Where that [ChangeMeta] sits depends on what the inner sequence does
     to slot [i]. Ranks freeze at push, so a [ChangeMeta] reaches only
     packets that arrive after it fires, and firing it late leaves the
     incoming traffic permanently ranked under the outgoing metadatum.

     When the inner replaces the whole slot, [ChangeMeta] goes first:
     [Designate] routes every survivor flow to the new arm at once and
     [Quiesce] silences the old one, so from that instant everything the
     slot accepts belongs to [arm2], and the old backlog drains on ranks
     no [ChangeMeta] can rewrite.

     When the inner acts strictly below slot [i], [ChangeMeta] goes last.
     The slot keeps serving arms that are neither arriving nor departing,
     and moving the metadatum early would re-tune them for as long as the
     drain runs, which nothing bounds. *)
  let slot_arm_edit i arm1 arm2 ?meta () =
    let inner = analyze_inner arm1 arm2 in
    let base = prepend_seq i inner in
    match meta with
    | None -> base
    | Some m ->
        let step = (True, Delta.ChangeMeta { path = [ i ]; new_meta = m }) in
        (* [analyze_inner] gives up by emitting the [Replace] idiom, whose
           [Designate] carries the empty path before [prepend_seq] pins it;
           every other emission it makes acts below slot [i]. *)
        let replaces_whole_slot =
          match inner with
          | (True, Delta.Designate { path = []; _ }) :: _ -> true
          | _ -> false
        in
        if replaces_whole_slot then step :: base else base @ [ step ]
  in
  (* Edit at one matched pair, fired at parent-relative [slot]: arm
     difference -> [slot_arm_edit] (with meta when the meta also
     differs); meta-only difference -> [ChangeMeta]; both agree ->
     nothing. [i] and [j] index the ps1- and ps2-frame meta lists. *)
  let edit_pair_at slot i j arm1 arm2 =
    let m = meta_diff i j in
    match (arm1 = arm2, m) with
    | true, None -> []
    | true, Some new_meta ->
        [ (True, Delta.ChangeMeta { path = [ slot ]; new_meta }) ]
    | false, _ -> slot_arm_edit slot arm1 arm2 ?meta:m ()
  in
  (* Shared emit shape for an [align_by_labels_bidir] result. Sibling
     order carries no semantics for SP/RR/WFQ (see [Pol.normalize]), so
     we appeal to that reorder freedom: adds are appended at the
     parent's tail (slot = current child count, +1 per Add); retires
     fire at their original ps1 indices, descending so the highest-index
     drain doesn't shift the lower indices still pending; per-match
     edits last, at slot [k] = position in the matches list, which is
     also the match's slot once retires drain. Adds-first means traffic
     for the new arms starts flowing immediately rather than waiting on
     the retires' drain. *)
  let emit matches ps1_only ps2_only =
    let frame_size = List.length matches + List.length ps1_only in
    let adds =
      List.mapi
        (fun n (j, arm) ->
          ( True,
            Delta.Add { path = [ frame_size + n ]; arm; meta = meta_for_j j } ))
        ps2_only
    in
    (* A match whose arms agree and whose meta moved shares nothing with
       the retires, so it fires up front rather than waiting behind an
       unbounded drain. It is indexed in [ps1]'s frame, at the arm's
       current slot [i], since nothing has moved yet when it fires; the
       adds land past [frame_size], so they cannot collide. Matches whose
       arms differ keep their post-retire slot [k], which is where
       [slot_arm_edit] expects to be. *)
    let meta_only, arm_edits =
      List.partition
        (fun (_, (_, _, a1, a2)) -> a1 = a2)
        (List.mapi (fun k m -> (k, m)) matches)
    in
    let early_metas =
      List.filter_map
        (fun (_, (i, j, _, _)) -> Option.map (change_meta_at i) (meta_diff i j))
        meta_only
    in
    let matched =
      List.concat_map
        (fun (k, (i, j, a1, a2)) -> edit_pair_at k i j a1 a2)
        arm_edits
    in
    early_metas @ adds @ retires_descending ps1_only @ matched
  in
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      (* Permutation pre-pass (metaed only): when [ps1] and [ps2] hold the
         same arm multiset in different positions, the difference is
         purely a meta shuffle exposed by [Pol.normalize]'s sort. Emit
         one [ChangeMeta] per slot whose meta moved; leaf-partition
         validity makes the per-slot lookup unique. *)
      begin match ms with
      | Some (ms1, ms2)
        when ps1 <> ps2 && List.sort compare ps1 = List.sort compare ps2 ->
          let indexed_ps2 = List.mapi (fun j a -> (j, a)) ps2 in
          List.concat
            (List.mapi
               (fun i arm1 ->
                 let j, _ = List.find (fun (_, a) -> a = arm1) indexed_ps2 in
                 let new_meta = List.nth ms2 j in
                 if List.nth ms1 i = new_meta then []
                 else [ change_meta_at i new_meta ])
               ps1)
      | _ ->
          (* Per-slot walk pairs arms at the same index; bidir picks up
             cross-slot morphs that [Pol.normalize]'s sort exposed. Slots
             whose arms agree contribute a lone [ChangeMeta], independent
             of every other slot, so they fire before the arm edits rather
             than behind whichever drain an earlier slot happens to open.
             Arity is equal and no arm moves, so indices are stable and
             the two groups can be reordered freely. *)
          let per_slot () =
            let pairs =
              List.mapi
                (fun i (arm1, arm2) -> (i, arm1, arm2))
                (List.combine ps1 ps2)
            in
            let meta_only, arm_edits =
              List.partition (fun (_, arm1, arm2) -> arm1 = arm2) pairs
            in
            let steps group =
              List.concat_map
                (fun (i, arm1, arm2) -> edit_pair_at i i i arm1 arm2)
                group
            in
            steps meta_only @ steps arm_edits
          in
          try_bidir_cross_slot ~emit ~per_slot ps1 ps2
      end
  | -1 ->
      (* Multi-arm add: every surplus entry in [ps2] is a pure addition
         (the arms of [ps1] appear in-order as a subsequence of [ps2]).
         When metaed, shared arms whose meta differs contribute a
         trailing [ChangeMeta] in [ps2]'s frame. Nothing here blocks --
         every step is [True]-guarded -- so, unlike the retire branch,
         these need not be hoisted ahead of the adds. *)
      begin match insertions ps1 ps2 with
      | Some adds ->
          let add_indices = List.map fst adds in
          let add_steps =
            List.map
              (fun (i, arm) ->
                (True, Delta.Add { path = [ i ]; arm; meta = meta_for_j i }))
              adds
          in
          let ps2_shared =
            List.filter
              (fun j -> not (List.mem j add_indices))
              (List.init (List.length ps2) (fun j -> j))
          in
          let change_meta_steps =
            List.filter_map
              (fun (i1, j) -> Option.map (change_meta_at j) (meta_diff i1 j))
              (List.mapi (fun i1 j -> (i1, j)) ps2_shared)
          in
          add_steps @ change_meta_steps
      | None -> bidir_or ~emit ~give_up ps1 ps2
      end
  | 1 ->
      (* Symmetric to [-1]: surplus [ps1] arms retire (descending so
         lower-index paths stay stable); when metaed, shared arms whose
         metas differ take a [ChangeMeta] in [ps2]'s post-retire frame. *)
      begin match insertions ps2 ps1 with
      | Some retires ->
          let retire_indices = List.map fst retires in
          let ps1_matched =
            List.filter
              (fun i -> not (List.mem i retire_indices))
              (List.init (List.length ps1) (fun i -> i))
          in
          (* These [ChangeMeta]s land on arms no retire touches, so they
             fire before the retires rather than behind their drains. They
             are therefore indexed at the arm's pre-retire slot [i1] in
             [ps1]'s frame, not at its post-retire slot [j]. *)
          let change_meta_steps =
            List.filter_map
              (fun (j, i1) -> Option.map (change_meta_at i1) (meta_diff i1 j))
              (List.mapi (fun j i1 -> (j, i1)) ps1_matched)
          in
          change_meta_steps @ retires_descending retires
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
        let ps1 = List.map fst pms1 in
        let ps2 = List.map fst pms2 in
        let ms1 = List.map snd pms1 in
        let ms2 = List.map snd pms2 in
        compare_children ~next:p2 ~ms:(ms1, ms2) ps1 ps2
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

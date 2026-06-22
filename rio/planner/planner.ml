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

let changes prev next =
  let rec loop i prev next acc =
    match (prev, next) with
    | [], [] -> List.rev acc
    | [], _ | _, [] -> failwith "changes: input lists of different lengths"
    | x1 :: t1, x2 :: t2 ->
        if x1 <> x2 then loop (i + 1) t1 t2 ((i, x2) :: acc)
        else loop (i + 1) t1 t2 acc
  in
  loop 0 prev next []

let single_change prev next =
  match changes prev next with
  | [ (i, x) ] -> Some (i, x)
  | _ -> None

let single_change_lockstep ps1 ps2 ws1 ws2 =
  match (single_change ps1 ps2, single_change ws1 ws2) with
  | Some (i, arm), Some (j, w) when i = j -> Some (i, arm, w)
  | _ -> None

(* Lockstep insertion across both projections: succeeds when both the
   arms-projection and the metas-projection witness [ps1]/[ws1] as a
   subsequence of [ps2]/[ws2] at the same index sequence. Returns the
   per-extra triples in the ascending order [insertions] hands back. *)
let multi_insertion_lockstep ps1 ps2 ws1 ws2 =
  match (insertions ps1 ps2, insertions ws1 ws2) with
  | Some xs, Some ys
    when List.length xs = List.length ys
         && List.for_all2 (fun (i, _) (j, _) -> i = j) xs ys ->
      Some (List.map2 (fun (i, arm) (_, w) -> (i, arm, w)) xs ys)
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
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      begin match single_change ps1 ps2 with
      | Some (i, _) ->
          let child1 = List.nth ps1 i in
          let child2 = List.nth ps2 i in
          let inner =
            let raw = analyze child1 child2 in
            match raw with
            | [ (True, Delta.Graft _) ] ->
                (* A nested Graft only says "child1 embeds in child2" at the
                   inner position; it does NOT say "prev embeds in next" at
                   the outer slot. Bubbling it up via [prepend_seq] would
                   mis-describe the edit, so we demote to a slot-level
                   give-up at the current level. *)
                replace ~next:child2 ()
            | _ when ends_in_change_root raw ->
                (* Same reasoning for nested [PruneDownTo]: the inner
                   sequence says "child2 sits inside child1", which doesn't
                   bubble up to the outer slot. Demote. *)
                replace ~next:child2 ()
            | d -> d
          in
          prepend_seq i inner
      | None -> if ps1 = ps2 then [] else give_up
      end
  | -1 ->
      (* Multi-arm add: every surplus entry in [ps2] is a pure addition (the
         arms of [ps1] appear in-order as a subsequence). One [Add] per extra
         in ascending index order; each subsequent path is into the structure
         as the earlier [Add]s grew it (and [insertions] already returns
         indices in that frame). *)
      begin match insertions ps1 ps2 with
      | Some adds ->
          List.map
            (fun (i, arm) ->
              (True, Delta.Add { path = [ i ]; arm; meta = None }))
            adds
      | None -> give_up
      end
  | 1 ->
      (* Multi-arm retire: symmetric to the [-1] branch. Descending index
         order so retiring a higher slot doesn't shift the lower-index paths
         still waiting to fire (mirrors [prune_down_to]'s within-level
         discipline). *)
      begin match insertions ps2 ps1 with
      | Some retires ->
          let descending =
            List.sort (fun (a, _) (b, _) -> compare b a) retires
          in
          List.concat_map (fun (i, _) -> prepend_seq i (retire ())) descending
      | None -> give_up
      end
  | _ -> failwith "Can't get here"

(* SP and WFQ share shape [(t * float) list] and sniffer behavior: a clean
   single edit shows up at the same index in both projections. *)
and compare_metaed_children ~next:p2 pms1 pms2 =
  let give_up = replace ~next:p2 () in
  let ps1 = List.map fst pms1 in
  let ps2 = List.map fst pms2 in
  let ms1 = List.map snd pms1 in
  let ms2 = List.map snd pms2 in
  match List.compare_lengths ps1 ps2 with
  | 0 when ms1 = ms2 -> compare_children ~next:p2 ps1 ps2
  | 0 when ps1 = ps2 ->
      begin match single_change ms1 ms2 with
      | Some (i, new_meta) ->
          [ (True, Delta.ChangeMeta { path = [ i ]; new_meta }) ]
      | None -> give_up
      end
  | 0 ->
      begin match single_change_lockstep ps1 ps2 ms1 ms2 with
      | Some (i, _, meta) ->
          let inner = analyze (List.nth ps1 i) (List.nth ps2 i) in
          if is_replace_root inner then
            (* Slot [i] replaces its arm AND its meta in one edit. Re-emit
               the give-up sequence with the meta tacked on, then bubble. *)
            let survivor =
              match inner with
              | (True, Delta.Designate { survivor; _ }) :: _ -> survivor
              | _ -> assert false
            in
            prepend_seq i (replace ~next:survivor ~meta ())
          else give_up
      | None -> give_up
      end
  | -1 ->
      (* Multi-arm add with metas: lockstep insertions across both
         projections, one [Add] per extra with the matching meta attached. *)
      begin match multi_insertion_lockstep ps1 ps2 ms1 ms2 with
      | Some adds ->
          List.map
            (fun (i, arm, meta) ->
              (True, Delta.Add { path = [ i ]; arm; meta = Some meta }))
            adds
      | None -> give_up
      end
  | 1 ->
      (* Multi-arm retire with metas: symmetric to [-1], retiring highest
         index first so lower paths stay stable. Metas are discarded since
         [retire ()] removes the slot wholesale. *)
      begin match multi_insertion_lockstep ps2 ps1 ms2 ms1 with
      | Some retires ->
          let descending =
            List.sort (fun (a, _, _) (b, _, _) -> compare b a) retires
          in
          List.concat_map
            (fun (i, _, _) -> prepend_seq i (retire ()))
            descending
      | None -> give_up
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

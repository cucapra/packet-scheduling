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

let single_insertion prev next =
  match insertions prev next with
  | Some [ (i, x) ] -> Some (i, x)
  | _ -> None

let single_change prev next =
  match changes prev next with
  | [ (i, x) ] -> Some (i, x)
  | _ -> None

let single_change_lockstep ps1 ps2 ws1 ws2 =
  match (single_change ps1 ps2, single_change ws1 ws2) with
  | Some (i, arm), Some (j, w) when i = j -> Some (i, arm, w)
  | _ -> None

let single_insertion_lockstep ps1 ps2 ws1 ws2 =
  match (single_insertion ps1 ps2, single_insertion ws1 ws2) with
  | Some (i, arm), Some (j, w) when i = j -> Some (i, arm, w)
  | _ -> None

(* -------- sequence helpers ---------------------------------- *)

let prepend_guard i = function
  | True -> True
  | Empty p -> Empty (i :: p)

let prepend_step i (g, d) = (prepend_guard i g, Delta.prepend_path i d)
let prepend_seq i seq = List.map (prepend_step i) seq

(* The paper's [Replace] idiom: swap in [next] at the current level. Expands
   into [Designate(path, next) ; Quiesce(path) ; (Empty path) Undesignate(path)].
   When the swap also rebinds the slot's per-arm meta (an SP rank or WFQ weight
   changing in lockstep with the arm), a final [ChangeMeta] step fires once the
   loser has drained.

   Also serves as the sniffer's give-up sequence: when [analyze] can't find a
   smaller atomic edit, it emits [Replace] at the divergent slot.

   All paths are emitted as [[]] at this level; bubble-up via [prepend_seq]
   pins them to the right slot. *)
let replace ~next ?meta () =
  let base =
    [
      (True, Delta.Designate { path = []; arm = next });
      (True, Delta.Quiesce []);
      (Empty [], Delta.Undesignate []);
    ]
  in
  match meta with
  | None -> base
  | Some m ->
      base @ [ (Empty [], Delta.ChangeMeta { path = []; new_meta = m }) ]

(* The paper's [Retire] idiom: quiesce the subtree at the current level, then
   structurally remove its arm once it has drained. Paths are emitted as [[]]
   at this level; [prepend_seq] pins them when the sequence bubbles up. *)
let retire ~arm () =
  [ (True, Delta.Quiesce []); (Empty [], Delta.Remove { path = []; arm }) ]

(* The paper's [SlowRetire] idiom: wait for the subtree at the current level
   to drain naturally (no [Quiesce] gate cutting off enqueues), then
   structurally remove its arm. Useful when upstream classifiers have already
   stopped routing traffic into the subtree and we want to avoid the extra
   [Quiesce] hop. [analyze] never emits this on its own: from a policy pair
   there's no signal distinguishing "drain aggressively" from "drain lazily",
   so it stays a planner-public helper for callers that know which they want
   (e.g. the imperative-mode surface). *)
let slow_retire ~arm () = [ (Empty [], Delta.Remove { path = []; arm }) ]

(* Recognize the inner [Replace] shape (pre-bubble-up). Used by the metaed
   comparator to gate "slot-replace-with-meta" rewrites. *)
let is_replace_root = function
  | [
      (True, Delta.Designate { path = []; _ });
      (True, Delta.Quiesce []);
      (Empty [], Delta.Undesignate []);
    ] -> true
  | _ -> false

(* -------- sub-policy / sniffer ------------------------------ *)

let rec is_sub_policy p1 p2 =
  if p1 = p2 then Some []
  else
    match p2 with
    | FIFO _ -> None
    | SP (prs, _) | WFQ prs ->
        let rec loop i = function
          | [] -> None
          | (p, _) :: t -> (
              if p = p1 then Some [ i ]
              else
                match is_sub_policy p1 p with
                | None -> loop (i + 1) t
                | Some path -> Some (i :: path))
        in
        loop 0 prs
    | RR ps ->
        let rec loop i = function
          | [] -> None
          | p :: t -> (
              if p = p1 then Some [ i ]
              else
                match is_sub_policy p1 p with
                | None -> loop (i + 1) t
                | Some path -> Some (i :: path))
        in
        loop 0 ps

(* Same control flow as the old [Compare.compare_children], but each branch
   emits a [Planner.t] (sequence) rather than a single [Delta.t]. *)
let rec compare_children ~next:p2 ps1 ps2 =
  let give_up = replace ~next:p2 () in
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      begin match single_change ps1 ps2 with
      | Some (i, _) ->
          let child1 = List.nth ps1 i in
          let child2 = List.nth ps2 i in
          let inner =
            match analyze child1 child2 with
            | [ (True, Delta.Graft _) ] | [ (True, Delta.ChangeRoot _) ] ->
                (* A nested Graft/ChangeRoot only says "child1 embeds in
                   child2" (or vice versa) at the inner position; it does
                   NOT say "prev embeds in next" at the outer slot. Bubbling
                   it up via [prepend_seq] would mis-describe the edit, so
                   we demote to a slot-level give-up at the current level. *)
                replace ~next:child2 ()
            | d -> d
          in
          prepend_seq i inner
      | None -> if ps1 = ps2 then [] else give_up
      end
  | -1 ->
      begin match single_insertion ps1 ps2 with
      | Some (i, arm) ->
          [ (True, Delta.Add { path = [ i ]; arm; meta = None }) ]
      | None -> give_up
      end
  | 1 ->
      begin match single_insertion ps2 ps1 with
      | Some (i, arm) -> prepend_seq i (retire ~arm ())
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
            let arm =
              match inner with
              | (True, Delta.Designate { arm; _ }) :: _ -> arm
              | _ -> assert false
            in
            prepend_seq i (replace ~next:arm ~meta ())
          else give_up
      | None -> give_up
      end
  | -1 ->
      begin match single_insertion_lockstep ps1 ps2 ms1 ms2 with
      | Some (i, arm, meta) ->
          [ (True, Delta.Add { path = [ i ]; arm; meta = Some meta }) ]
      | None -> give_up
      end
  | 1 ->
      begin match single_insertion_lockstep ps2 ps1 ms2 ms1 with
      | Some (i, arm, _) -> prepend_seq i (retire ~arm ())
      | None -> give_up
      end
  | _ -> failwith "Can't get here"

and analyze p1 p2 =
  if p1 = p2 then []
  else
    match (is_sub_policy p1 p2, is_sub_policy p2 p1) with
    | Some _, Some _ -> [] (* unreachable: would mean p1 = p2 *)
    | Some path, None -> [ (True, Delta.Graft path) ]
    | None, Some path -> [ (True, Delta.ChangeRoot path) ]
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

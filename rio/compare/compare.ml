open Rio_core.Policy

type path = int list

(* A structural difference between two policies.

   Constructor names follow the paper's grammar of atomic edits [delta]
   (paper/sketch.md sec3.3) where applicable, and the paper's planner-level
   idiom names (paper/sketch.md sec4.2) where the change is bigger than a
   single [delta] production.

   - [Same] is the empty-difference reply. The paper drops [Same] from
     [delta] (the [prev = next] case is the empty sequence in sec4), but the
     sniffer still needs a name for "no difference."
   - [Replace] names the paper's [Replace(path, B)] idiom. It is emitted
     both when the sniffer detects a precise single-arm swap (e.g.,
     [SP(A,B)] vs [SP(A,C)] yields [Replace { path = [1]; arm = FIFO C }])
     and when the sniffer cannot decompose further at a level
     (multi-arm divergence, constructor mismatch), in which case the
     [arm] field carries the wholesale replacement target and [path]
     names where it lands. The IR-side lowering goes through [Designate]
     in both cases. Once the planner gains a sequence representation
     (worklist PR 10), [Replace] will be expanded into the underlying
     [Designate ; Quiesce ; Undesignate] sequence.

   The [weight] field on [Add] and [Replace] is [Some w] iff the parent
   runs WFQ; for SP, RR, and UNION it is [None]. *)
type t =
  | Same
  | Add of {
      path : path;
      arm : Rio_core.Policy.t;
      weight : float option;
    }
  | Remove of {
      path : path;
      arm : Rio_core.Policy.t;
    }
  | ChangeWeight of {
      path : path;
      new_weight : float;
    }
  | Replace of {
      path : path;
      arm : Rio_core.Policy.t;
      weight : float option;
    }
  | Graft of path
      (** [prev] sits as a strict subtree of [next] at [path]:
          [next = ctx[prev]] where [ctx]'s hole is at [path]. *)
  | ChangeRoot of path
      (** [next] is a strict subtree of [prev] at [path]: pruning [prev] down to
          that subtree gives [next]. *)

(** [insertions prev next] determines whether [next] can be obtained by
    inserting elements into [prev] without reordering existing elements. If so,
    it returns [Some ins], where [ins] is a list of pairs of the form [(i, x)].
    Each pair indicates that element [x] appears in [next] at index [i] and was
    not present at that position in [prev] (i.e., it was inserted). If [next]
    cannot be formed by inserting elements into [prev] (for example, if elements
    would need to be removed or reordered), we return [None]. *)
let insertions prev next =
  let rec loop i prev next acc =
    match (prev, next) with
    | [], [] -> Some (List.rev acc)
    | [], x :: t ->
        (* elements in [next] are all insertions *)
        loop (i + 1) [] t ((i, x) :: acc)
    | x1 :: t1, x2 :: t2 ->
        if x1 = x2 then loop (i + 1) t1 t2 acc
        else
          (* [x2] was inserted at position [i] *)
          loop (i + 1) prev t2 ((i, x2) :: acc)
    | _ :: _, [] -> None (* ran out of [next] *)
  in
  loop 0 prev next []

(** [changes prev next] returns a list of all positions at which the two input
    lists differ. Returns a list of pairs, in which pair [(i, x)] appears iff
    [prev[i] <> next[i] = x] *)
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

(* [Some (i, x)] iff [next] is [prev] with [x] inserted at index [i]; else
   [None]. *)
let single_insertion prev next =
  match insertions prev next with
  | Some [ (i, x) ] -> Some (i, x)
  | _ -> None

(* [Some (i, x)] iff [prev] and [next] have equal length and differ at exactly
   one slot [i], where [next.(i) = x]. *)
let single_change prev next =
  match changes prev next with
  | [ (i, x) ] -> Some (i, x)
  | _ -> None

(* Lockstep versions used by WFQ: a clean single edit must show up at the
   same index in both the policy list and the weight list. *)
let single_change_lockstep ps1 ps2 ws1 ws2 =
  match (single_change ps1 ps2, single_change ws1 ws2) with
  | Some (i, arm), Some (j, w) when i = j -> Some (i, arm, w)
  | _ -> None

let single_insertion_lockstep ps1 ps2 ws1 ws2 =
  match (single_insertion ps1 ps2, single_insertion ws1 ws2) with
  | Some (i, arm), Some (j, w) when i = j -> Some (i, arm, w)
  | _ -> None

(* Prepend [i] to every embedded [path] inside [diff]. Used by [compare]
   when descending: a child's diff comes back parent-relative, and we tack
   on the child's own index to make it grandparent-relative (and so on up
   the recursion). *)
let prepend_path i diff =
  match diff with
  | Same -> Same
  | Add { path; arm; weight } -> Add { path = i :: path; arm; weight }
  | Remove { path; arm } -> Remove { path = i :: path; arm }
  | ChangeWeight { path; new_weight } ->
      ChangeWeight { path = i :: path; new_weight }
  | Replace { path; arm; weight } -> Replace { path = i :: path; arm; weight }
  | Graft p -> Graft (i :: p)
  | ChangeRoot p -> ChangeRoot (i :: p)

let rec is_sub_policy p1 p2 =
  (* Is p1 a sub-policy of p2?
     Returns [Some path], where [path] tells us where in p2 we found p1.
     Returns [Some []] when p1 = p2.
     Returns None if not found. *)
  if p1 = p2 then Some []
  else
    match p2 with
    | FIFO _ -> None
    | UNION ps | SP ps | RR ps | WFQ (ps, _) ->
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

(* We arrive here when two parents have the same policy, so we now need to
   compare their children against each other.
   [Add] only fires when exactly one arm was inserted;
   [Remove] only when exactly one was dropped;
   a single in-place divergence recurses into the differing children
   (this is how deep diffs get recognized: the inner [analyze]
   returns a parent-relative diff and we tack on [i] using [prepend_path]).
   When the sniffer can't break the change down at this level (multi-arm
   divergence, mismatched insertions, etc.), we "give up" by emitting
   [Replace { path = []; arm = p2; weight = None }]. *)
and compare_children ~next:p2 ps1 ps2 =
  let give_up = Replace { path = []; arm = p2; weight = None } in
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      (* Same length. Exactly one slot differing means we recurse on it
         (the inner diff might be deep — an [Add]/[Remove] whose path
         bubbles up via [prepend_path], or a leaf [Replace { path = [] }]
         which [prepend_path] turns into [{ path = [i] }]). Anything else
         is a multi-arm give-up. *)
      begin match single_change ps1 ps2 with
      | Some (i, _) ->
          let child1 = List.nth ps1 i in
          let child2 = List.nth ps2 i in
          let inner =
            match analyze child1 child2 with
            | Graft _ | ChangeRoot _ ->
                (* A nested [Graft]/[ChangeRoot] only says "child1 embeds in
                   child2" (or vice versa) — it does NOT say "prev embeds
                   in next" at the outer position, so bubbling it up with
                   [prepend_path] would mis-describe the edit. Demote to a
                   wholesale slot replacement. *)
                Replace { path = []; arm = child2; weight = None }
            | d -> d
          in
          prepend_path i inner
      | None -> if ps1 = ps2 then Same else give_up
      end
  | -1 ->
      (* ps1 shorter; an insertion into ps1 could make ps2. *)
      begin match single_insertion ps1 ps2 with
      | Some (i, arm) -> Add { path = [ i ]; arm; weight = None }
      | None -> give_up
      end
  | 1 ->
      (* ps2 shorter; equivalently, an arm was removed from ps1. *)
      begin match single_insertion ps2 ps1 with
      | Some (i, arm) -> Remove { path = [ i ]; arm }
      | None -> give_up
      end
  | _ -> failwith "Can't get here"

(* When the parents are WFQ, comparing their children is a little more
   complicated: a single clean edit must show up at the same index in
   both the policy list and the weight list. *)
and compare_wfq_children ~next:p2 ps1 (ws1 : float list) ps2 (ws2 : float list)
    =
  let give_up = Replace { path = []; arm = p2; weight = None } in
  match List.compare_lengths ps1 ps2 with
  | 0 when ws1 = ws2 ->
      (* Pure policy edit in-place; weights unchanged. Defer to the
         non-WFQ comparator. *)
      compare_children ~next:p2 ps1 ps2
  | 0 when ps1 = ps2 ->
      (* Pure weight edit in-place. Same slot counts ⇒ [changes] on the
         weights is well-defined, and the empty result is unreachable
         because [ws1 <> ws2] here. *)
      begin match single_change ws1 ws2 with
      | Some (i, new_weight) -> ChangeWeight { path = [ i ]; new_weight }
      | None -> give_up
      end
  | 0 ->
      (* Same length but both lists differ. The only single edit we could
         describe is a WFQ arm-replace: one slot must be the lone
         in-place difference in both [ps] and [ws], and the slot's
         arm-vs-arm diff must itself be a leaf-level give-up
         (otherwise we'd be folding a deep arm change with a weight
         change into one variant, which [Ir.patch] can't express). *)
      begin match single_change_lockstep ps1 ps2 ws1 ws2 with
      | Some (i, _, weight) ->
          begin match analyze (List.nth ps1 i) (List.nth ps2 i) with
          | Replace { path = []; arm; weight = None } ->
              Replace { path = [ i ]; arm; weight = Some weight }
          | _ -> give_up
          end
      | None -> give_up
      end
  | -1 ->
      (* ps1 shorter: a slot was added to make ps2, with its weight. *)
      begin match single_insertion_lockstep ps1 ps2 ws1 ws2 with
      | Some (i, arm, weight) -> Add { path = [ i ]; arm; weight = Some weight }
      | None -> give_up
      end
  | 1 ->
      (* ps2 shorter: a slot was removed from ps1. The dropped weight
         isn't needed to describe the removal, so we use [Remove]. *)
      begin match single_insertion_lockstep ps2 ps1 ws2 ws1 with
      | Some (i, arm, _) -> Remove { path = [ i ]; arm }
      | None -> give_up
      end
  | _ -> failwith "Can't get here"

and analyze p1 p2 =
  if p1 = p2 then Same
  else
    match (is_sub_policy p1 p2, is_sub_policy p2 p1) with
    | Some _, Some _ -> Same (* Can't get here *)
    | Some path, None -> Graft path
    | None, Some path -> ChangeRoot path
    | _ -> (
        match (p1, p2) with
        | UNION ps1, UNION ps2 | SP ps1, SP ps2 | RR ps1, RR ps2 ->
            compare_children ~next:p2 ps1 ps2
        | WFQ (ps1, ws1), WFQ (ps2, ws2) ->
            compare_wfq_children ~next:p2 ps1 ws1 ps2 ws2
        | _ ->
            (* FIFO->FIFO with a different class, or any constructor mismatch
               (FIFO<->SP, SP<->RR, etc.) — wholesale replacement at this
               position. The leaf-level diff is path-empty; [compare_children]'s
               [prepend_path] tags on the child index when this bubbles up,
               but only if it's the sole divergence at that level. *)
            Replace { path = []; arm = p2; weight = None })

(* Pretty-printers — only used to format failure messages from the test
   suite; the patcher never goes through these. *)

let path_to_string = function
  | [] -> "(root)"
  | [ i ] -> Printf.sprintf "(child %d)" i
  | p ->
      let s = String.concat "->" (List.map string_of_int p) in
      Printf.sprintf "(path %s)" s

let string_of_arm path arm =
  Printf.sprintf "%s at %s"
    (Rio_core.Policy.to_string arm)
    (path_to_string path)

let string_of_arm_w path arm weight =
  Printf.sprintf "%s (weight %g) at %s"
    (Rio_core.Policy.to_string arm)
    weight (path_to_string path)

let to_string = function
  | Same -> "Same"
  | Add { path; arm; weight = None } ->
      Printf.sprintf "Add: %s" (string_of_arm path arm)
  | Add { path; arm; weight = Some w } ->
      Printf.sprintf "Add: %s" (string_of_arm_w path arm w)
  | Remove { path; arm } -> Printf.sprintf "Remove: %s" (string_of_arm path arm)
  | ChangeWeight { path; new_weight } ->
      Printf.sprintf "ChangeWeight: %s -> %g" (path_to_string path) new_weight
  | Replace { path; arm; weight = None } ->
      Printf.sprintf "Replace: %s" (string_of_arm path arm)
  | Replace { path; arm; weight = Some w } ->
      Printf.sprintf "Replace: %s" (string_of_arm_w path arm w)
  | Graft p -> Printf.sprintf "Graft at %s" (path_to_string p)
  | ChangeRoot p -> Printf.sprintf "ChangeRoot at %s" (path_to_string p)

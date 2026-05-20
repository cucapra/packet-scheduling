open Rio_core.Policy

type path = int list

(* A structural diff between two policies. *)
type t =
  | Same
  | OneArmAdded of arm_diff
  | OneArmAddedWFQ of arm_diff_w
    (* WFQ-specific arm addition. Adding a slot to a WFQ parent always
       carries a weight, so the new arm and its weight are a single edit. *)
  | OneArmRemoved of arm_diff
  | WeightChanged of weight_change
  | OneArmReplaced of arm_diff
    (* Wholesale replacement of the subtree at [path] with [arm]. If [path] is nil, that means we're not (yet) clever enough to specify the change and the arm being replaced is the whole tree. *)
  | OneArmReplacedWFQ of arm_diff_w
    (* WFQ-specific arm replacement: a single WFQ slot's arm and weight
       both changed. Bundles the new arm and the new weight together so
       it doesn't decompose into [OneArmReplaced] + [WeightChanged]. *)
  | SuperPol of path
    (* [SuperPol]/[SubPol] are only emitted at the top level of [analyze]:
       [SuperPol path] means [path] points to [prev] inside [next];
       [SubPol path] means [path] points to [next] inside [prev]. When
       nested comparisons would otherwise bubble one of these up through
       [compare_children], we demote to [OneArmReplaced] at the differing
       slot — the inner sub-policy path would no longer describe "prev
       sits inside next" (or vice versa), just "child1 sits inside
       child2", which is not actionable from [Ir.patch]. *)
  | SubPol of path

and arm_diff = {
  path : path;
      (* Path from the root to the position of this arm.
      For [OneArmAdded] this is a position in *next*; for
          [OneArmRemoved] it is a position in *prev*. *)
  arm : Rio_core.Policy.t;
}

and arm_diff_w = {
  path : path;
  arm : Rio_core.Policy.t;
  weight : float;
}

and weight_change = {
  path : path;
  new_weight : float;
}

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
  let prepend_arm (ad : arm_diff) = { ad with path = i :: ad.path } in
  let prepend_arm_w (ad : arm_diff_w) = { ad with path = i :: ad.path } in
  let prepend_wc (wc : weight_change) = { wc with path = i :: wc.path } in
  match diff with
  | Same -> Same
  | OneArmAdded ad -> OneArmAdded (prepend_arm ad)
  | OneArmAddedWFQ ad -> OneArmAddedWFQ (prepend_arm_w ad)
  | OneArmRemoved ad -> OneArmRemoved (prepend_arm ad)
  | WeightChanged wc -> WeightChanged (prepend_wc wc)
  | OneArmReplaced ad -> OneArmReplaced (prepend_arm ad)
  | OneArmReplacedWFQ ad -> OneArmReplacedWFQ (prepend_arm_w ad)
  | SuperPol p -> SuperPol (i :: p)
  | SubPol p -> SubPol (i :: p)

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
   [OneArmAdded] only fires when exactly one arm was inserted; 
   [OneArmRemoved] only when exactly one was dropped; 
   a single in-place divergence recurses into the differing children 
   (this is how deep diffs get recognized: the inner [analyze]
   returns a parent-relative diff and we tack on [i] using [prepend_path]).
   When the sniffer can't break the change down at this level (multi-arm
   divergence, mismatched insertions, etc.), we "give up" by emitting
   [OneArmReplaced { path = []; arm = p2 }]. *)
and compare_children ~next:p2 ps1 ps2 =
  let give_up = OneArmReplaced { path = []; arm = p2 } in
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      (* Same length. Exactly one slot differing means we recurse on it
         (the inner diff might be deep — an [OneArmAdded]/[OneArmRemoved]
         whose path bubbles up via [prepend_path], or a leaf
         [OneArmReplaced { path = [] }] which [prepend_path] turns into
         [{ path = [i] }]). Anything else is a multi-arm give-up. *)
      begin match single_change ps1 ps2 with
      | Some (i, _) ->
          let child1 = List.nth ps1 i in
          let child2 = List.nth ps2 i in
          let inner =
            match analyze child1 child2 with
            | SuperPol _ | SubPol _ ->
                (* A nested [SuperPol]/[SubPol] only says "child1 embeds in
                   child2" (or vice versa) — it does NOT say "prev embeds
                   in next" at the outer position, so bubbling it up with
                   [prepend_path] would mis-describe the edit. Demote to a
                   wholesale slot replacement. *)
                OneArmReplaced { path = []; arm = child2 }
            | d -> d
          in
          prepend_path i inner
      | None -> if ps1 = ps2 then Same else give_up
      end
  | -1 ->
      (* ps1 shorter; an insertion into ps1 could make ps2. *)
      begin match single_insertion ps1 ps2 with
      | Some (i, arm) -> OneArmAdded { path = [ i ]; arm }
      | None -> give_up
      end
  | 1 ->
      (* ps2 shorter; equivalently, an arm was removed from ps1. *)
      begin match single_insertion ps2 ps1 with
      | Some (i, arm) -> OneArmRemoved { path = [ i ]; arm }
      | None -> give_up
      end
  | _ -> failwith "Can't get here"

(* When the parents are WFQ, comparing their children is a little more
   complicated: a single clean edit must show up at the same index in
   both the policy list and the weight list. *)
and compare_wfq_children ~next:p2 ps1 (ws1 : float list) ps2 (ws2 : float list)
    =
  let give_up = OneArmReplaced { path = []; arm = p2 } in
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
      | Some (i, new_weight) -> WeightChanged { path = [ i ]; new_weight }
      | None -> give_up
      end
  | 0 ->
      (* Same length but both lists differ. The only single edit we could
         describe is a WFQ arm-replace: one slot must be the lone
         in-place difference in both [ps] and [ws], and the slot's
         arm-vs-arm diff must itself be a leaf-level replacement
         (otherwise we'd be folding a deep arm change with a weight
         change into one variant, which [Ir.patch] can't express). *)
      begin match single_change_lockstep ps1 ps2 ws1 ws2 with
      | Some (i, _, weight) ->
          begin match analyze (List.nth ps1 i) (List.nth ps2 i) with
          | OneArmReplaced { path = []; arm } ->
              OneArmReplacedWFQ { path = [ i ]; arm; weight }
          | _ -> give_up
          end
      | None -> give_up
      end
  | -1 ->
      (* ps1 shorter: a slot was added to make ps2, with its weight. *)
      begin match single_insertion_lockstep ps1 ps2 ws1 ws2 with
      | Some (i, arm, weight) -> OneArmAddedWFQ { path = [ i ]; arm; weight }
      | None -> give_up
      end
  | 1 ->
      (* ps2 shorter: a slot was removed from ps1. The dropped weight
         isn't needed to describe the removal, so we use [OneArmRemoved]. *)
      begin match single_insertion_lockstep ps2 ps1 ws2 ws1 with
      | Some (i, arm, _) -> OneArmRemoved { path = [ i ]; arm }
      | None -> give_up
      end
  | _ -> failwith "Can't get here"

and analyze p1 p2 =
  if p1 = p2 then Same
  else
    match (is_sub_policy p1 p2, is_sub_policy p2 p1) with
    | Some _, Some _ -> Same (* Can't get here *)
    | Some path, None -> SuperPol path
    | None, Some path -> SubPol path
    | _ -> (
        match (p1, p2) with
        | UNION ps1, UNION ps2 | SP ps1, SP ps2 | RR ps1, RR ps2 ->
            compare_children ~next:p2 ps1 ps2
        | WFQ (ps1, ws1), WFQ (ps2, ws2) ->
            compare_wfq_children ~next:p2 ps1 ws1 ps2 ws2
        | _ ->
            (* FIFO→FIFO with a different class, or any constructor mismatch
               (FIFO↔SP, SP↔RR, etc.) — wholesale replacement at this
               position. The leaf-level diff is path-empty; [compare_lists]'s
               [prepend_path] tags on the child index when this bubbles up,
               but only if it's the sole divergence at that level. *)
            OneArmReplaced { path = []; arm = p2 })

(* Pretty-printers — only used to format failure messages from the test
   suite; the patcher never goes through these. *)

let path_to_string = function
  | [] -> "(root)"
  | [ i ] -> Printf.sprintf "(child %d)" i
  | p ->
      let s = String.concat "->" (List.map string_of_int p) in
      Printf.sprintf "(path %s)" s

let string_of_arm_diff { path; arm } =
  Printf.sprintf "%s at %s"
    (Rio_core.Policy.to_string arm)
    (path_to_string path)

let string_of_arm_diff_w { path; arm; weight } =
  Printf.sprintf "%s (weight %g) at %s"
    (Rio_core.Policy.to_string arm)
    weight (path_to_string path)

let string_of_weight_change { path; new_weight } =
  Printf.sprintf "%s → %g" (path_to_string path) new_weight

let to_string = function
  | Same -> "Same"
  | OneArmAdded ad -> Printf.sprintf "OneArmAdded: %s" (string_of_arm_diff ad)
  | OneArmAddedWFQ ad ->
      Printf.sprintf "OneArmAddedWFQ: %s" (string_of_arm_diff_w ad)
  | OneArmRemoved ad ->
      Printf.sprintf "OneArmRemoved: %s" (string_of_arm_diff ad)
  | WeightChanged wc ->
      Printf.sprintf "WeightChanged: %s" (string_of_weight_change wc)
  | OneArmReplaced ad ->
      Printf.sprintf "OneArmReplaced: %s" (string_of_arm_diff ad)
  | OneArmReplacedWFQ ad ->
      Printf.sprintf "OneArmReplacedWFQ: %s" (string_of_arm_diff_w ad)
  | SuperPol p -> Printf.sprintf "SuperPol at %s" (path_to_string p)
  | SubPol p -> Printf.sprintf "SubPol at %s" (path_to_string p)

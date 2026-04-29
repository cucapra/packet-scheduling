open Frontend.Policy

type path = int list

(* A structural diff between two policies. *)
type t =
  | Same
  | OneArmAdded of arm_diff
  | OneArmRemoved of arm_diff
  | WeightChanged of weight_change
  | OneArmReplaced of arm_diff
  | VeryDifferent of path
  | SuperPol of path (* [path] points to [prev] inside [next]. *)
  | SubPol of path (* [path] points to [next] inside [prev]. *)

and arm_diff = {
  path : path;
      (* Path from the root to the position of this arm. 
      For [OneArmAdded] this is a position in *next*; for
          [OneArmRemoved] it is a position in *prev*. *)
  arm : Frontend.Policy.t;
}

and weight_change = {
  path : path;
  new_weight : float;
}

(** [inserted prev next] determines whether [next] can be obtained by inserting
    elements into [prev] without reordering existing elements. If so, it returns
    [Some ins], where [ins] is a list of pairs of the form [(i, x)]. Each pair
    indicates that element [x] appears in [next] at index [i] and was not
    present at that position in [prev] (i.e., it was inserted). If [next] cannot
    be formed by inserting elements into [prev] (for example, if elements would
    need to be removed or reordered), we return [None]. *)
let inserted prev next =
  let rec loop i prev next acc =
    match (prev, next) with
    | [], [] -> Some (List.rev acc)
    | [], x :: t ->
        (* elements in [next] are all insertions *)
        loop (i + 1) [] t ((i, x) :: acc)
    | x1 :: t1, x2 :: t2 ->
        if x1 = x2 then loop (i + 1) t1 t2 acc
        else
          (* x2 was inserted at position i *)
          loop (i + 1) prev t2 ((i, x2) :: acc)
    | _ :: _, [] -> None (* ran out of [next] *)
  in
  loop 0 prev next []

(** [changed prev next] returns a list of all positions at which the two input
    lists differ. Returns a list of pairs, in which pair [(i, x)] appears iff
    [prev[i] <> next[i] = x] *)
let changed prev next =
  let rec loop i prev next acc =
    match (prev, next) with
    | [], [] -> List.rev acc
    | [], _ | _, [] -> failwith "changed: input lists of different lengths"
    | x1 :: t1, x2 :: t2 ->
        if x1 <> x2 then loop (i + 1) t1 t2 ((i, x2) :: acc)
        else loop (i + 1) t1 t2 acc
  in
  loop 0 prev next []

(* Prepend [i] to every embedded [path] inside [diff]. Used by [compare]
   when descending: a child's diff comes back parent-relative, and we tack
   on the child's own index to make it grandparent-relative (and so on up
   the recursion). *)
let prepend_path i diff =
  let prepend_arm (ad : arm_diff) = { ad with path = i :: ad.path } in
  let prepend_wc (wc : weight_change) = { wc with path = i :: wc.path } in
  match diff with
  | Same -> Same
  | OneArmAdded ad -> OneArmAdded (prepend_arm ad)
  | OneArmRemoved ad -> OneArmRemoved (prepend_arm ad)
  | WeightChanged wc -> WeightChanged (prepend_wc wc)
  | OneArmReplaced ad -> OneArmReplaced (prepend_arm ad)
  | VeryDifferent p -> VeryDifferent (i :: p)
  | SuperPol p -> SuperPol (i :: p)
  | SubPol p -> SubPol (i :: p)

let rec is_sub_policy p1 p2 =
  (* Is p1 a sub-policy of p2?
     Returns [Some path], where [path] tells us where in p2 we found p1.
     When found at an *immediate* child i, path = [i].
     When found deeper, path = i :: deeper_path.
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

(* SP/RR/UNION share a flat list-of-children shape, so they share the same
   diff strategy. [OneArmAdded] only fires when exactly one arm was
   inserted; [OneArmRemoved] only when exactly one was dropped; a single
   in-place divergence recurses into the differing children (which is
   how deep diffs get recognized: the inner [analyze] returns a 
   parent-relative diff and we tack on [i] via [prepend_path]).
   Multi-arm changes in any direction degrade to [VeryDifferent]. *)
and compare ps1 ps2 =
  (* Lengths differ by some number of insertions in one direction. We can
     only describe the case of a single insertion: [inserted prev next]
     returning exactly one diff means [next] is [prev] with one element
     added at index [i]; package it with [ctor] (either [OneArmAdded] or
     [OneArmRemoved] depending on which direction we were checking). *)
  let single_insert prev next ctor =
    match inserted prev next with
    | Some [ (i, arm) ] -> ctor { path = [ i ]; arm }
    | _ -> VeryDifferent []
  in
  let n = List.compare_lengths ps1 ps2 in
  if n = 0 then begin
    (* lists of same length *)
    match changed ps1 ps2 with
    | [] -> Same
    | [ (i, _) ] ->
        (* Exactly one slot differs. We recurse, since diff might be
           a deep [OneArmAdded]/[OneArmRemoved] (path bubbles up through
           [prepend_path]) or a leaf [OneArmReplaced { path = [] }] which
           prepend_path turns into [{ path = [i] }]. A big divergence
           below us comes back as [VeryDifferent] and prepend_path
           tags it with [i]. *)
        prepend_path i (analyze (List.nth ps1 i) (List.nth ps2 i))
    | _ ->
        (* Detected that more than one arm was changed in-place. We can't handle that yet, but eventually we'll set up a map over all such changes and just generate a list of OneArmReplaced. *)
        VeryDifferent []
  end
  else if n < 0 then
    (* ps1 was shorter; an insertion into ps1 could make ps2. *)
    single_insert ps1 ps2 (fun ad -> OneArmAdded ad)
  else
    (* ps2 was shorter; equivalently, we can remove an arm from ps1 to make ps2. *)
    single_insert ps2 ps1 (fun ad -> OneArmRemoved ad)

and compare_wfq ps1 (ws1 : float list) ps2 (ws2 : float list) =
  match (ps1 = ps2, ws1 = ws2) with
  | true, true -> Same (* Shoudn't get here; this is [Same] at a higher level *)
  | false, true ->
      (* We suspect it's a pure policy change in-place, with weights left unchanged. We can just pass this to [compare]. But first let's check if their lengths are the same *)
      if List.length ps1 = List.length ps2 then compare ps1 ps2
      else VeryDifferent []
  | true, false -> begin
      (* Pure weight change in-place. [ps1 = ps2] guarantees the slot
         counts match, so [ws1] and [ws2] are necessarily the same length
         and [changed] is well-defined. The [[]] result is unreachable
         because [ws1 <> ws2] in this branch. *)
      match changed ws1 ws2 with
      | [ (i, new_weight) ] -> WeightChanged { path = [ i ]; new_weight }
      | _ ->
          (* Detected more than one point of difference. We can't handle that yet. *)
          VeryDifferent []
    end
  | false, false -> begin
      (* Both lists changed. The only single-edit story we can tell here
         is an arm _removal_: dropping one (arm, weight) pair changes both
         [ps] and [ws]. We require the same drop position in both — that
         confirms we're looking at one consistent slot removal rather
         than two coincidental edits. WFQ-add isn't expressible (the new
         slot's weight has nowhere to ride on [arm_diff]), so we don't
         look the other direction. *)
      match (inserted ps2 ps1, inserted ws2 ws1) with
      | Some [ (i, arm) ], Some [ (j, _) ] when i = j ->
          OneArmRemoved { path = [ i ]; arm }
      | _ -> VeryDifferent []
    end

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
            compare ps1 ps2
        | WFQ (ps1, ws1), WFQ (ps2, ws2) -> compare_wfq ps1 ws1 ps2 ws2
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
    (Frontend.Policy.to_string arm)
    (path_to_string path)

let string_of_weight_change { path; new_weight } =
  Printf.sprintf "%s → %g" (path_to_string path) new_weight

let to_string = function
  | Same -> "Same"
  | OneArmAdded ad -> Printf.sprintf "OneArmAdded: %s" (string_of_arm_diff ad)
  | OneArmRemoved ad ->
      Printf.sprintf "OneArmRemoved: %s" (string_of_arm_diff ad)
  | WeightChanged wc ->
      Printf.sprintf "WeightChanged: %s" (string_of_weight_change wc)
  | OneArmReplaced ad ->
      Printf.sprintf "OneArmReplaced: %s" (string_of_arm_diff ad)
  | VeryDifferent p -> Printf.sprintf "VeryDifferent at %s" (path_to_string p)
  | SuperPol p -> Printf.sprintf "SuperPol at %s" (path_to_string p)
  | SubPol p -> Printf.sprintf "SubPol at %s" (path_to_string p)

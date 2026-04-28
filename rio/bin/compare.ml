open Frontend.Policy

type path = int list

(* A structural diff between two policies. Variants that describe local
   changes carry their own [path] inline. *)
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
  arm : Frontend.Policy.t; (* The arm itself. *)
}

and weight_change = {
  path : path;
  new_weight : float;
}

(** [insertions prev next] determines whether [next] can be obtained by
    inserting elements into [prev] without reordering existing elements. If so,
    it returns [Some ins], where [ins] is a list of pairs [(i, x)]. Each pair
    indicates that element [x] appears in [next] at index [i] and was not
    present at that position in [prev] (i.e., it was inserted). If [next] cannot
    be formed by inserting elements into [prev] (for example, if elements would
    need to be removed or reordered), return [None]. *)
let insertions prev next =
  let rec loop i prev next acc =
    match (prev, next) with
    | [], [] -> Some (List.rev acc)
    | [], x :: t ->
        (* remaining elements in [next] are all insertions *)
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

(* Prepend [i] to every embedded [path] inside [diff]. Used by
   [compare_lists] when descending: a child's diff comes back parent-relative,
   and we tack on the child's own index to make it grandparent-relative
   (and so on up the recursion). *)
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
     Returns (found, path).
     path tells us where in p2 we found p1, if anywhere.
     When found at an *immediate* child i, path = [i].
     When found deeper, path = i :: deeper_path.
     Returns (true, []) when p1 = p2.
     Returns (false, []) if not found. *)
  if p1 = p2 then (true, [])
  else
    match p2 with
    | FIFO _ -> (false, [])
    | UNION ps | SP ps | RR ps | WFQ (ps, _) ->
        let rec loop i = function
          | [] -> (false, [])
          | p :: t ->
              if p = p1 then (true, [ i ])
              else
                let found, path = is_sub_policy p1 p in
                if found then (true, i :: path) else loop (i + 1) t
        in
        loop 0 ps

(* SP/RR/UNION share a flat list-of-children shape, so they share the same
   diff strategy: precise [one_arm_added] (the patcher's main trick — works
   for inserts at any position, not just the end), then a subsequence-based
   [OneArmRemoved], then structural [compare_lists]. The two callers (SP
   and RR/UNION) differ only in how they compute the removed-arm indices,
   so we parameterize over [removed_fn]. [OneArmAdded] only fires when
   exactly one arm was inserted; [OneArmRemoved] only when exactly one was
   dropped. Multi-arm changes in either direction degrade to
   [VeryDifferent]. *)
and compare ps1 ps2 =
  match List.compare_lengths ps1 ps2 with
  | 0 ->
      (* lists of same length *)
      begin match changed ps1 ps2 with
      | [] -> Same
      | [ (i, c) ] -> OneArmReplaced { path = [ i ]; arm = c }
      | _ ->
          (* [changed] has detected more than one point of difference. We can't handle that yet. *)
          VeryDifferent []
      end
  | -1 -> begin
      (* ps1 was shorter, so we check for insertions into ps1 *)
      match insertions ps1 ps2 with
      | None ->
          (* [insertions] is unable to create ps2 by inserting elements to ps1. Give up *)
          VeryDifferent []
      | Some [ (i, c) ] ->
          (* One insertion into ps1 does the trick *)
          OneArmAdded { path = [ i ]; arm = c }
      | Some _ ->
          (* More than one insertion. We can't handle that yet. *)
          VeryDifferent []
    end
  | 1 ->
      (* ps2 was shorter, so we check for deletions in ps1 *)
      begin match insertions ps2 ps1 with
      | None ->
          (* [insertions] is unable to create ps1 by inserting elements to ps2. Give up *)
          VeryDifferent []
      | Some [ (i, c) ] ->
          (* One insertion into ps2 creates ps1. So we can equivalently remove that arm from ps1 to create ps2 *)
          OneArmRemoved { path = [ i ]; arm = c }
      | Some _ ->
          (* More than one point of deletion. We can't handle that yet. *)
          VeryDifferent []
      end
  | _ -> failwith "Can't get here"

and compare_wfq ps1 (ws1 : float list) ps2 (ws2 : float list) =
  match (ps1 = ps2, ws1 = ws2) with
  | true, true -> Same
  | false, true ->
      (* we think it's a policy change in-place *)
      if List.length ps1 = List.length ps2 then compare ps1 ps2
      else VeryDifferent []
  | true, false -> begin
      (* we think it's a weight change *)
      match changed ws1 ws2 with
      | [] -> Same
      | [ (i, new_weight) ] -> WeightChanged { path = [ i ]; new_weight }
      | _ ->
          (* [changed] has detected more than one point of difference. We can't handle that yet. *)
          VeryDifferent []
    end
  | false, false -> (* give up for now *) VeryDifferent []

and analyze p1 p2 =
  if p1 = p2 then Same
  else
    let super_found, super_idx = is_sub_policy p1 p2 in
    if super_found then SuperPol super_idx
    else
      let sub_found, sub_idx = is_sub_policy p2 p1 in
      if sub_found then SubPol sub_idx
      else
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
            OneArmReplaced { path = []; arm = p2 }

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

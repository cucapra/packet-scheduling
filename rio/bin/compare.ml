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
  arm : Frontend.Policy.t;
  weight : float option;
      (* [Some w] for arms that carry an explicit weight (i.e. WFQ). *)
}

and weight_change = {
  path : path;
  new_weight : float;
}

(* Check if lst1 appears as an order-preserving sub-sequence of lst2.
   E.g. [A,C] is an order-preserving sub-sequence of [A,B,C,D], but
   [C,A] is not. *)
let rec is_ordered_subsequence lst1 lst2 =
  match (lst1, lst2) with
  | [], _ -> true
  | _, [] -> false
  | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then is_ordered_subsequence t1 t2
      else is_ordered_subsequence lst1 t2

(* Single-arm-insertion check. Returns [Some (arm, idx)] iff [ps2] is [ps1]
   with exactly one extra element [arm] inserted at position [idx]; otherwise
   [None]. Walks both lists in lockstep, consuming equal heads, then declares
   the first divergence to be the inserted element and demands that the
   remainder line up exactly. We trust callers (per [Frontend.Policy]
   normalize) not to feed us duplicate arms. *)
let one_arm_added ps1 ps2 =
  let rec loop i l1 l2 =
    match (l1, l2) with
    | _, [] -> None
    | [], [ a ] -> Some (a, i)
    | [], _ :: _ :: _ -> None
    | x1 :: t1, x2 :: t2 when x1 = x2 -> loop (i + 1) t1 t2
    | _, x2 :: t2 -> if l1 = t2 then Some (x2, i) else None
  in
  loop 0 ps1 ps2

(* compute_*: turn two child lists into structured diffs. *)

(* SP-SP removed arms: position is the index in the *prev* list. *)
let compute_strict_arms_removed ps1 ps2 =
  let rec loop i1 l1 l2 acc =
    match (l1, l2) with
    | [], _ -> List.rev acc
    | p1 :: t1, [] ->
        loop (i1 + 1) t1 [] ({ path = [ i1 ]; arm = p1; weight = None } :: acc)
    | p1 :: t1, p2 :: t2 ->
        if p1 = p2 then loop (i1 + 1) t1 t2 acc
        else
          loop (i1 + 1) t1 l2 ({ path = [ i1 ]; arm = p1; weight = None } :: acc)
  in
  loop 0 ps1 ps2 []

(* RR / UNION: post-normalize both child lists are sorted, so each added
   (resp. removed) arm has a well-defined index in [ps2] (resp. [ps1]). *)
let compute_rr_arms_added ps1 ps2 =
  List.mapi (fun i p -> (i, p)) ps2
  |> List.filter (fun (_, p) -> not (List.mem p ps1))
  |> List.map (fun (i, p) -> { path = [ i ]; arm = p; weight = None })

let compute_rr_arms_removed ps1 ps2 =
  List.mapi (fun i p -> (i, p)) ps1
  |> List.filter (fun (_, p) -> not (List.mem p ps2))
  |> List.map (fun (i, p) -> { path = [ i ]; arm = p; weight = None })

(* WFQ: same shape as RR/UNION but elements are (policy, weight) pairs. *)
let compute_wfq_arms_added pairs1 pairs2 =
  List.mapi (fun i pw -> (i, pw)) pairs2
  |> List.filter (fun (_, pw) -> not (List.mem pw pairs1))
  |> List.map (fun (i, (p, w)) -> { path = [ i ]; arm = p; weight = Some w })

let compute_wfq_arms_removed pairs1 pairs2 =
  List.mapi (fun i pw -> (i, pw)) pairs1
  |> List.filter (fun (_, pw) -> not (List.mem pw pairs2))
  |> List.map (fun (i, (p, w)) -> { path = [ i ]; arm = p; weight = Some w })

(* WFQ pure-weight: walk parallel weight lists looking for *exactly one*
   position whose weight changed. If zero positions differ, the caller
   should never have ended up here; if two or more differ we give up
   ([None]) and the caller emits [VeryDifferent]. Caller guarantees
   equal length. *)
let compute_single_weight_change ws1 ws2 =
  let rec loop i found l1 l2 =
    match (l1, l2) with
    | [], [] -> found
    | w1 :: t1, w2 :: t2 -> (
        if w1 = w2 then loop (i + 1) found t1 t2
        else
          let wc = { path = [ i ]; new_weight = w2 } in
          match found with
          | None -> loop (i + 1) (Some wc) t1 t2
          | Some _ -> None)
    | _ ->
        failwith
          "compute_single_weight_change: ws1 and ws2 must have equal length"
  in
  loop 0 None ws1 ws2

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

let rec compare_lists ps1 ps2 =
  (* Compare two equal-length lists of children structurally. Scan every
     position so we can tell whether exactly one child differs (precise
     diff that we propagate, with the index baked into its path) or
     several do (we can't pinpoint a single change, so [VeryDifferent]
     at this level). The latter case is what gives [OneArmReplaced] its
     "exactly one" guarantee. *)
  if List.compare_lengths ps1 ps2 <> 0 then VeryDifferent []
  else
    let rec scan i acc l1 l2 =
      match (l1, l2) with
      | [], [] -> List.rev acc
      | p1 :: t1, p2 :: t2 ->
          let acc' =
            match analyze p1 p2 with
            | Same -> acc
            | other -> (i, other) :: acc
          in
          scan (i + 1) acc' t1 t2
      | _ ->
          failwith
            "same length guaranteed by outer condition; we'll never get here"
    in
    match scan 0 [] ps1 ps2 with
    | [] -> Same
    | [ (i, diff) ] -> prepend_path i diff
    | _ :: _ :: _ -> VeryDifferent []

(* SP/RR/UNION share a flat list-of-children shape, so they share the same
   diff strategy: precise [one_arm_added] (the patcher's main trick — works
   for inserts at any position, not just the end), then the broader
   subsequence-based [ArmsAdded] / [OneArmRemoved], then structural
   [compare_lists]. The two callers (SP and RR/UNION) differ only in how
   they compute the multi-arm diffs, so we parameterize over [added_fn] /
   [removed_fn]. [OneArmRemoved] only fires when exactly one arm was
   dropped; multi-arm removals degrade to [VeryDifferent]. *)
and compare_flat ~removed_fn ps1 ps2 =
  match one_arm_added ps1 ps2 with
  | Some (arm, idx) -> OneArmAdded { path = [ idx ]; arm; weight = None }
  | None ->
      if is_ordered_subsequence ps1 ps2 then VeryDifferent []
      else if is_ordered_subsequence ps2 ps1 then
        match removed_fn ps1 ps2 with
        | [ ad ] -> OneArmRemoved ad
        | _ -> VeryDifferent []
      else compare_lists ps1 ps2

and compare_strict ps1 ps2 =
  compare_flat ~removed_fn:compute_strict_arms_removed ps1 ps2

and compare_rr_like ps1 ps2 =
  (* for RR and Union *)
  compare_flat ~removed_fn:compute_rr_arms_removed ps1 ps2

and compare_wfq ps1 ws1 ps2 ws2 =
  (* WFQ never reports [OneArmAdded] — the patcher is out of scope
     for WFQ either way (weight changes, weighted arm-adds), so we let
     [analyze] describe the change as precisely as it can.
     The patcher will give up later. *)
  let pairs1 = List.combine ps1 ws1 in
  let pairs2 = List.combine ps2 ws2 in
  match one_arm_added pairs1 pairs2 with
  | Some ((arm, weight), idx) ->
      OneArmAdded { path = [ idx ]; arm; weight = Some weight }
  | None ->
      if is_ordered_subsequence ps1 ps2 then VeryDifferent []
      else if is_ordered_subsequence pairs2 pairs1 then
        match compute_wfq_arms_removed pairs1 pairs2 with
        | [ ad ] -> OneArmRemoved ad
        | _ -> VeryDifferent []
      else if ps1 = ps2 then
        (* Same arms in same positions, but weights changed. We only
       describe this precisely when *exactly one* weight moved;
       otherwise give up. *)
        match compute_single_weight_change ws1 ws2 with
        | Some wc -> WeightChanged wc
        | None -> VeryDifferent []
      else if ws1 = ws2 then
        (* Same weights; the diff must be inside the children themselves. *)
        compare_lists ps1 ps2
      else
        (* Mixed differences (e.g. remove-and-reweight). *)
        VeryDifferent []

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
        | UNION ps1, UNION ps2 -> compare_rr_like ps1 ps2
        | SP ps1, SP ps2 -> compare_strict ps1 ps2
        | RR ps1, RR ps2 -> compare_rr_like ps1 ps2
        | WFQ (ps1, ws1), WFQ (ps2, ws2) -> compare_wfq ps1 ws1 ps2 ws2
        | _ ->
            (* FIFO→FIFO with a different class, or any constructor mismatch
               (FIFO↔SP, SP↔RR, etc.) — wholesale replacement at this
               position. The leaf-level diff is path-empty; [compare_lists]'s
               [prepend_path] tags on the child index when this bubbles up,
               but only if it's the sole divergence at that level. *)
            OneArmReplaced { path = []; arm = p2; weight = None }

(* Pretty-printers — only used to format failure messages from the test
   suite; the patcher never goes through these. *)

let path_to_string = function
  | [] -> "(root)"
  | [ i ] -> Printf.sprintf "(child %d)" i
  | p ->
      let s = String.concat "->" (List.map string_of_int p) in
      Printf.sprintf "(path %s)" s

let string_of_arm_diff { path; arm; weight } =
  let base =
    Printf.sprintf "%s at %s"
      (Frontend.Policy.to_string arm)
      (path_to_string path)
  in
  match weight with
  | None -> base
  | Some w -> Printf.sprintf "%s with weight %g" base w

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

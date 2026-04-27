open Frontend.Policy

type path = int list

(* A structural diff that can describe *where* a change occurs.
   A change at the root has path = []. *)
type t =
  | Same
  | Change of path * change

and change =
  | OneArmAppended of Frontend.Policy.t
  | ArmsAdded of {
      old_count : int;
      new_count : int;
      details : string; (* human-readable description of what was added *)
    }
  | ArmsRemoved of {
      old_count : int;
      new_count : int;
      details : string; (* human-readable description of what was removed *)
    }
  | WeightChanged of {
      details : string;
          (* human-readable per-arm description of weight changes *)
    }
  | VeryDifferent
  | SuperPol

let list_diff xs ys = List.filter (fun x -> not (List.mem x ys)) xs

(* Check if lst1 appears as an order-preserving sub-sequence of lst2.
   E.g. [A,C] is an order-preserving sub-sequence of [A,B,C,D], but
   [C,A] is not. *)
let rec is_ordered_subsequence lst1 lst2 =
  match (lst1, lst2) with
  | [], _ -> true (* Empty list is sub-sequence of anything *)
  | _, [] -> false (* Non-empty list can't be sub-sequence of empty *)
  | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then is_ordered_subsequence t1 t2
      else is_ordered_subsequence lst1 t2

(* Greedy single-arm-append check that matches before [OneArmAppended]. 
   Returns the appended arm if [ps2] is exactly [ps1] with one extra element
   tacked onto the end; otherwise [None]. Strictly narrower than
   [is_ordered_subsequence]. *)
let one_arm_appended ps1 ps2 =
  match List.rev ps2 with
  | last :: init_rev when List.rev init_rev = ps1 -> Some last
  | _ -> None

let arms_added_by_subseq details_fn lst1 lst2 =
  let old_count = List.length lst1 in
  let new_count = List.length lst2 in
  let details = details_fn lst1 lst2 in
  Change ([], ArmsAdded { old_count; new_count; details })

(* Mirror of [arms_added_by_subseq] for the case where [lst2] is an ordered
   subsequence of [lst1] — i.e., arms were removed from [lst1] to get [lst2]. *)
let arms_removed_by_subseq details_fn lst1 lst2 =
  let old_count = List.length lst1 in
  let new_count = List.length lst2 in
  let details = details_fn lst1 lst2 in
  Change ([], ArmsRemoved { old_count; new_count; details })

(* The function we'll use to construct SP-SP details when arms are added *)
let details_strict_arm_added ps1 ps2 =
  let rec loop i2 l1 l2 acc =
    match (l1, l2) with
    | _, [] -> acc
    | [], p2 :: t2 ->
        loop (i2 + 1) [] t2
          (Printf.sprintf "added %s at %d" (Frontend.Policy.to_string p2) i2
          :: acc)
    | p1 :: t1, p2 :: t2 ->
        if p1 = p2 then loop (i2 + 1) t1 t2 acc
        else
          (* p2 is an inserted arm *)
          loop (i2 + 1) l1 t2
            (Printf.sprintf "added %s at %d" (Frontend.Policy.to_string p2) i2
            :: acc)
  in
  loop 0 ps1 ps2 [] |> List.rev |> String.concat ", "

(* The function we'll use to construct RR-RR or Union-Union details when
   arms are added *)
let details_rr_arm_added additions =
  (* Sometimes we need text details but the policy doesn't care about
     the _index_ of the addition. *)
  if additions = [] then ""
  else
    "added "
    ^ (additions |> List.map Frontend.Policy.to_string |> String.concat ", ")

(* WFQ: build details for added (policy,weight) arms *)
let details_wfq_arm_added pairs1 pairs2 =
  let added_pairs = List.filter (fun pw -> not (List.mem pw pairs1)) pairs2 in
  match added_pairs with
  | [] -> ""
  | _ ->
      added_pairs
      |> List.map (fun (p, w) ->
          Printf.sprintf "added %s with weight %g"
            (Frontend.Policy.to_string p)
            w)
      |> String.concat ", "

(* SP-SP details when arms are removed: the position is the index in the
   *prev* list (ps1, the longer one), since that's where the missing arm
   used to live. *)
let details_strict_arm_removed ps1 ps2 =
  let rec loop i1 l1 l2 acc =
    match (l1, l2) with
    | [], _ -> acc
    | p1 :: t1, [] ->
        loop (i1 + 1) t1 []
          (Printf.sprintf "removed %s from %d" (Frontend.Policy.to_string p1) i1
          :: acc)
    | p1 :: t1, p2 :: t2 ->
        if p1 = p2 then loop (i1 + 1) t1 t2 acc
        else
          (* p1 is a removed arm *)
          loop (i1 + 1) t1 l2
            (Printf.sprintf "removed %s from %d"
               (Frontend.Policy.to_string p1)
               i1
            :: acc)
  in
  loop 0 ps1 ps2 [] |> List.rev |> String.concat ", "

(* RR/Union details when arms are removed: position-agnostic, like the
   _added counterpart. *)
let details_rr_arm_removed removals =
  if removals = [] then ""
  else
    "removed "
    ^ (removals |> List.map Frontend.Policy.to_string |> String.concat ", ")

(* WFQ: build details for removed (policy,weight) arms *)
let details_wfq_arm_removed pairs1 pairs2 =
  let removed_pairs = List.filter (fun pw -> not (List.mem pw pairs2)) pairs1 in
  match removed_pairs with
  | [] -> ""
  | _ ->
      removed_pairs
      |> List.map (fun (p, w) ->
          Printf.sprintf "removed %s with weight %g"
            (Frontend.Policy.to_string p)
            w)
      |> String.concat ", "

(* WFQ: per-arm "old → new" deltas, restricted to arms whose weight actually
   changed. Caller guarantees [ps1 = ps2] and [List.length ws1 = List.length ws2]. *)
let details_wfq_weight_changed ps ws1 ws2 =
  List.map2 (fun w1 w2 -> (w1, w2)) ws1 ws2
  |> List.combine ps
  |> List.filter (fun (_, (w1, w2)) -> w1 <> w2)
  |> List.map (fun (p, (w1, w2)) ->
      Printf.sprintf "%s: %g → %g" (Frontend.Policy.to_string p) w1 w2)
  |> String.concat ", "

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
    | FIFO _ -> (false, []) (* No sub-policies inside FIFO *)
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
  (* Compare two equal-length lists of children structurally and report
     the index of the first child that differs. *)
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  if len1 <> len2 then Change ([], VeryDifferent)
  else
    let rec loop i l1 l2 =
      match (l1, l2) with
      | [], [] -> Same
      | p1 :: t1, p2 :: t2 -> (
          match analyze p1 p2 with
          | Same -> loop (i + 1) t1 t2
          | Change (child_path, child_change) ->
              Change (i :: child_path, child_change))
      | _ ->
          failwith
            "same length guaranteed by outer condition; we'll never get here"
    in
    loop 0 ps1 ps2

(* SP/RR/UNION share a flat list-of-children shape, so they share the same
   diff strategy: greedy [one_arm_appended] (the only thing the patcher
   knows how to do), then the broader subsequence-based [ArmsAdded] /
   [ArmsRemoved], then structural [compare_lists]. The only thing that
   varies between SP and RR/UNION is how we describe added/removed arms in
   human-readable form, so we parameterize over [added_fn] / [removed_fn]. *)
and compare_flat ~added_fn ~removed_fn ps1 ps2 =
  match one_arm_appended ps1 ps2 with
  | Some arm -> Change ([], OneArmAppended arm)
  | None ->
      if is_ordered_subsequence ps1 ps2 then
        arms_added_by_subseq added_fn ps1 ps2
      else if is_ordered_subsequence ps2 ps1 then
        arms_removed_by_subseq removed_fn ps1 ps2
      else compare_lists ps1 ps2

and compare_strict ps1 ps2 =
  compare_flat ~added_fn:details_strict_arm_added
    ~removed_fn:details_strict_arm_removed ps1 ps2

and compare_rr_like ps1 ps2 =
  (* for RR and Union *)
  compare_flat
    ~added_fn:(fun xs1 xs2 -> details_rr_arm_added (list_diff xs2 xs1))
    ~removed_fn:(fun xs1 xs2 -> details_rr_arm_removed (list_diff xs1 xs2))
    ps1 ps2

and compare_wfq ps1 ws1 ps2 ws2 =
  (* WFQ never reports [OneArmAppended] — the patcher is out of scope
     for WFQ either way (weight changes, weighted arm-adds), so we let
     [analyze] describe the change as precisely as it can.
     The patcher will give up later. *)
  let pairs1 = List.combine ps1 ws1 in
  let pairs2 = List.combine ps2 ws2 in
  if is_ordered_subsequence pairs1 pairs2 then
    arms_added_by_subseq details_wfq_arm_added pairs1 pairs2
  else if is_ordered_subsequence pairs2 pairs1 then
    arms_removed_by_subseq details_wfq_arm_removed pairs1 pairs2
  else if ps1 = ps2 then
    (* Same arms in same positions, but weights changed. (Pure weight diff
       defeats both subseq checks because (policy, weight) pairs differ.) *)
    Change
      ([], WeightChanged { details = details_wfq_weight_changed ps1 ws1 ws2 })
  else if ws1 = ws2 then
    (* Same weights, so fall back to structural child diff on arms *)
    compare_lists ps1 ps2
  else
    (* Mixed differences (e.g. remove-and-reweight) *)
    Change ([], VeryDifferent)

and analyze p1 p2 =
  if p1 = p2 then Same
  else
    let found, idx = is_sub_policy p1 p2 in
    if found then Change (idx, SuperPol)
    else
      match (p1, p2) with
      | FIFO _, FIFO _ -> Change ([], VeryDifferent)
      | UNION ps1, UNION ps2 -> compare_rr_like ps1 ps2
      | SP ps1, SP ps2 -> compare_strict ps1 ps2
      | RR ps1, RR ps2 -> compare_rr_like ps1 ps2
      | WFQ (ps1, ws1), WFQ (ps2, ws2) -> compare_wfq ps1 ws1 ps2 ws2
      | _ -> Change ([], VeryDifferent)

let rec to_string diff =
  match diff with
  | Same -> "Same"
  | Change (path, change) ->
      let loc =
        match path with
        | [] -> "(root)"
        | [ i ] -> Printf.sprintf "(child %d)" i
        | path ->
            let s = String.concat "->" (List.map string_of_int path) in
            Printf.sprintf "(path %s)" s
      in
      Printf.sprintf "%s: %s" loc (change_to_string change)

and change_to_string = function
  | OneArmAppended arm ->
      Printf.sprintf "OneArmAppended: %s" (Frontend.Policy.to_string arm)
  | ArmsAdded { old_count; new_count; details } ->
      if details = "" then
        Printf.sprintf "ArmsAdded %d → %d" old_count new_count
      else Printf.sprintf "ArmsAdded %d → %d: %s" old_count new_count details
  | ArmsRemoved { old_count; new_count; details } ->
      if details = "" then
        Printf.sprintf "ArmsRemoved %d → %d" old_count new_count
      else Printf.sprintf "ArmsRemoved %d → %d: %s" old_count new_count details
  | WeightChanged { details } ->
      if details = "" then "WeightChanged"
      else Printf.sprintf "WeightChanged: %s" details
  | VeryDifferent -> "VeryDifferent"
  | SuperPol -> "SuperPol"

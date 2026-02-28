open Frontend.Policy

type path = int list

(* A structural diff that can describe *where* a change occurs. *)
type t =
  | Same
  | Change of path * change
(* Path of child indices; change type *)
(* An empty path means that the change is at the root level *)

and change =
  | ArmsAdded of {
      old_count : int;
      new_count : int;
      details : string; (* human-readable description of what was added *)
    }
  | VeryDifferent
  | SuperPol

let subset lst1 lst2 = List.for_all (fun x -> List.mem x lst2) lst1

(* Arms are guaranteed unique, so set-equality is fine. *)
let set_equal xs ys = subset xs ys && subset ys xs
let list_diff xs ys = List.filter (fun x -> not (List.mem x ys)) xs
let list_inter xs ys = List.filter (fun x -> List.mem x ys) xs

(* Check if lst1 appears as an order-preserving sub-sequence of lst2. E.g. [A,C] is an order-preserving sub-sequence of [A,B,C,D], but [C,A] is not. *)
let rec is_ordered_subsequence lst1 lst2 =
  match (lst1, lst2) with
  | [], _ -> true (* Empty list is subsequence of anything *)
  | _, [] -> false (* Non-empty list can't be subsequence of empty *)
  | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then is_ordered_subsequence t1 t2
      else is_ordered_subsequence lst1 t2

and is_sub_policy p1 p2 : bool * path =
  (* Is p1 a sub-policy of p2?
     Returns (found, path). When found at an *immediate* child i, path = [i].
     When found deeper, path = i :: deeper_path.
     Returns (true, []) when p1 = p2.
     Returns (false, []) otherwise. *)
  if p1 = p2 then (true, [])
  else
    match p2 with
    | FIFO _ | EDF _ -> (false, []) (* No sub-policies inside FIFO/EDF *)
    | Strict ps | RR ps | WFQ (ps, _) ->
        let rec loop i = function
          | [] -> (false, [])
          | p :: t ->
              if p = p1 then (true, [ i ])
              else
                let found, path = is_sub_policy p1 p in
                if found then (true, i :: path) else loop (i + 1) t
        in
        loop 0 ps

and compare_lists ps1 ps2 =
  (* Compare lists of children structurally and report the index of the change. *)
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

and details_strict ps1 ps2 =
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

(* Strict comparison: detect arms added (order-preserving) or treat other diffs. *)
and compare_strict ps1 ps2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  if len2 > len1 && is_ordered_subsequence ps1 ps2 then
    let details = details_strict ps1 ps2 in
    Change ([], ArmsAdded { old_count = len1; new_count = len2; details })
  else if len1 > len2 then Change ([], VeryDifferent) (* Arms removed *)
  else if len1 = len2 && set_equal ps1 ps2 then Change ([], VeryDifferent)
    (* Same arms, different order *)
  else compare_lists ps1 ps2
(* else, we can't figure it out at the root level so we'll dig deeper *)

and details_added_indexless added =
  match added with
  | [] -> ""
  | _ ->
      "added "
      ^ (added |> List.map Frontend.Policy.to_string |> String.concat ", ")

(* RR comparison: detect arms added; removals are VeryDifferent; otherwise recurse. *)
and compare_rr_like ps1 ps2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  if len2 > len1 && subset ps1 ps2 then
    let added = list_diff ps2 ps1 in
    let details = details_added_indexless added in
    Change ([], ArmsAdded { old_count = len1; new_count = len2; details })
  else if len1 > len2 then Change ([], VeryDifferent)
  else compare_lists ps1 ps2

(* Helper: check if old weights are preserved in new weights *)
and old_weights_preserved old_weights new_weights =
  let old_count = List.length old_weights in
  old_count <= List.length new_weights
  && List.for_all2 ( = ) old_weights
       (List.filteri (fun i _ -> i < old_count) new_weights)

(* WFQ comparison: detect arms added (with preserved old weights), or differences. *)
and compare_wfq ps1 ws1 ps2 ws2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  if ps1 = ps2 && ws1 <> ws2 then Change ([], VeryDifferent)
    (* Same arms, weights changed *)
  else if len2 > len1 && subset ps1 ps2 && old_weights_preserved ws1 ws2 then
    let added_pairs =
      List.combine ps2 ws2 |> List.filter (fun (p, _w) -> not (List.mem p ps1))
    in
    let details =
      match added_pairs with
      | [] -> ""
      | _ ->
          added_pairs
          |> List.map (fun (p, w) ->
              Printf.sprintf "added %s with weight %g"
                (Frontend.Policy.to_string p)
                w)
          |> String.concat ", "
    in
    Change ([], ArmsAdded { old_count = len1; new_count = len2; details })
  else if len1 > len2 then Change ([], VeryDifferent) (* Arms removed *)
  else compare_lists ps1 ps2

and analyze p1 p2 : t =
  if p1 = p2 then Same
  else
    let found, idx = is_sub_policy p1 p2 in
    if found then Change (idx, SuperPol)
    else
      match (p1, p2) with
      | FIFO _, FIFO _ | EDF _, EDF _ -> Change ([], VeryDifferent)
      | Strict ps1, Strict ps2 -> compare_strict ps1 ps2
      | RR ps1, RR ps2 -> compare_rr_like ps1 ps2
      | WFQ (ps1, ws1), WFQ (ps2, ws2) -> compare_wfq ps1 ws1 ps2 ws2
      | _, _ -> Change ([], VeryDifferent)

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
  | ArmsAdded { old_count; new_count; details } ->
      if details = "" then
        Printf.sprintf "ArmsAdded %d → %d" old_count new_count
      else Printf.sprintf "ArmsAdded %d → %d: %s" old_count new_count details
  | VeryDifferent -> "VeryDifferent"
  | SuperPol -> "SuperPol"

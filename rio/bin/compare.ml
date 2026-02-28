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

let policy_type_name = function
  | FIFO _ -> "FIFO"
  | EDF _ -> "EDF"
  | Strict _ -> "SP"
  | RR _ -> "RR"
  | WFQ _ -> "WFQ"

let subset lst1 lst2 = List.for_all (fun x -> List.mem x lst2) lst1

(* Check if lst1 appears as an order-preserving subsequence of lst2 *)
let rec is_ordered_subsequence lst1 lst2 =
  match (lst1, lst2) with
  | [], _ -> true (* Empty list is subsequence of anything *)
  | _, [] -> false (* Non-empty list can't be subsequence of empty *)
  | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then is_ordered_subsequence t1 t2
      else is_ordered_subsequence lst1 t2

let rec is_sub_policy p1 p2 : bool * int list option =
  (* Is p1 a sub-policy of p2? Return (found, index) where [index] is the
     immediate child index of [p2] that contains [p1] (or None if p1 == p2
     or if p1 is found at the root). Examples:
      - RR(A, B) is NOT sub-policy of RR(A, B, C); that should be ArmsAdded
      - WFQ(A, B) is NOT a sub-policy of WFQ(A, B, C), for the same reason
      - RR(A, B) is a sub-policy of SP(RR(A, B), C) -> (true, Some 0)
      - RR(A, B) is a sub-policy of SP(RR(RR(A, B),C), D) -> (true, Some 0)
  *)
  if p1 = p2 then (true, None)
  else
    match p2 with
    | FIFO _ | EDF _ -> (false, None)
    | Strict ps | RR ps ->
        let rec loop i = function
          | [] -> (false, None)
          | p :: t ->
              if p = p1 then (true, Some [ i ])
              else
                let found, path = is_sub_policy p1 p in
                if found then
                  match path with
                  | None -> (true, Some [ i ])
                  | Some pth -> (true, Some (i :: pth))
                else loop (i + 1) t
        in
        loop 0 ps
    | WFQ (ps, _) ->
        let rec loop i = function
          | [] -> (false, None)
          | p :: t ->
              if p = p1 then (true, Some [ i ])
              else
                let found, path = is_sub_policy p1 p in
                if found then
                  match path with
                  | None -> (true, Some [ i ])
                  | Some pth -> (true, Some (i :: pth))
                else loop (i + 1) t
        in
        loop 0 ps

(* Compare lists of children structurally and report the index of the change. *)
let rec compare_lists ps1 ps2 =
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
      | _ -> Same (* Same length guaranteed by outer condition *)
    in
    loop 0 ps1 ps2

and details_strict ps1 ps2 =
  (* For SP, report what was added in a human-readable way, e.g. "added C at index 2" or "added B at index 1". *)
  let rec go i1 i2 l1 l2 acc =
    match (l1, l2) with
    | _, [] -> acc
    | [], p2 :: t2 ->
        go i1 (i2 + 1) [] t2
          (Printf.sprintf "added %s at %d" (Frontend.Policy.to_string p2) i2
          :: acc)
    | p1 :: t1, p2 :: t2 ->
        if p1 = p2 then go (i1 + 1) (i2 + 1) t1 t2 acc
        else
          go i1 (i2 + 1) l1 t2
            (Printf.sprintf "added %s at %d" (Frontend.Policy.to_string p2) i2
            :: acc)
  in
  go 0 0 ps1 ps2 [] |> List.rev |> String.concat ", "

(* Strict comparison: detect arms added (order-preserving) or treat other diffs. *)
and compare_strict ps1 ps2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  let found, idx = is_sub_policy (Strict ps1) (Strict ps2) in
  if found then Change (Option.get idx, SuperPol)
  else if len2 > len1 && is_ordered_subsequence ps1 ps2 then
    (* Arms added: old arms appear in the same order in new list *)
    let details = details_strict ps1 ps2 in
    Change ([], ArmsAdded { old_count = len1; new_count = len2; details })
  else if len1 > len2 then
    (* Some arms were removed: treat as VeryDifferent *)
    Change ([], VeryDifferent)
  else if len1 = len2 && subset ps1 ps2 && subset ps2 ps1 then
    (* Same arms but different order *)
    Change ([], VeryDifferent)
  else compare_lists ps1 ps2
(* else, we can't figure it out at the root level so we'll dig deeper *)

(* RR comparison: detect arms added or removed. Removals are considered VeryDifferent. Otherwise recurse *)
and compare_rr_like ps1 ps2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  let found, idx = is_sub_policy (RR ps1) (RR ps2) in
  if found then Change (Option.get idx, SuperPol)
  else if len2 > len1 && subset ps1 ps2 then
    (* Order-insensitive: just list what was added, no index needed *)
    let added =
      ps2
      |> List.filter (fun p -> not (List.mem p ps1))
      |> List.map (fun p -> Frontend.Policy.to_string p)
    in
    let details =
      match added with
      | [] -> "" (* Shouldn't happen if len2 > len1 and subset ps1 ps2 *)
      | _ -> "added " ^ String.concat ", " added
    in
    Change ([], ArmsAdded { old_count = len1; new_count = len2; details })
  else if len1 > len2 then
    (* Arms were removed: treat as VeryDifferent *)
    Change ([], VeryDifferent)
  else compare_lists ps1 ps2

(* Helper: check if old weights are preserved in new weights *)
and old_weights_preserved old_weights new_weights old_count =
  old_count <= List.length new_weights
  && List.for_all2 ( = ) old_weights
       (List.filteri (fun i _ -> i < old_count) new_weights)

(* WFQ comparison: detect arms added or removed. Removals are considered VeryDifferent. If arms are the same but weights differ, that's also VeryDifferent. Otherwise recurse. *)
and compare_wfq ps1 ws1 ps2 ws2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  let found, idx = is_sub_policy (WFQ (ps1, ws1)) (WFQ (ps2, ws2)) in
  if found then Change (Option.get idx, SuperPol)
  else if ps1 = ps2 && ws1 <> ws2 then
    (* Same arms but different weights *)
    Change ([], VeryDifferent)
  else if len2 > len1 && subset ps1 ps2 && old_weights_preserved ws1 ws2 len1
  then
    (* Arms added with same weights for old arms *)
    (* Collect only newly added (arm, weight) pairs, without exposing index. *)
    let added_pairs =
      (* Pair each p2 with its corresponding w2, then filter by not in ps1 *)
      let zipped = List.combine ps2 ws2 in
      zipped |> List.filter (fun (p, _w) -> not (List.mem p ps1))
    in
    let details =
      match added_pairs with
      | [] -> ""
      | _ ->
          let parts =
            added_pairs
            |> List.map (fun (p, w) ->
                Printf.sprintf "added %s with weight %g"
                  (Frontend.Policy.to_string p)
                  w)
          in
          String.concat ", " parts
    in
    Change ([], ArmsAdded { old_count = len1; new_count = len2; details })
  else if len1 > len2 then
    (* Arms removed: treat as VeryDifferent *)
    Change ([], VeryDifferent)
  else compare_lists ps1 ps2

(* Main structural comparison *)
and analyze p1 p2 : t =
  if p1 = p2 then Same
  else
    match (p1, p2) with
    | FIFO _, FIFO _ | EDF _, EDF _ -> Change ([], VeryDifferent)
    | Strict ps1, Strict ps2 -> compare_strict ps1 ps2
    | RR ps1, RR ps2 -> compare_rr_like ps1 ps2
    | WFQ (ps1, ws1), WFQ (ps2, ws2) -> compare_wfq ps1 ws1 ps2 ws2
    | _, _ ->
        let found, idx = is_sub_policy p1 p2 in
        if found then Change (Option.value idx ~default:[], SuperPol)
        else Change ([], VeryDifferent)

(* Pretty-printing *)
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

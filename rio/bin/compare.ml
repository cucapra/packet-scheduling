open Frontend.Policy

type path = int list

(* A structural diff that can describe *where* a change occurs.
   A change at the root has path = []. *)
type t = Same | Change of path * change

and change =
  | ArmsAdded of {
      old_count : int;
      new_count : int;
      added : added_arms;
          (* The arms that were appended to reach the new policy. *)
    }
  | VeryDifferent
  | SuperPol

(* The arms added on the right of an [ArmsAdded] change. We keep the WFQ
   case separate because each added arm comes with a weight that the IR
   patcher needs in order to emit the right [Change_weight] instructions.

   Append-only by construction: this scope handles the case where every
   arm of the old policy lines up as a prefix of the new policy's arms.
   Anything more interesting (mid-insert, reorder + add, weight change)
   currently demotes to [VeryDifferent]. *)
and added_arms =
  | Plain of Frontend.Policy.t list
  | Wfq of (Frontend.Policy.t * float) list

(* Is [lst1] a prefix of [lst2]? *)
let rec is_prefix lst1 lst2 =
  match (lst1, lst2) with
  | [], _ -> true
  | _, [] -> false
  | h1 :: t1, h2 :: t2 -> h1 = h2 && is_prefix t1 t2

(* Drop the first [n] elements of [xs]. Returns [] if [n >= length xs]. *)
let rec drop n xs =
  if n <= 0 then xs
  else match xs with [] -> [] | _ :: t -> drop (n - 1) t

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

(* Given the prefix-check has already succeeded, build the [ArmsAdded]
   change describing the appended tail. *)
let arms_added_plain ps1 ps2 =
  let old_count = List.length ps1 in
  let new_count = List.length ps2 in
  let added = Plain (drop old_count ps2) in
  Change ([], ArmsAdded { old_count; new_count; added })

let arms_added_wfq pairs1 pairs2 =
  let old_count = List.length pairs1 in
  let new_count = List.length pairs2 in
  let added = Wfq (drop old_count pairs2) in
  Change ([], ArmsAdded { old_count; new_count; added })

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

(* SP, RR, UNION all share the same shape: a flat list of child policies.
   The append-only contract: if [ps1] is a strict prefix of [ps2] we report
   [ArmsAdded]; if the lengths match we recurse with [compare_lists];
   anything else (mid-insert, reorder+add, removal) → [VeryDifferent]. *)
and compare_plain ps1 ps2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  if len1 = len2 then compare_lists ps1 ps2
  else if len1 < len2 && is_prefix ps1 ps2 then arms_added_plain ps1 ps2
  else Change ([], VeryDifferent)

(* WFQ is just like [compare_plain] but on (policy, weight) pairs, so an
   added arm is identified by (policy, weight) rather than by policy alone. *)
and compare_wfq ps1 ws1 ps2 ws2 =
  let pairs1 = List.combine ps1 ws1 in
  let pairs2 = List.combine ps2 ws2 in
  let len1 = List.length pairs1 in
  let len2 = List.length pairs2 in
  if len1 = len2 then
    if ws1 = ws2 then compare_lists ps1 ps2
    else Change ([], VeryDifferent)
  else if len1 < len2 && is_prefix pairs1 pairs2 then
    arms_added_wfq pairs1 pairs2
  else Change ([], VeryDifferent)

and analyze p1 p2 =
  if p1 = p2 then Same
  else
    let found, idx = is_sub_policy p1 p2 in
    if found then Change (idx, SuperPol)
    else
      match (p1, p2) with
      | FIFO _, FIFO _ -> Change ([], VeryDifferent)
      | UNION ps1, UNION ps2 -> compare_plain ps1 ps2
      | SP ps1, SP ps2 -> compare_plain ps1 ps2
      | RR ps1, RR ps2 -> compare_plain ps1 ps2
      | WFQ (ps1, ws1), WFQ (ps2, ws2) -> compare_wfq ps1 ws1 ps2 ws2
      | _ -> Change ([], VeryDifferent)

let added_arms_to_string = function
  | Plain ps -> ps |> List.map Frontend.Policy.to_string |> String.concat ", "
  | Wfq pws ->
      pws
      |> List.map (fun (p, w) ->
             Printf.sprintf "%s with weight %g" (Frontend.Policy.to_string p) w)
      |> String.concat ", "

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
  | ArmsAdded { old_count; new_count; added } ->
      let summary = added_arms_to_string added in
      if summary = "" then
        Printf.sprintf "ArmsAdded %d → %d" old_count new_count
      else
        Printf.sprintf "ArmsAdded %d → %d: added %s" old_count new_count summary
  | VeryDifferent -> "VeryDifferent"
  | SuperPol -> "SuperPol"

open Frontend.Policy

type path = int list

(* A structural diff that can describe *where* a change occurs.
   A change at the root has path = []. *)
type t =
  | Same
  | Change of path * change

and change =
  | ArmAdded of Frontend.Policy.t
    (* Exactly one arm was appended at the indicated parent. The patcher
         has access to the parent's [prev] policy via the path, so it can
         derive arity (and, for SP, the new arm's positional weight) on
         its own; we just hand it the new arm. *)
  | VeryDifferent
  | SuperPol

(* If [ps2] is exactly [ps1] with one extra element appended, return that
   element; otherwise [None]. The intentionally narrow check that gates
   the [ArmAdded] case — anything more interesting (multi-arm add,
   mid-insert, reorder + add, weight change) falls through to
   [VeryDifferent]. *)
let rec one_arm_appended ps1 ps2 =
  match (ps1, ps2) with
  | [], [ arm ] -> Some arm
  | h1 :: t1, h2 :: t2 when h1 = h2 -> one_arm_appended t1 t2
  | _ -> None

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

(* SP, RR, UNION all share the same shape: a flat list of child policies.
   When the lengths match, recurse to find a deeper diff; when [ps2] is
   [ps1] plus exactly one appended arm, report [ArmAdded]; otherwise
   give up with [VeryDifferent]. *)
and compare_plain ps1 ps2 =
  if List.length ps1 = List.length ps2 then compare_lists ps1 ps2
  else
    match one_arm_appended ps1 ps2 with
    | Some arm -> Change ([], ArmAdded arm)
    | None -> Change ([], VeryDifferent)

(* WFQ never itself produces an [ArmAdded] in this scope — weight changes
   make even an "appended arm" non-trivial for the patcher. We only let
   WFQ act as a transparent passthrough so deeper arm-additions can
   surface: when both the arms list and the weight list are pointwise
   equal, recurse via [compare_lists]. *)
and compare_wfq ps1 ws1 ps2 ws2 =
  if List.length ps1 = List.length ps2 && ws1 = ws2 then compare_lists ps1 ps2
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
  | ArmAdded arm ->
      Printf.sprintf "ArmAdded: %s" (Frontend.Policy.to_string arm)
  | VeryDifferent -> "VeryDifferent"
  | SuperPol -> "SuperPol"

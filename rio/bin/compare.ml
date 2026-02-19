open Frontend.Policy

(* A structural diff that can describe *where* a change occurs. *)
type t =
  | Same
  | NodeChange of {
      policy_type : string; (* The node where the change occurred *)
      index : int option; (* Which child changed, if any *)
      change : change; (* What changed *)
    }

and change =
  | ArmsAdded of {
      old_count : int;
      new_count : int;
    }
  | WeightsChanged of {
      old_weights : float list;
      new_weights : float list;
    }
  | SubChange of t (* Recursive diff *)
  | VeryDifferent
  | SuperPol

let policy_type_name = function
  | FIFO _ -> "FIFO"
  | EDF _ -> "EDF"
  | Strict _ -> "Strict"
  | RR _ -> "RoundRobin"
  | WFQ _ -> "WeightedFair"

let subset lst1 lst2 = List.for_all (fun x -> List.mem x lst2) lst1

(* Check if lst1 appears as an order-preserving subsequence of lst2 *)
let rec is_ordered_subsequence lst1 lst2 =
  match (lst1, lst2) with
  | [], _ -> true (* Empty list is subsequence of anything *)
  | _, [] -> false (* Non-empty list can't be subsequence of empty *)
  | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then is_ordered_subsequence t1 t2
      else is_ordered_subsequence lst1 t2

let rec is_sub_policy p1 p2 =
  (* Is p1 a sub-policy of p2? Examples:
      - RR(A, B) is NOT sub-policy of RR(A, B, C); that should be ArmsAdded
      - WFQ(A, B) is NOT a sub-policy of WFQ(A, B, C), for the same reason
      - RR(A, B) is a sub-policy of SP(RR(A, B), C)
      - RR(A, B) is a sub-policy of SP(RR(RR(A, B),C), D)
      *)
  if p1 = p2 then true
  else
    match p2 with
    | FIFO _ | EDF _ -> false
    | Strict ps | RR ps -> List.exists (is_sub_policy p1) ps
    | WFQ (ps, _) -> List.exists (is_sub_policy p1) ps

(* Compare lists of children structurally and report the index of the change. *)
let rec compare_lists policy_type ps1 ps2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  if len1 <> len2 then
    NodeChange { policy_type; index = None; change = VeryDifferent }
  else
    let rec loop i l1 l2 =
      match (l1, l2) with
      | [], [] -> Same
      | p1 :: t1, p2 :: t2 -> (
          match analyze p1 p2 with
          | Same -> loop (i + 1) t1 t2
          | d ->
              NodeChange { policy_type; index = Some i; change = SubChange d })
      | _ -> Same (* Same length guaranteed by outer condition *)
    in
    loop 0 ps1 ps2

(* Helper: centralize super-policy (nested) result *)
and make_super_pol policy_type =
  NodeChange { policy_type; index = None; change = SuperPol }

(* Strict comparison: detect arms added (preserving order) or recurse into children. *)
and compare_strict ps1 ps2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  if is_sub_policy (Strict ps1) (Strict ps2) then make_super_pol "Strict"
  else if len2 > len1 && is_ordered_subsequence ps1 ps2 then
    (* Arms added: old arms appear in the same order in new list *)
    NodeChange
      {
        policy_type = "Strict";
        index = None;
        change = ArmsAdded { old_count = len1; new_count = len2 };
      }
  else compare_lists "Strict" ps1 ps2

(* RR comparison: detect arms added or recurse into children. *)
and compare_rr_like policy_type ps1 ps2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  if is_sub_policy (RR ps1) (RR ps2) then make_super_pol policy_type
  else if len2 > len1 && subset ps1 ps2 then
    NodeChange
      {
        policy_type;
        index = None;
        change = ArmsAdded { old_count = len1; new_count = len2 };
      }
  else compare_lists policy_type ps1 ps2

(* WFQ comparison: detect arms added, weight changes, or recurse. *)
and compare_wfq ps1 ws1 ps2 ws2 =
  let len1 = List.length ps1 in
  let len2 = List.length ps2 in
  if is_sub_policy (WFQ (ps1, ws1)) (WFQ (ps2, ws2)) then
    make_super_pol "WeightedFair"
  else if len2 > len1 && subset ps1 ps2 then
    NodeChange
      {
        policy_type = "WeightedFair";
        index = None;
        change = ArmsAdded { old_count = len1; new_count = len2 };
      }
  else if ps1 = ps2 && ws1 <> ws2 then
    NodeChange
      {
        policy_type = "WeightedFair";
        index = None;
        change = WeightsChanged { old_weights = ws1; new_weights = ws2 };
      }
  else compare_lists "WeightedFair" ps1 ps2

(* Main structural comparison *)
and analyze p1 p2 : t =
  if p1 = p2 then Same
  else
    match (p1, p2) with
    | FIFO _, FIFO _ | EDF _, EDF _ ->
        NodeChange
          {
            policy_type = policy_type_name p1;
            index = None;
            change = VeryDifferent;
          }
    | Strict ps1, Strict ps2 -> compare_strict ps1 ps2
    | RR ps1, RR ps2 -> compare_rr_like "RoundRobin" ps1 ps2
    | WFQ (ps1, ws1), WFQ (ps2, ws2) -> compare_wfq ps1 ws1 ps2 ws2
    | _, _ ->
        if is_sub_policy p1 p2 then
          NodeChange
            {
              policy_type = policy_type_name p2;
              index = None;
              change = SuperPol;
            }
        else
          NodeChange
            {
              policy_type = policy_type_name p2;
              index = None;
              change = VeryDifferent;
            }

(* Pretty-printing *)
let rec to_string diff =
  match diff with
  | Same -> "Same"
  | NodeChange { policy_type; index; change } ->
      let loc =
        match index with
        | None -> "(root)"
        | Some i -> Printf.sprintf "(child %d)" i
      in
      Printf.sprintf "%s %s: %s" policy_type loc (change_to_string change)

and change_to_string = function
  | ArmsAdded { old_count; new_count } ->
      Printf.sprintf "ArmsAdded %d → %d" old_count new_count
  | WeightsChanged { old_weights; new_weights } ->
      let fmt ws =
        ws |> List.map (Printf.sprintf "%.9g") |> String.concat ", "
      in
      Printf.sprintf "WeightsChanged [%s] → [%s]" (fmt old_weights)
        (fmt new_weights)
  | SubChange d -> "SubChange(" ^ to_string d ^ ")"
  | VeryDifferent -> "Incompatible (different types or structure)"
  | SuperPol -> "OneIsNestedInOther"

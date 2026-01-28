(* ADT describing differences between two policies *)
type t =
  | Same
  | ArmAdded of {
      policy_type : string;
      old_count : int;
      new_count : int;
    }
  | WeightsChanged of {
      old_weights : float list;
      new_weights : float list;
    }
  | SuperPol of {
      outer_policy : string;
      inner_policy : string;
    }
  | VeryDifferent of { reason : string }

(* Check if policy1 appears as a sub-policy within policy2 *)
let rec is_subpolicy policy1 policy2 =
  if policy1 = policy2 then true
  else
    match policy2 with
    | Frontend.Policy.Strict ps | Frontend.Policy.RR ps ->
        List.exists (is_subpolicy policy1) ps
    | Frontend.Policy.WFQ (ps, _) -> List.exists (is_subpolicy policy1) ps
    | Frontend.Policy.FIFO _ | Frontend.Policy.EDF _ -> false

(* Get a simple string representation of a policy type *)
let policy_type_name = function
  | Frontend.Policy.FIFO _ -> "FIFO"
  | Frontend.Policy.EDF _ -> "EDF"
  | Frontend.Policy.Strict _ -> "Strict"
  | Frontend.Policy.RR _ -> "RoundRobin"
  | Frontend.Policy.WFQ _ -> "WeightedFair"

(* ---------- Helpers ---------- *)

(* Structural prefix check for lists of policies *)
let is_prefix xs ys =
  let rec go xs ys =
    match (xs, ys) with
    | [], _ -> true
    | _ :: _, [] -> false
    | x :: xs', y :: ys' -> x = y && go xs' ys'
  in
  List.length xs <= List.length ys && go xs ys

(* (Optional) subsequence check; keep but default to prefix semantics *)
let is_subsequence xs ys =
  let rec go xs ys =
    match (xs, ys) with
    | [], _ -> true
    | _ :: _, [] -> false
    | x :: xs', y :: ys' -> if x = y then go xs' ys' else go xs ys'
  in
  go xs ys

(* Overlap metric for better error messages: how many of xs are found in order within ys *)
let preserved_count xs ys =
  let rec go count xs ys =
    match (xs, ys) with
    | [], _ -> count
    | _, [] -> count
    | x :: xs', y :: ys' ->
        if x = y then go (count + 1) xs' ys' else go count xs ys'
  in
  go 0 xs ys

let float_ne ~eps a b = abs_float (a -. b) > eps

let weights_changed ~eps ws1 ws2 =
  let rec go = function
    | [], [] -> false
    | x :: xs, y :: ys -> if float_ne ~eps x y then true else go (xs, ys)
    | _ -> true (* different lengths definitely changed *)
  in
  go (ws1, ws2)

(* Centralized child extractors *)
let children_of = function
  | Frontend.Policy.Strict ps -> ps
  | Frontend.Policy.RR ps -> ps
  | Frontend.Policy.WFQ (ps, _) -> ps
  | Frontend.Policy.FIFO _ -> []
  | Frontend.Policy.EDF _ -> []

let weights_of = function
  | Frontend.Policy.WFQ (_, ws) -> Some ws
  | _ -> None

(* ArmAdded decision: prefer append-only (prefix) semantics. Switch to subsequence if desired. *)
let arm_added_like ~allow_insert_anywhere ps1 ps2 =
  if List.length ps2 = List.length ps1 + 1 then
    if allow_insert_anywhere then is_subsequence ps1 ps2 else is_prefix ps1 ps2
  else false

(* Analyze the difference between two policies *)
let analyze ?(allow_insert_anywhere = false) ?(weight_eps = 1e-9) p1 p2 : t =
  if p1 = p2 then Same
  else
    match (p1, p2) with
    (* Strict / RR: arm-structure-only nodes *)
    | Frontend.Policy.Strict ps1, Frontend.Policy.Strict ps2
    | Frontend.Policy.RR ps1, Frontend.Policy.RR ps2 ->
        if arm_added_like ~allow_insert_anywhere ps1 ps2 then
          ArmAdded
            {
              policy_type = policy_type_name p1;
              old_count = List.length ps1;
              new_count = List.length ps2;
            }
        else
          let kept = preserved_count ps1 ps2 in
          VeryDifferent
            {
              reason =
                Printf.sprintf "Different %s structure (%d/%d arms preserved)"
                  (policy_type_name p1) kept (List.length ps1);
            }
    (* WFQ: structure + weights *)
    | Frontend.Policy.WFQ (ps1, ws1), Frontend.Policy.WFQ (ps2, ws2) ->
        if ps1 = ps2 then
          if weights_changed ~eps:weight_eps ws1 ws2 then
            WeightsChanged { old_weights = ws1; new_weights = ws2 }
          else Same
        else if
          arm_added_like ~allow_insert_anywhere ps1 ps2
          && List.length ws2 = List.length ws1 + 1
        then
          (* Optional: you could also verify that ws2 starts with ws1 if using prefix mode *)
          ArmAdded
            {
              policy_type = policy_type_name p1;
              old_count = List.length ps1;
              new_count = List.length ps2;
            }
        else
          let kept = preserved_count ps1 ps2 in
          VeryDifferent
            {
              reason =
                Printf.sprintf
                  "WFQ arms changed (%d/%d arms preserved; weights %s)" kept
                  (List.length ps1)
                  (if weights_changed ~eps:weight_eps ws1 ws2 then
                     "also changed"
                   else "unchanged");
            }
    (* Check if one policy is nested in the other *)
    | _ when is_subpolicy p1 p2 ->
        SuperPol
          {
            outer_policy = policy_type_name p2;
            inner_policy = policy_type_name p1;
          }
    (* Completely different kinds or incompatible structures *)
    | _ ->
        VeryDifferent
          {
            reason =
              Printf.sprintf "%s vs %s" (policy_type_name p1)
                (policy_type_name p2);
          }

(* Format the diff for display *)
let to_string = function
  | Same -> "Programs are equivalent"
  | ArmAdded { policy_type; old_count; new_count } ->
      Printf.sprintf "Arm added to %s: %d → %d children" policy_type old_count
        new_count
  | WeightsChanged { old_weights; new_weights } ->
      let fmt_weights ws =
        ws |> List.map (Printf.sprintf "%.9g") |> String.concat ", "
      in
      Printf.sprintf "Weights changed: [%s] → [%s]" (fmt_weights old_weights)
        (fmt_weights new_weights)
  | SuperPol { outer_policy; inner_policy } ->
      Printf.sprintf "Nested: %s contains %s as sub-policy" outer_policy
        inner_policy
  | VeryDifferent { reason } -> Printf.sprintf "Very different: %s" reason

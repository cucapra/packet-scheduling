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
    | _ -> false

(* Get a simple string representation of a policy type *)
let policy_type_name = function
  | Frontend.Policy.FIFO _ -> "FIFO"
  | Frontend.Policy.EDF _ -> "EDF"
  | Frontend.Policy.Strict _ -> "Strict"
  | Frontend.Policy.RR _ -> "RoundRobin"
  | Frontend.Policy.WFQ _ -> "WeightedFair"

(* Analyze the difference between two policies *)
let analyze p1 p2 : t =
  if p1 = p2 then Same
  else
    match (p1, p2) with
    (* Same policy type with different number of children *)
    | Frontend.Policy.Strict ps1, Frontend.Policy.Strict ps2
    | Frontend.Policy.RR ps1, Frontend.Policy.RR ps2 ->
        if List.length ps1 <> List.length ps2 then
          ArmAdded
            {
              policy_type = policy_type_name p1;
              old_count = List.length ps1;
              new_count = List.length ps2;
            }
        else
          (* Same count but different children - dig deeper *)
          VeryDifferent
            {
              reason =
                Printf.sprintf "Different %s structure" (policy_type_name p1);
            }
    (* WFQ with different weights *)
    | Frontend.Policy.WFQ (ps1, ws1), Frontend.Policy.WFQ (ps2, ws2) ->
        if ps1 = ps2 && ws1 <> ws2 then
          WeightsChanged { old_weights = ws1; new_weights = ws2 }
        else if ps1 <> ps2 then VeryDifferent { reason = "WFQ arms changed" }
        else Same
    (* Check if one policy is nested in the other *)
    | _ when is_subpolicy p1 p2 ->
        SuperPol
          {
            outer_policy = policy_type_name p2;
            inner_policy = policy_type_name p1;
          }
    (* Completely different *)
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
        ws |> List.map (Printf.sprintf "%.0f") |> String.concat ", "
      in
      Printf.sprintf "Weights changed: [%s] → [%s]" (fmt_weights old_weights)
        (fmt_weights new_weights)
  | SuperPol { outer_policy; inner_policy } ->
      Printf.sprintf "Nested: %s contains %s as sub-policy" outer_policy
        inner_policy
  | VeryDifferent { reason } -> Printf.sprintf "Very different: %s" reason

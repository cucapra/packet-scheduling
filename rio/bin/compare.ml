open Frontend

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

(* Get a simple string representation of a policy type *)
let policy_type_name = function
  | Frontend.Policy.FIFO _ -> "FIFO"
  | Frontend.Policy.EDF _ -> "EDF"
  | Frontend.Policy.Strict _ -> "Strict"
  | Frontend.Policy.RR _ -> "RoundRobin"
  | Frontend.Policy.WFQ _ -> "WeightedFair"

let analyze p1 p2 : t =
  if Json.equiv_policy p1 p2 then Same
  else VeryDifferent { reason = "Policies are not equivalent" }

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

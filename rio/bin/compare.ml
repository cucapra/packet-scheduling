open Frontend

(* ADT describing differences between two policies *)
type t =
  | Same
  | ArmsAdded of {
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

let subset lst1 lst2 = List.for_all (fun x -> List.mem x lst2) lst1

(* Analyze differences between two policies *)

let analyze p1 p2 : t =
  if Json.equiv_policy p1 p2 then Same
  else
    (* We'd like to report an added arm. We can assume that the programs are normalized. So RR(A,B) and RR(A,B,C) would indicate an added arm. But RR(A,B) and RR(C,D,E) would indicate VeryDifferent. Similar logic applies to WFQ. *)
    match (p1, p2) with
    | Frontend.Policy.RR arms1, Frontend.Policy.RR arms2 ->
        (* use subset *)
        let len1 = List.length arms1 in
        let len2 = List.length arms2 in
        if len2 > len1 && subset arms1 arms2 then
          ArmsAdded
            { policy_type = "RoundRobin"; old_count = len1; new_count = len2 }
        else VeryDifferent { reason = "Policies are not equivalent" }
    | Frontend.Policy.WFQ (arms1, _), Frontend.Policy.WFQ (arms2, _) ->
        let len1 = List.length arms1 in
        let len2 = List.length arms2 in
        if len2 > len1 && subset arms1 arms2 then
          ArmsAdded
            { policy_type = "WeightedFair"; old_count = len1; new_count = len2 }
        else VeryDifferent { reason = "Policies are not equivalent" }
    | _, _ -> VeryDifferent { reason = "Policies are not equivalent" }

(* Format the diff for display *)
let to_string = function
  | Same -> "Programs are equivalent"
  | ArmsAdded { policy_type; old_count; new_count } ->
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

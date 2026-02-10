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
  | SuperPol (* Make more sophisticated. *)
  | VeryDifferent

(* Get a simple string representation of a policy type *)
let policy_type_name = function
  | Frontend.Policy.FIFO _ -> "FIFO"
  | Frontend.Policy.EDF _ -> "EDF"
  | Frontend.Policy.Strict _ -> "Strict"
  | Frontend.Policy.RR _ -> "RoundRobin"
  | Frontend.Policy.WFQ _ -> "WeightedFair"

let subset lst1 lst2 = List.for_all (fun x -> List.mem x lst2) lst1

let is_sub_policy p1 p2 =
  (* Is p1 a sub-policy of p2? Examples:
      - RR(A, B) is NOT sub-policy of RR(A, B, C); that should be ArmsAdded
      - WFQ(A, B) is NOT a sub-policy of WFQ(A, B, C), for the same reason
      - RR(A, B) is a sub-policy of SP(RR(A, B), C)
      - RR(A, B) is a sub-policy of SP(RR(RR(A, B),C), D)
      *)
  let rec helper p2 =
    if p1 = p2 then true
      (* This feels a little silly but it actually exists for the recursive case,
          once we have drilled into p2. *)
    else
      match p2 with
      | Frontend.Policy.FIFO _ | Frontend.Policy.EDF _ -> false
      | Frontend.Policy.Strict arms2 -> List.exists helper arms2
      | Frontend.Policy.RR arms2 -> List.exists helper arms2
      | Frontend.Policy.WFQ (arms2, _) -> List.exists helper arms2
  in
  helper p2

let analyze p1 p2 : t =
  (* Analyze differences between two policies *)
  if p1 = p2 then Same
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
        else VeryDifferent
    | ( Frontend.Policy.WFQ (arms1, weights1),
        Frontend.Policy.WFQ (arms2, weights2) ) ->
        let len1 = List.length arms1 in
        let len2 = List.length arms2 in
        if len2 > len1 && subset arms1 arms2 then
          ArmsAdded
            { policy_type = "WeightedFair"; old_count = len1; new_count = len2 }
        else if arms1 = arms2 && weights1 <> weights2 then
          WeightsChanged { old_weights = weights1; new_weights = weights2 }
        else VeryDifferent
    | _, _ ->
        (* Either sub-pol or give up with VeryDifferent *)
        if is_sub_policy p1 p2 then SuperPol else VeryDifferent

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
  | SuperPol -> Printf.sprintf "Nested policy"
  | VeryDifferent -> Printf.sprintf "Very different"

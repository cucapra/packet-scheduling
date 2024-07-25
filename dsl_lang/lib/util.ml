exception NonTerminal
exception ParserError of string

(* Helper function to compactly join policy lists by comma *)
let rec join = List.fold_left (fun acc s ->
  let parsed = string_of_policy s in
  if acc = "" then parsed
  else (acc ^ ", " ^ parsed)) ""

(* Helper function to compactly join weighted policy lists by comma *)
and join_weighted = List.fold_left (fun acc s ->
  let parsed = "(" ^ string_of_policy (fst s) ^ ", " ^ string_of_int (snd s) ^ ")" in
  if acc = "" then parsed
  else (acc ^ ", " ^ parsed)) ""

(** Takes a policy and returns the string representation of it. **)
and string_of_policy (pol : Ast.policy) : string =
  match pol with
  | Class c -> c
  | Fifo lst -> "fifo[" ^ join lst ^ "]"
  | RoundRobin lst -> "rr[" ^ join lst ^ "]"
  | Strict lst -> "strict[" ^ join lst ^ "]"
  | WeightedFair lst -> "strict[" ^ join_weighted lst ^ "]"

  | EarliestDeadline lst -> "edf[" ^ join lst ^ "]"
  | ShortestJobNext lst -> "sjn[" ^ join lst ^ "]"
  | ShortestRemaining lst -> "srtf[" ^ join lst ^ "]"
  | RateControlled lst -> "rcsp[" ^ join lst ^ "]"

  | LeakyBucket (lst, weight, buffer) ->
      "leaky[[" ^ join list ^ "], weight = "
      ^ string_of_int weight ^ ", buffer = "
      ^ string_of_int buffer ^ "]"

  | TokenBucket (lst, weight, buffer) ->
      "token[[" ^ join list ^ "], weight = "
      ^ string_of_int weight ^ ", buffer = "
      ^ string_of_int buffer ^ "]"

  | StopAndGo (lst, weight) -> 
      "stopandgo[[" ^ join lst ^ "], weight = "
      ^ string_of_int weight ^ "]"

  | _ -> "error"

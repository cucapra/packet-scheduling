exception NonTerminal
exception ParserError of string

(** Takes a policy and returns the string representation of it. **)
let rec string_of_policy (pol : Ast.policy) : string =
  
  (* Helper function to compactly join policy lists by comma *)
  let join = List.fold_left (fun acc s ->
    let parsed = string_of_policy s in
    if acc = "" then parsed
    else (acc ^ ", " ^ parsed)) "" in

  (* Helper function to compactly join weighted policy lists by comma *)
  let join_weighted = List.fold_left (fun acc s ->
    let parsed = "(" ^ string_of_policy (fst s) ^ ", " ^ string_of_int (snd s) ^ ")" in
    if acc = "" then parsed
    else (acc ^ ", " ^ parsed)) "" in

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

  | LeakyBucket (lst, width, buffer) ->
      "leaky[[" ^ join lst ^ "], width = "
      ^ string_of_int width ^ ", buffer = "
      ^ string_of_int buffer ^ "]"

  | TokenBucket (lst, width, buffer) ->
      "token[[" ^ join lst ^ "], width = "
      ^ string_of_int width ^ ", time = "
      ^ string_of_int buffer ^ "]"

  | StopAndGo (lst, width) -> 
      "stopandgo[[" ^ join lst ^ "], width = "
      ^ string_of_int width ^ "]"

  | _ -> "error"

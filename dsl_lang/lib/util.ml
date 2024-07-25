exception NonTerminal
exception ParserError of string

(** Helper function to turn policy lists into strings. **)
let rec string_of_plist (plist : Ast.policy list) : string =
  match plist with
  | [] -> ""
  | [x] -> string_of_policy x
  | h :: t -> string_of_policy h ^ ", " ^ string_of_plist t

(** Takes a policy and returns the string representation of it. **)
and string_of_policy (pol : Ast.policy) : string =
  match pol with
  | Class c -> c
  | Fifo [x] -> "fifo[" ^ string_of_policy x ^ "]"
  | Strict [x] -> "strict[" ^ string_of_policy x ^ "]"
  | RoundRobin [x] -> "rr[" ^ string_of_policy x ^ "]"
  | Fifo (h :: t) ->
      "fifo[" ^ string_of_policy h ^ ", " ^ string_of_plist t ^ "]"
  | Strict (h :: t) ->
      "strict[" ^ string_of_policy h ^ ", " ^ string_of_plist t ^ "]"
  | RoundRobin (h :: t) ->
      "rr[" ^ string_of_policy h ^ ", " ^ string_of_plist t ^ "]"
  | _ -> "error"

exception NonTerminal
exception ParserError of string

(** Helper function to turn policy lists into strings. **)
let rec string_of_plist (plist : Ast.policy list) : string =
  match plist with
  | [] -> ""
  | [x] -> string_of_policy x
  | h :: t -> string_of_policy h ^ ", " ^ string_of_plist t

(** Takes a policy and returns the string representation of it. **)
and
string_of_policy (pol : Ast.policy) : string =
  match pol with
  | Class c -> c
  | Fifo pl -> "fifo[" ^ string_of_plist pl ^ "]"
  | Strict pl -> "strict[" ^ string_of_plist pl ^ "]"
  | RoundRobin pl -> "rr[" ^ string_of_plist pl ^ "]"
  | Var v -> v

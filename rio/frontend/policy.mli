(* Changes to this type must also be reflected in `Ast.policy` in ast.ml *)

type set =
  | Class of Ast.clss
  | Union of set list

type t =
  | Fifo of set
  | EarliestDeadline of set
  | ShortestJobNext of set
  | ShortestRemaining of set
  | RoundRobin of t list
  | Strict of t list
  | WeightedFair of t list * int list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

val of_program : Ast.program -> t
val to_string : t -> string

(* Changes to this type must also be reflected in `Ast.policy` in ast.ml *)
type t =
  | Class of Ast.clss
  | Fifo of t list
  | RoundRobin of t list
  | Strict of t list
  | WeightedFair of (t * float) list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss

val of_program : Ast.program -> t
val to_string : t -> string

type t =
  | Fifo of Ast.clss list
  | EDF of Ast.clss list
  | RoundRobin of t list
  | Strict of t list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

val of_program : Ast.program -> t
val to_string : t -> string

type t =
  | FIFO of Ast.clss list
  | EDF of Ast.clss list
  | RR of t list
  | Strict of t list
  | WFQ of t list * float list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

val of_program : Ast.program -> t
val to_string : t -> string

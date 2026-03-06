type t =
  | FIFO of Ast.clss
  | Union of t list
  | EDF of t
  | Strict of t list
  | RR of t list
  | WFQ of t list * float list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

val of_program : Ast.program -> t
val to_string : t -> string
val to_json : t -> Yojson.Basic.t

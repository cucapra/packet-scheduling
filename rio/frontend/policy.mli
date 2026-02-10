type t =
  | FIFO of Ast.clss list
  | EDF of Ast.clss list
  | Strict of t list
  | RR of t list
  | WFQ of t list * float list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

val of_program : Ast.program -> t
val to_string : t -> string
val equiv : t -> t -> bool
val to_normalized_json : t -> Yojson.Basic.t

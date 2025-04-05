type set2stream = EDF | FIFO
type stream2stream = RR | Strict

type t =
  | Leaf of (set2stream * Ast.clss list)
  | Node of (stream2stream * t list)

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

val of_program : Ast.program -> t
val to_string : t -> string
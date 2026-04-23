type t =
  | FIFO of Ast.clss
  | UNION of t list
  | SP of t list
  | RR of t list
  | WFQ of t list * float list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

val of_program : Ast.program -> t
(** Resolve a parsed program (declarations + assignments + return) into a single
    [t]. Variables are substituted in by following assignments; declared classes
    that never appear in the returned policy are silently dropped, per the DSL
    semantics. *)

val to_string : t -> string
val to_json : t -> Yojson.Basic.t

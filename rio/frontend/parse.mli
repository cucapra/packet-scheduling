exception ParserError of Rio_core.Ast.error_info
exception FileNotFound of string

val parse_string : string -> Rio_core.Ast.program
val parse_file : string -> Rio_core.Ast.program

val parse_policy_string : string -> Rio_core.Ast.stream
(** Parse a bare policy expression (as it would appear on the right-hand side of
    a [return]). Used by the REPL so users can type [fifo[A]] without the
    surrounding [classes] / [return] scaffolding. *)

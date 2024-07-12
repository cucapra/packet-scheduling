
(* The type of tokens. *)

type token = 
  | TRANSIENT
  | STRICT
  | RBRACE
  | LBRACE
  | FIFO
  | FAIR
  | EOF
  | COMMA
  | CLSS of (string)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.policy)

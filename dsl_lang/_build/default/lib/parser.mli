
(* The type of tokens. *)

type token = 
  | WHDREST
  | WEMPTY
  | VAR of (string)
  | TRUE
  | TNAME of (string)
  | THEN
  | STAR
  | RPAREN
  | PLUS
  | OR
  | NOT
  | MINUS
  | MATCH
  | LPAREN
  | LIST
  | LET
  | LESS
  | LAMBDA
  | INT of (int)
  | IN
  | IF
  | GREATER
  | FIX
  | FALSE
  | EQUALS
  | EOF
  | EMPTY
  | ELSE
  | DOT
  | CONS
  | COMMA
  | COLON
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.exp)

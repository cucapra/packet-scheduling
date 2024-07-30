
(* The type of tokens. *)

type token = 
  | WIDTH
  | WFQ
  | VAR of (string)
  | TOKEN
  | TIME
  | STRICT
  | STOPGO
  | SRTF
  | SJN
  | SEMICOLON
  | RR
  | RPAREN
  | RETURN
  | RCSP
  | RBRACKET
  | LPAREN
  | LEAKY
  | LBRACKET
  | INT of (int)
  | FIFO
  | EQUALS
  | EOF
  | EDF
  | COMMA
  | CLSS of (string)
  | CLASSES
  | BUFFER

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)

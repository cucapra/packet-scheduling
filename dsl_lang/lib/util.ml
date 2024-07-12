(* open Ast *)

exception NonTerminal
exception ParserError of string

(** syntax_error_msg lexbuf is a syntax error message for the current position **)
let syntax_error_msg lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    let lnum, cnum = pos.pos_lnum, (pos.pos_cnum - pos.pos_bol) in
    Printf.sprintf "Syntax error at line %d, character %d" lnum cnum

let parse lexbuf = Parser.prog Lexer.token lexbuf

(** parse s parses a program string into an AST **)
let parse_string (s : string) =
  let lexbuf = Lexing.from_string s in 
  try parse lexbuf with
    | Parser.Error -> raise (ParserError (syntax_error_msg lexbuf))

(** parse s parses a program file into an AST **)
let parse_file (f : string) =
  let lexbuf = Lexing.from_channel (open_in f) in 
  try parse lexbuf with
    | Parser.Error -> prerr_endline (syntax_error_msg lexbuf); exit 1
    | Lexer.Err -> prerr_endline (syntax_error_msg lexbuf); exit 1
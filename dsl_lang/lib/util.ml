open Ast

exception NonTerminal
exception ParserError of string
exception FormatError of string

(** syntax_error_msg lexbuf is a syntax error message for the current position **)
let syntax_error_msg lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    let lnum, cnum = pos.pos_lnum, (pos.pos_cnum - pos.pos_bol) in
    Printf.sprintf "Syntax error at line %d, character %d" lnum cnum

(* Check that a program contains exactly one return statement as its final component *)
let rec validate_seq acc = function
| RtnComp(pol) :: [] -> acc, pol
| RtnComp(_) :: _ ->
    raise(FormatError "Program must contain exactly one return statement as its final component.")
| h :: t -> validate_seq (acc @ [h]) t
| [] -> raise (FormatError "Program must begin with a declaration of classes.")

(* Check that a program's assignment list does not contain declarations *)
let check_assn_list acc = function
| AssnComp(var, pol) -> (var, pol) :: acc
| _ -> raise(FormatError "Cannot interleave declarations and assignments.")

(* Validate a program sequence as a valid program *)
let validate_program seq = match (validate_seq [] seq) with
| DeclareComp(classes) :: t, rtn ->
    Prog (classes, (List.fold_left check_assn_list [] t), rtn)
| _ -> raise(FormatError "Program must begin with a declaration of classes.")

let parse lexbuf = validate_program (Parser.prog Lexer.token lexbuf)

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
exception ParserError of Ast.error_info
exception FileNotFound of string

(* `syntax_error_msg lexbuf` is the "information" for a syntax error at the
    current position *)
let syntax_error_msg lexbuf : Ast.error_info =
  let pos = Lexing.lexeme_start_p lexbuf in
  let row, col, char =
    (Some pos.pos_lnum, Some (pos.pos_cnum - pos.pos_bol), None)
  in
  { row; col; char }

let parse lexbuf = Parser.prog Lexer.token lexbuf

(* `parse s` is the AST for program string `s` *)
let parse_string (s : string) =
  let lexbuf = Lexing.from_string s in
  try parse lexbuf
  with Parser.Error -> raise (ParserError (syntax_error_msg lexbuf))

(* `parse s` is the AST for program file with name `s` *)
let parse_file (f : string) =
  let lexbuf =
    try Lexing.from_channel (open_in f)
    with Sys_error _ -> raise (FileNotFound f)
  in
  try parse lexbuf
  with Parser.Error -> raise (ParserError (syntax_error_msg lexbuf))

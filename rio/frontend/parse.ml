exception ParserError of Rio_core.Ast.error_info
exception FileNotFound of string

(* `syntax_error_msg lexbuf` is the "information" for a syntax error at the 
    current position *)
let syntax_error_msg lexbuf : Rio_core.Ast.error_info =
  let pos = Lexing.lexeme_start_p lexbuf in
  let row, col, char =
    (Some pos.pos_lnum, Some (pos.pos_cnum - pos.pos_bol), None)
  in
  { row; col; char }

let parse lexbuf = Parser.prog Lexer.token lexbuf

(* Run [k] against [lexbuf] and surface both parser and lexer failures as
   [ParserError]. Lex errors carry their own position info from the lexer; for
   parser errors we read the position out of the lexbuf at the failure site. *)
let run_or_error lexbuf k =
  try k lexbuf with
  | Parser.Error -> raise (ParserError (syntax_error_msg lexbuf))
  | Lexer.LexerError info -> raise (ParserError info)

(* `parse s` is the AST for program string `s` *)
let parse_string (s : string) =
  let lexbuf = Lexing.from_string s in
  run_or_error lexbuf parse

(* `parse s` is the AST for program file with name `s` *)
let parse_file (f : string) =
  let lexbuf =
    try Lexing.from_channel (open_in f)
    with Sys_error _ -> raise (FileNotFound f)
  in
  run_or_error lexbuf parse

let parse_policy_string (s : string) =
  let lexbuf = Lexing.from_string s in
  run_or_error lexbuf (fun lb -> Parser.policy_only Lexer.token lb)

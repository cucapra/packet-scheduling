{
   open Parser
   open Printf
   exception Eof
   exception Err
}


let clss = ['a'-'z' 'A'-'Z']*
let whitespace = [' ' '\t']+
(* let id = ['a'-'z'] ['a'-'z' '0'-'9']* *)
let bigid = ['A'-'Z'] ['a'-'z' '0'-'9']*
let newline = ['\n']*


rule token = parse
| whitespace        { token lexbuf}
| newline       { Lexing.new_line lexbuf; token lexbuf }
(* | "="       { EQUALS } *)
| "["       { LBRACE }
| "]"       { RBRACE }
(* | "return"  { RETURN }
| "classes" { CLASSES } *)
| ","       { COMMA }
| "fifo"    { FIFO }
| "fair"    { FAIR }
| "strict"  { STRICT }
| "transient"   { TRANSIENT }
(* | id as v   { VAR(v) } *)
| bigid as i    { CLSS(i) }
| eof       { EOF }


| _ as c {
           let pos = lexbuf.Lexing.lex_curr_p in
           printf "Error at line %d\n" pos.Lexing.pos_lnum;
           printf "Unrecognized character: [%c]\n" c;
           exit 1
       }

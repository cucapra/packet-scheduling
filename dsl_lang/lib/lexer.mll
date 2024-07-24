{
   open Parser
   open Printf
   exception Eof
   exception Err
}

let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let id = ['a'-'z'] ['a'-'z' '0'-'9' '_']*
let bigid = ['A'-'Z']*
let newline = ['\n']*
let comment = ['/' '/'] ['\x00' - '\x09']* ['\x0b' - '\x80']*


rule token = parse
| whitespace        { token lexbuf}
| newline       { Lexing.new_line lexbuf; token lexbuf }
| comment      { token lexbuf }
| "="       { EQUALS }
| "["       { LBRACE }
| "]"       { RBRACE }
| "("       { LPAREN }
| ")"       { RPAREN }
| "return"  { RETURN }
| "classes" { CLASSES }
| ","       { COMMA }
| "fifo"    { FIFO }
| "rr"      { FAIR }
| "strict"  { STRICT }
| "wfq"     { WFQ }
| "edf"     { EDF }
| "sjn"     { SJN }
| "srtf"    { SRTF }
| ";"       { SEMICOLON }
| id as v   { VAR(v) }
| bigid as i    { CLSS(i) }
| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
| eof       { EOF }


| _ as c {
           let pos = lexbuf.Lexing.lex_curr_p in
           printf "Error at line %d\n" pos.Lexing.pos_lnum;
           printf "Unrecognized character: [%c]\n" c;
           exit 1
       }

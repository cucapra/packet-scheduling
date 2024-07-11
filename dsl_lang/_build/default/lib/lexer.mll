{
open Parser
open Printf
exception Eof
exception Err of string
}

let num = ['-']?['0'-'9']+
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let bigid = ['A'-'Z'] ['a'-'z' '0'-'9']*
let ws = [' ' '\t']

rule token = parse
| ws               { token lexbuf }
| '\n'             { Lexing.new_line lexbuf; token lexbuf }
| "("              { LPAREN }
| ")"              { RPAREN }
| "."              { DOT }
| ":"              { COLON }
| "->"             { ARROW }
| "lambda"         { LAMBDA }
| "let"            { LET }
| "="              { EQUALS }
| "in"             { IN }
| "+"              { PLUS }
| "-"              { MINUS }
| "<"              { LESS }
| ">"              { GREATER }
| ","              { COMMA }
| "*"              { STAR }
| "List"           { LIST }
| "fix"            { FIX }
| "true"           { TRUE }
| "false"          { FALSE }
| "if"             { IF }
| "then"           { THEN }
| "else"           { ELSE }
| "and"            { AND }
| "or"             { OR }
| "not"            { NOT }
| "::"             { CONS }
| "empty"          { EMPTY }
| "match"          { MATCH }
| "with_empty"     { WEMPTY }
| "with_head_rest" { WHDREST }
| id as v          { VAR(v) }
| bigid as i       { TNAME(i) }
| num as n         { INT(int_of_string n) }
| eof              { EOF }

| _ as c  {
            let pos = lexbuf.Lexing.lex_curr_p in
            printf "Error at line %d\n" pos.Lexing.pos_lnum;
            printf "Unrecognized character: [%c]\n" c;
            exit 1
          }

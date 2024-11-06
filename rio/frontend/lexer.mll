{
   open Parser
}

let whitespace = [' ' '\t']+
let int = '-'? ['0'-'9']+
let float = '-'? (['0'-'9']* '.')? ['0'-'9']+
let id = ['a'-'z'] ['a'-'z' '0'-'9' '_']*
let bigid = ['A'-'Z']*
let comment = ['/' '/'] ['\x00' - '\x09']* ['\x0b' - '\x80']*

rule token = parse
| whitespace            { token lexbuf}
| "\n"                  { Lexing.new_line lexbuf; token lexbuf }
| comment               { token lexbuf }
| "="                   { EQUALS }
| "["                   { LBRACKET }
| "]"                   { RBRACKET }
| "("                   { LPAREN }
| ")"                   { RPAREN }
| "return"              { RETURN }
| "classes"             { CLASSES }
| "union"               { UNION }
| "width"               { WIDTH }
| "buffer"              { BUFFER }
| "time"                { TIME }

| ","                   { COMMA }
| "fifo"                { FIFO }
| "rr"                  { RR }
| "strict"              { STRICT }
| "wfq"                 { WFQ }
| "edf"                 { EDF }
| "sjn"                 { SJN }
| "srtf"                { SRTF }
| "rcsp"                { RCSP }
| "leakybucket"         { LEAKY }
| "tokenbucket"         { TOKEN }
| "stopandgo"           { STOPGO }
| ";"                   { SEMICOLON }
| id as v               { VAR(v) }
| bigid as i            { CLSS(i) }
| int                   { INT (int_of_string (Lexing.lexeme lexbuf)) }
| eof                   { EOF }


| _ as c {
            let pos = lexbuf.Lexing.lex_curr_p in
            Printf.printf "Error at line %d\n" pos.Lexing.pos_lnum;
            Printf.printf "Unrecognized character: [%c]\n" c;
            exit 1
         }

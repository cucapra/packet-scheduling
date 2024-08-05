exception ParserError of string

val parse_string : string -> Ast.program
val parse_file : string -> Ast.program

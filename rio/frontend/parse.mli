exception ParserError of Ast.error_info

val parse_string : string -> Ast.program
val parse_file : string -> Ast.program

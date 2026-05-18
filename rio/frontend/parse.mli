exception ParserError of Rio_core.Ast.error_info

val parse_string : string -> Rio_core.Ast.program
val parse_file : string -> Rio_core.Ast.program

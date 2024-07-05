%{
    open Ast

    let list_expr ls =
    match ls with
    | [l] -> l 
    | _ -> List ls (* should be something from my ast, not List? *)
%}

%token <string> VAR
%token LPARENS RPAREN EQUALS LBRACE RBRACE RETURN CLASS 
COMMA EOF

%type <policy> policy
%type <exp> expression
%type <Ast.exp> program

%start program

%%
program: exp EOF


pexp:
    | RETURN plist                             { list_expr $1 }

plist:
    | LBRACE policy COMMA plist RBRACE         { $1::$3 }
    | policy                                   { [$1] }

policy:
    | CLASS {Class $1}
    | {Fifo}
    | {Fair}
    | {Strict}
    | {Transient}



%{
    open Ast

    let list_expr ls =
    match ls with
    | [l] -> l 
    | _ -> Policy ls (* what to put for typ?? *)
%}

%token <string> VAR
%token LPARENS RPAREN EQUALS LBRACE RBRACE RETURN CLASSES 
COMMA EOF FIFO FAIR STRICT TRANSIENT

%type <policy> policy
%type <exp> expression
%type <Ast.exp> program

%start program

%%
program: exp EOF                               { $1 }

exp:
    | VAR EQUALS pexp                          { Assn($1, $3) }
    | RETURN policy                            { Return($2) } //unsure
    | pexp                                     { $1 }

pexp:
    | plist                                    { list_expr $1 }

plist:
    | policy COMMA plist                       { $1::$3 } //put lbrace and rbrace?
    | policy                                   { [$1] }

policy:
    | CLASS                                    { Class }
    | FIFO plist                               { Fifo($1) }
    | FAIR plist                               { Fair($1) }
    | STRICT plist                             { Strict($1) }
    | TRANSIENT plist                          { Transient($1) }



%{
    open Ast

    let list_expr ls =
    match ls with
    | [l] -> l 
    | _ -> Transient ls (* what to put for typ?? *)

%}

// %token <string> VAR 
%token <string> CLSS
// %token EQUALS
%token LBRACE
%token RBRACE
// %token RETURN 
// %token CLASSES 
%token COMMA
%token EOF 
%token FIFO 
%token FAIR 
%token STRICT 
%token TRANSIENT
// %token NEWLINE

%type <policy> policy
%type <Ast.policy> prog

%start prog

%%
prog: policy EOF                               { $1 }

policy:
    | CLSS                                     { Class($1) }
    | pexp                                     { $1 }

pexp:
    | plist                                    { list_expr $1 }

plist:
    | policy COMMA plist                       { $1::$3 }
    | lexp                                   { [$1] }

lexp:
    | FIFO LBRACE plist RBRACE                 { Fifo($3) }
    | FAIR LBRACE plist RBRACE                 { Fair($3) }
    | STRICT LBRACE plist RBRACE               { Strict($3) }
    | TRANSIENT LBRACE plist RBRACE            { Transient($3) }




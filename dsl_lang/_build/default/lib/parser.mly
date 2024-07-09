%{
    open Ast

    let list_expr ls =
    match ls with
    | [l] -> l 
    | _ -> Policy (Transient, ls) (* what to put for typ?? *)

%}

%token <string> VAR 
%token <string> CLSS
%token EQUALS
%token LBRACE
%token RBRACE
%token RETURN 
%token CLASSES 
%token COMMA
%token EOF 
%token FIFO 
%token FAIR 
%token STRICT 
%token TRANSIENT

%type <policy> policy
// %type <exp> expression
%type <Ast.statement> program

%start program

%%
program: statement EOF                               { $1 }

statement:
    | VAR EQUALS pexp                          { Assn($1, $3) }
    | RETURN policy                            { Return($2) } //unsure
    | CLASSES clist                             { $1 }
    | pexp                                     { $1 }

pexp:
    | plist                                    { list_expr $1 }

plist:
    | policy COMMA plist                       { $1::$3 } //put lbrace and rbrace?
    | policy                                   { [$1] }

policy:
    | CLSS                                     { Class($1) }
    | FIFO LBRACE plist RBRACE                 { Fifo, $3 }
    | FAIR LBRACE plist RBRACE                 { Fair, $3 }
    | STRICT LBRACE plist RBRACE               { Strict, $3 }
    | TRANSIENT LBRACE plist RBRACE            { Transient, $3 }
    | clist                                    { Classes($1) }

clist:
    | CLSS COMMA clist                         { $1::$3 }
    | CLSS                                     { [$1] }



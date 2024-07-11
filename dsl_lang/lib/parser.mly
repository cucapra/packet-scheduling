%{
    open Ast
    open Lexing

    let list_expr ls =
    match ls with
    | [l] -> l 
    | _ -> Transient ls (* what to put for typ?? *)

    let class_expr cs = 
    match cs with
    | [c] -> c 
    | _ -> DeclareClasses cs

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
%token NEWLINE

%type <policy> policy
// %type <exp> expression
%type <Ast.program> prog

%start prog

%%
program: prog EOF                               { $1 }

prog:
    | statement NEWLINE statement              { Prog($1, $3) }
    | statement

statement:
    | VAR EQUALS pexp                          { Assn($1, $3) }
    | RETURN pexp                              { Return($2) }
    | CLASSES cexp                             { DeclareClasses($1) }
    | pexp                                     { $1 }

pexp:
    | plist                                    { list_expr $1 }

plist:
    | policy COMMA plist                       { $1::$3 } //put lbrace and rbrace?
    | policy                                   { [$1] }

policy:
    | CLSS                                     { Class($1) }
    | FIFO LBRACE clist RBRACE                 { Fifo, $3 }
    | FAIR LBRACE plist RBRACE                 { Fair, $3 }
    | STRICT LBRACE plist RBRACE               { Strict, $3 }
    | TRANSIENT LBRACE plist RBRACE            { Transient, $3 }
    | cexp                                     { $1 }
cexp:
    | clist                                    { class_expr $1 }

clist:
    | CLSS COMMA clist                         { $1::$3 }
    | CLSS                                     { [$1] }



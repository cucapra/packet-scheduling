%{
    open Ast

    let list_expr ls =
    match ls with
    | [l] -> l 
    | _ -> Policy Transient ls (* what to put for typ?? *)

    let class_expr ls =
    match ls with
    | [l] -> l 
    | _ -> Classes ls
%}

%token <string> VAR CLSS
%token EQUALS LBRACE RBRACE RETURN CLASSES 
COMMA EOF FIFO FAIR STRICT TRANSIENT

%type <policy> policy
// %type <exp> expression
%type <Ast.exp> program

%start program

%%
program: exp EOF                               { $1 }

exp:
    | VAR EQUALS pexp                          { Assn($1, $3) }
    | RETURN policy                            { Return($2) } //unsure
    | CLASSES clist                            { class_expr $1}
    | pexp                                     { $1 }

clist:
    | CLSS COMMA clist                         { $1::$3 }
    | CLSS                                     {[$1]}

pexp:
    | plist                                    { list_expr $1 }

plist:
    | policy COMMA plist                       { $1::$3 } //put lbrace and rbrace?
    | policy                                   { [$1] }

policy:
    | CLSS                                     { Class } // what is argument
    | FIFO LBRACE plist RBRACE                 { Policy(Fifo, $3) } // put braces here?
    | FAIR LBRACE plist RBRACE                 { Policy(Fair, $3) }
    | STRICT LBRACE plist RBRACE               { Policy(Strict, $3) }
    | TRANSIENT LBRACE plist RBRACE            { Policy(Transient, $3) }



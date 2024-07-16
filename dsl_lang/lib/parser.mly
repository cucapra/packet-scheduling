%{
    open Ast

    let fifo_list ls =
    match ls with
    | [l] -> l 
    | _ -> Fifo ls 

    let rr_list ls =
    match ls with
    | [l] -> l 
    | _ -> Fair ls

    let strict_list ls =
    match ls with
    | [l] -> l 
    | _ -> Strict ls

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
%token SEMICOLON

%type <policy> policy
%type <Ast.policy> prog

%start prog

%%
// dc:
//     | CLASSES clist     { class_list $2}

// clist:
//     | exp COMMA clist { $1::$3}
//     | exp   { [$1] }

policy:    
    | FIFO LBRACE arglist RBRACE               { fifo_list $3 }
    | STRICT LBRACE arglist RBRACE             { strict_list $3 }
    | FAIR LBRACE arglist RBRACE               { rr_list $3 }
    | CLSS                                     { Class($1) }
    | VAR EQUALS policy                        { Assn($1, $3) }
    | RETURN policy                            { Return($2)}
    | policy SEMICOLON policy       {Seq($1, $3)}


arglist:
    | exp COMMA arglist                        { $1::$3 }
    | exp                                      { [$1] }

exp:
    | policy                                   { $1 }

prog: policy EOF                               { $1 }

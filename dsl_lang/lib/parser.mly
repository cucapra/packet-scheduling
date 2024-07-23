%{
    open Ast
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
%type <Ast.program> prog

%start prog

%%

/* Policies */
policy:
    | FIFO LBRACE; pl = arglist; RBRACE             { Fifo pl }
    | STRICT LBRACE; pl = arglist; RBRACE           { Strict pl }
    | FAIR LBRACE; pl = arglist; RBRACE             { Fair pl }
    | CLSS                                          { Class($1) }
    | VAR                                           { Var($1) }
arglist:
    | pl = separated_list(COMMA, policy)            { pl } ;

/* Declarations */
declare:
    | CLASSES; vl  = list_fields; SEMICOLON         { DeclareClasses vl }
list_fields:
    vl = separated_list(COMMA, CLSS)                { vl } ;

/* Return */
return:
    | RETURN policy                                 { Return($2) }

/* Assignment List */
assignments:
    l = list(assignment)                            { l }
assignment:
    | VAR EQUALS policy SEMICOLON                   { Assn($1, $3) }

/* Program */
prog: declare assignments return EOF                { Prog($1, $2, $3) }

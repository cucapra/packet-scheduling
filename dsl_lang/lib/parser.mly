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
%type <Ast.statement> prog

%start prog

%%

/* Policies */
policy:
    | FIFO LBRACE; pl = arglist; RBRACE               { Fifo pl }
    | STRICT LBRACE; pl = arglist; RBRACE             { Strict pl }
    | FAIR LBRACE; pl = arglist; RBRACE               { Fair pl }
    | CLSS                                            { Class($1) }
    | VAR                                             { Var($1) }

arglist:
    | pl = separated_list(COMMA, policy)              { pl } ;

/* Statements */
state:
    | statem SEMICOLON state                           { Seq($1, $3) }
    | statem                                           { $1 }

statem:
    | RETURN policy                         { Return($2) }
    | CLASSES; vl  = list_fields            { Declare(DeclareClasses vl) }

list_fields:
    vl = separated_list(COMMA, CLSS)          { vl } ;

/* Program */
prog: state EOF                               { $1 }

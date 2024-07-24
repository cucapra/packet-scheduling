%{
    open Ast
%}

%token <string> VAR
%token <string> CLSS
%token EQUALS
%token LBRACKET
%token RBRACKET
%token RETURN
%token CLASSES
%token COMMA
%token EOF
%token FIFO
%token FAIR
%token STRICT
%token SEMICOLON

%type <Ast.policy> policy
%type <Ast.seq> prog

%start prog

%%

/* Policies */
policy:
    | FIFO LBRACKET; pl = arglist; RBRACKET             { Fifo pl }
    | STRICT LBRACKET; pl = arglist; RBRACKET           { Strict pl }
    | FAIR LBRACKET; pl = arglist; RBRACKET             { Fair pl }
    | CLSS                                              { Class($1) }
    | VAR                                               { Var($1) }
arglist:
    | pl = separated_list(COMMA, policy)                { pl } ;

/* Declarations, assignments and returns */
internalcomp :
    | CLASSES; vl = separated_list(COMMA, CLSS); SEMICOLON     { DeclareComp (vl) }
    | VAR EQUALS policy SEMICOLON                              { AssnComp ($1, $3) }
    | RETURN policy                                            { RtnComp ($2) }
    | RETURN policy SEMICOLON                                  { RtnComp ($2) }

/* Program */
prog:
    | list (internalcomp) EOF                                  { $1 }
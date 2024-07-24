%{
    open Ast
%}

%token <string> VAR
%token <string> CLSS
%token <int> INT
%token EQUALS
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token RETURN
%token CLASSES
%token COMMA
%token SEMICOLON
%token EOF

%token FIFO
%token RR
%token STRICT
%token WFQ
%token EDF
%token SJN
%token SRTF

%type <policy> policy
%type <Ast.program> prog

%start prog

%%

/* Policies */
policy:
    | FIFO LBRACE; pl = arglist; RBRACE             { Fifo pl }
    | STRICT LBRACE; pl = arglist; RBRACE           { Strict pl }
    | RR LBRACE; pl = arglist; RBRACE               { RoundRobin pl }
    | WFQ LBRACE; pl = weighted_arglist; RBRACE     { WeightedFair pl }
    | EDF LBRACE; pl = arglist; RBRACE              { EarliestDeadline pl }
    | SJN LBRACE; pl = arglist; RBRACE              { ShortestJobNext pl }
    | SRTF LBRACE; pl = arglist; RBRACE             { ShortestRemaining pl }
    | CLSS                                          { Class($1) }
    | VAR                                           { Var($1) }
arglist:
    | pl = separated_list(COMMA, policy)            { pl }
weighted_arglist:
    | pl = separated_list(COMMA, weighted_arg)      { pl }
weighted_arg:
    | LPAREN; arg = separated_pair(policy, COMMA, INT); RPAREN      { arg } ;

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

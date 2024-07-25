%{
    open Ast
%}

%token <string> VAR
%token <string> CLSS
%token <int> INT
%token EQUALS
%token LBRACKET
%token RBRACKET
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
%token RCSP
%token LEAKY
%token TOKEN
%token STOPGO

%type <policy> policy
%type <Ast.program> prog

%start prog

%%

/* Policies */
policy:
    | FIFO LBRACKET; pl = arglist; RBRACKET             { Fifo pl }
    | STRICT LBRACKET; pl = arglist; RBRACKET           { Strict pl }
    | RR LBRACKET; pl = arglist; RBRACKET               { RoundRobin pl }
    | WFQ LBRACKET; pl = weighted_arglist; RBRACKET     { WeightedFair pl }
    | EDF LBRACKET; pl = arglist; RBRACKET              { EarliestDeadline pl }
    | SJN LBRACKET; pl = arglist; RBRACKET              { ShortestJobNext pl }
    | SRTF LBRACKET; pl = arglist; RBRACKET             { ShortestRemaining pl }
    | RCSP LBRACKET; pl = arglist; RBRACKET             { RateControlled pl }
    | LEAKY LBRACKET; LBRACKET; pl = arglist; RBRACKET; COMMA; args = pairing; RBRACKET    { LeakyBucket (pl, fst args, snd args) }
    | TOKEN LBRACKET; LBRACKET; pl = arglist; RBRACKET; COMMA; args = pairing; RBRACKET    { TokenBucket (pl, fst args, snd args) }
    | STOPGO LBRACKET; LBRACKET; pl = arglist; RBRACKET; COMMA; t = INT; RBRACKET   { StopAndGo (pl, t) } 
    | CLSS                                          { Class($1) }
    | VAR                                           { Var($1) }
arglist:
    | pl = separated_list(COMMA, policy)            { pl }
weighted_arglist:
    | pl = separated_list(COMMA, weighted_arg)      { pl }
weighted_arg:
    | LPAREN; arg = separated_pair(policy, COMMA, INT); RPAREN      { arg }
pairing:
    | arg = separated_pair(INT, COMMA, INT)         { arg }


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

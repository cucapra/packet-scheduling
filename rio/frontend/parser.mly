%{
    open Ast

    exception FormatError of string

    (* error messages *)
    let misplaced_return = "Program must contain exactly one return statement as its final component."
    let misplaced_declare = "Cannot interleave declarations and assignments. All declarations must be grouped as one at the top of the program."
    let missing_declare = "Program must begin with a declaration of classes."

    type internalcomp =
    | DeclareComp of clss list
    | AssnComp of var * policy
    | RtnComp of policy

    (* Check that a program contains exactly one return statement as its final component *)
    let rec validate_seq acc = function
    | RtnComp pol :: [] -> acc, pol
    | RtnComp _ :: _ -> raise (FormatError misplaced_return)
    | h :: t -> validate_seq (acc @ [h]) t
    | [] -> raise (FormatError misplaced_return)

    (* Check that a program's assignment list does not contain declarations *)
    let check_assn_list acc = function
    | AssnComp (var, pol) -> (var, pol) :: acc
    | _ -> raise (FormatError misplaced_declare)

    (* Validate a program sequence as a valid program *)
    let validate_program seq = match (validate_seq [] seq) with
    | DeclareComp classes :: t, rtn ->
        (classes, (List.fold_left check_assn_list [] t), rtn)
    | _ -> raise (FormatError missing_declare)
%}

%token <string> VAR
%token <string> CLSS
%token <int> INT
%token EQUALS
%token LBRACKET
%token RBRACKET
%token RETURN
%token CLASSES
%token UNION
%token COMMA
%token SEMICOLON
%token EOF

%token WIDTH
%token BUFFER
%token TIME

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

%start <program> prog

%%

set:
    | CLSS                                              { Class($1) }
    | UNION LBRACKET; pl = setlist; RBRACKET            { Union pl }

/* Policies */
policy:
    | FIFO LBRACKET; pl = set; RBRACKET                                              { Fifo pl }
    | EDF LBRACKET; pl = set; RBRACKET                                               { EarliestDeadline pl }
    | SJN LBRACKET; pl = set; RBRACKET                                               { ShortestJobNext pl }
    | SRTF LBRACKET; pl = set; RBRACKET                                              { ShortestRemaining pl }
    | RR LBRACKET; pl = arglist; RBRACKET                                            { RoundRobin pl }
    | STRICT LBRACKET; pl = arglist; RBRACKET                                        { Strict pl }
    | WFQ LBRACKET; pl = arglist; RBRACKET; LBRACKET; wt = weight_list; RBRACKET     { WeightedFair (pl, wt) }
    | RCSP LBRACKET; pl = arglist; RBRACKET                                          { RateControlled pl }

    | LEAKY LBRACKET; LBRACKET;
        pl = arglist; RBRACKET; COMMA;
        WIDTH EQUALS; i1 = INT; COMMA; BUFFER EQUALS; i2 = INT;
        RBRACKET                                        { LeakyBucket (pl, i1, i2) }

    | TOKEN LBRACKET; LBRACKET;
        pl = arglist; RBRACKET; COMMA;
        WIDTH EQUALS; i1 = INT; COMMA; TIME EQUALS; i2 = INT;
        RBRACKET                                        { TokenBucket (pl, i1, i2) }

    | STOPGO LBRACKET; LBRACKET;
        pl = arglist; RBRACKET; COMMA;
        WIDTH EQUALS; t = INT; RBRACKET                 { StopAndGo (pl, t) }

    | VAR                                               { Var($1) }
setlist:
    | pl = separated_list(COMMA, set)               { pl }
arglist:
    | pl = separated_list(COMMA, policy)            { pl }
weight_list:
    | pl = separated_list(COMMA, INT)               { pl }

/* Declarations, assignments and returns */
internalcomp :
    | CLASSES; vl = separated_list(COMMA, CLSS); SEMICOLON     { DeclareComp vl }
    | VAR EQUALS policy SEMICOLON                              { AssnComp ($1, $3) }
    | RETURN policy                                            { RtnComp $2 }
    | RETURN policy SEMICOLON                                  { RtnComp $2 }

/* Program Sequence */
progseq: list (internalcomp)                                   { $1 }

/* Program */
prog: progseq EOF                                              { validate_program $1 }
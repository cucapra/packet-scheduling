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

%start <program> prog

%%

/* Policies */
policy:
    | FIFO LBRACKET; pl = arglist; RBRACKET             { Fifo pl }
    | STRICT LBRACKET; pl = arglist; RBRACKET           { Strict pl }
    | FAIR LBRACKET; pl = arglist; RBRACKET             { Fair pl }
    | CLSS                                              { Class $1 }
    | VAR                                               { Var $1 }
arglist: separated_list(COMMA, policy)                  { $1 } ;

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
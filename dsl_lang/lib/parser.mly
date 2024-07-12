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

// %token <string> VAR 
%token <string> CLSS
// %token EQUALS
%token LBRACE
%token RBRACE
// %token RETURN 
// %token CLASSES 
%token COMMA
%token EOF 
%token FIFO 
%token FAIR 
%token STRICT 
// %token TRANSIENT
// %token NEWLINE

%type <policy> policy
%type <Ast.policy> prog

%start prog

%%
prog: policy EOF                               { $1 }

clss:
    | CLSS                                     { Class($1) }
    | policy                                { $1 }

policy:    
    | FIFO LBRACE arglist RBRACE               { fifo_list $3 } // or Fifo($3)
    | STRICT LBRACE arglist RBRACE              { strict_list $3 }
    | FAIR LBRACE arglist RBRACE               { rr_list $3 }

arglist:
    | exp COMMA arglist                         { $1::$3 }
    | exp                               { [$1] }

exp:
    | clss     { $1 }
    | policy    { $1 }

// fexp:
//     | FIFO LBRACE flist RBRACE                 { fifo_list $1 }

// flist:
//     | rrexp COMMA flist                       { $1::$3 }
//     | rrexp                                   { [$1] }

// rrexp:
//     | FAIR LBRACE rrlist RBRACE                            { rr_list $1 }

// rrlist:
//     | stexp COMMA rrlist            { $1::$3}
//     | stexp                         { [$1] }

// stexp:
//     | STRICT LBRACE stlist RBRACE                            { strict_list $1 }

// stlist:
//     | fexp COMMA stlist         { $1::$3 }
//     | fexp                      { [$1] }



// lexp:
//     | FIFO LBRACE plist RBRACE                 { Fifo($3) }
//     | FAIR LBRACE plist RBRACE                 { Fair($3) }
//     | STRICT LBRACE plist RBRACE               { Strict($3) }
//     | TRANSIENT LBRACE plist RBRACE            { Transient($3) }                

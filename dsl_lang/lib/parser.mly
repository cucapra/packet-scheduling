%{
  open Ast

  let tuple_type ts =
  match ts with
  | [t] -> t
  | _ -> TTuple ts

  let tuple_expr es =
  match es with
  | [e] -> e
  | _ -> Tuple es
%}

%token <string> VAR TNAME
%token LPAREN RPAREN DOT COLON EOF
%token LAMBDA ARROW
%token LET EQUALS IN
%token <int> INT
%token PLUS MINUS LESS GREATER
%token IF THEN ELSE TRUE FALSE AND OR NOT
%token EMPTY CONS MATCH WEMPTY WHDREST LIST
%token COMMA STAR
%token FIX

%type <Ast.exp> prog

%start prog

%%
prog: exp EOF                           { $1 }

exp:
    | LET VAR EQUALS texp IN exp        { Let($2, $4, $6) }
    | texp                              { $1 }

texp:
    | tlist                             { tuple_expr $1 }

tlist:
    | fexp COMMA tlist                  { $1::$3 }
    | fexp                              { [$1] }

fexp:
    | FIX lexp                          { Fix($2) }
    | lexp                              { $1 }

lexp:
    | LAMBDA VAR COLON typ DOT lexp     { Lam($2, $4, $6) }
    | mexp                              { $1 }

mexp: 
    | MATCH exp WEMPTY exp WHDREST exp  { Match($2, $4, $6) }
    | ifexp                             { $1 }

ifexp:
    | IF unexp THEN fexp ELSE fexp      { If($2, $4, $6) }
    | unexp                             { $1 }

unexp:
    | NOT unexp                         { Unary(Not, $2) }
    | boolbinexp                        { $1 }

boolbinexp:
    | boolbinexp AND cmpexp             { Binary(And, $1, $3) }
    | boolbinexp OR cmpexp              { Binary(Or, $1, $3) }
    | cmpexp                            { $1 }

cmpexp:
    | cmpexp LESS ibinexp               { Binary(Less, $1, $3) }
    | cmpexp GREATER ibinexp            { Binary(Greater, $1, $3) }
    | cmpexp EQUALS ibinexp             { Binary(Equal, $1, $3) }
    | ibinexp                           { $1 }

ibinexp:
    | ibinexp PLUS appexp               { Binary(Plus, $1, $3) }
    | ibinexp MINUS appexp              { Binary(Minus, $1, $3) }
    | ibinexp STAR appexp               { Binary(Times, $1, $3) }
    | ibinexp CONS appexp               { Binary(Cons, $1, $3) }
    | appexp                            { $1 }

appexp:
    | appexp pexp                       { App($1, $2) }
    | pexp                              { $1 }

pexp:
    | pexp DOT INT                      { Proj($1, $3) }
    | aexp                              { $1 }

aexp:
    | VAR                               { Var($1) }
    | INT                               { Int($1) }
    | TRUE                              { True }
    | FALSE                             { False }
    | EMPTY COLON typ                   { Empty($3) }
    | LPAREN exp RPAREN                 { $2 }

typ:
    | ftyp                              { $1 }

ftyp:
    | atyp ARROW ftyp                   { TFun($1, $3) }
    | ttlist                            { tuple_type $1 }

ttlist:
    | atyp STAR ttlist                  { $1::$3 }
    | atyp                              { [$1] }

atyp:
    | TNAME                             { TBase($1) }
    | LIST typ                          { TList($2) } 
    | LPAREN typ RPAREN                 { $2 }                   

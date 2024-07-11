type var = string

type typ =
  | TFun of typ * typ
  | TBase of string
  | TTuple of typ list
  | TList of typ

type binop =
  | Plus
  | Less
  | Greater
  | And
  | Or
  | Equal
  | Times
  | Minus
  | Cons

type unop =
  | Not

type exp =
  | True
  | False
  | Empty of typ
  | Int of int
  | Var of var
  | App of exp * exp
  | Lam of var * typ * exp
  | Let of var * exp * exp
  | Binary of binop * exp * exp
  | Unary of unop * exp
  | Tuple of exp list
  | Proj of exp * int
  | Fix of exp
  | If of exp * exp * exp
  | Match of exp * exp * exp

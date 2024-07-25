open Ast

exception UnboundVariable of var
exception IllformedExpression of string

type store = (var * policy) list

(* keeps track of all the classes the user has declared. *)
type classes = string list

(* A function to update the binding for x in store s.
   update (s, x, v) returns the store s[x->v]. *)
let rec update s x v : store =
  match s with
  | [] -> [ (x, v) ]
  | (y, u) :: t -> if x = y then (x, v) :: t else (y, u) :: update t x v

(* A function to look up the binding for a variable in a store.
   lookup s x returns s(x) or UnboundVariable if s is not defined on s. *)
let rec lookup s x : policy =
  match s with
  | [] -> raise (UnboundVariable x)
  | (y, u) :: t -> if x = y then u else lookup t x

(* Helper function that evaulates a policy list. *)
let rec evalplist (pl : policy list) (st : store) (cl : classes) =
  match pl with [] -> pl | h :: t -> eval_pol h st cl :: evalplist t st cl

(* Evaluates a policy, looking up any variables and substituting them in. *)
and eval_pol (p : policy) (st : store) (cl : classes) : policy =
  match p with
  | Class c ->
      if List.mem c cl then p
      else raise (UnboundVariable "Undeclared class used in policy")
  | Var x ->
      let pol = lookup st x in
      eval_pol pol st cl
  | Fifo (h :: t) -> Fifo (eval_pol h st cl :: evalplist t st cl)
  | RoundRobin (h :: t) -> RoundRobin (eval_pol h st cl :: evalplist t st cl)
  | Strict (h :: t) -> Strict (eval_pol h st cl :: evalplist t st cl)
  | _ -> failwith "cannot have empty policy"

(* A function to evaluate all the assignments in a program by updating the store
   with the variable and the policy it maps to. *)
let rec eval_assn (alist : assignment list) (st : store) (cl : classes) : store
    =
  match alist with
  | [] -> st
  | (var, pol) :: t ->
      let st' = update st var pol in
      eval_assn t st' cl

(* First funtion called by eval. Matches against the components of type program
   and further calls helper functions to check each component of a program. *)
let eval_helper (prog : program) (st : store) (cl : classes) : policy =
  match prog with
  | (clist, alist, ret) ->
      let cl' = cl @ clist in
      let st' = eval_assn alist st cl' in 
      eval_pol ret st' cl'

(* Outermost function that is called by main.ml *)
let eval (p : program) : policy = eval_helper p [] []

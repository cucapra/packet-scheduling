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
    | [] ->
      [(x, v)]
    | (y ,u)::t ->
      if x = y then (x, v)::t
      else (y, u)::(update t x v)
  
  (* A function to look up the binding for a variable in a store.
     lookup s x returns s(x) or UnboundVariable if s is not defined on s. *)
  let rec lookup s x : policy =
    match s with
    | [] ->
      raise (UnboundVariable x)
    | (y,u)::t ->
      if x = y then u
      else (lookup t x)


let rec evalp (p: policy) (st : store) (cl : classes) = 
  match p with 
  | Class c -> if List.mem c cl then c else raise (UnboundVariable "Undeclared class")
  | Fifo (h :: t) -> failwith "unimplimented"
  
(* A function to evaluate all the assignments in a program by updating the store *)
let rec evala (alist : assignment list) (st : store) (cl : classes) : store =
  match alist with
  | [] -> st
  | Assn(var, pol) :: t -> let st' = update st var pol in 
    evala t st' cl


(* Second-outermost function that is the first that is called by eval. So
therefore it knows that the first thing that is matched must be a sequence since
a program must at least declare classes then return a policy. *)
let eval' (prog : program) (st : store) (cl : classes): policy =
  match prog with 
  | Prog (dec, alist, ret) -> begin
    match dec with
    | DeclareClasses clist -> let cl' = cl @ clist in 
      let st' = evala alist st cl' in begin
      match ret with
      | Return p -> 

let eval (p : program) : policy =
  eval' p [] []
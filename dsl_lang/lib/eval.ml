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

let rec evals (s : statement) (st : store) (cl : classes): statement =
  match s with 
  | Seq (s1, s2) -> let dec = evals s1 st cl in begin
    match dec with 
    | Declare (DeclareClasses clsses) -> let seq2 = evals s2 st clsses in begin
      match seq2 with
      | Seq (c1, c2) -> let assn = evals c1 st clsses in begin 
        match assn with
        | Assignment (Assn (var, p)) -> let st' = update st var p in 
        evals c2 st' clsses
        | _ -> failwith "unimplimented yet"
      end
      | Ret (Return pol) -> seq2 (* return statement we have reached *)
      end
    
    | _ -> raise (IllformedExpression "first statement must be declaration of classes")
    end
  | _ -> raise (IllformedExpression "expected sequence")


(* Second-outermost function that is the first that is called by eval. So
therefore it knows that the first thing that is matched must be a sequence since
a program must at least declare classes then return a policy. *)
let rec eval' (stmnt : statement) (st : store) : policy =
  match stmnt with
  | Seq (s1, s2) ->
    let fin = evals stmnt st [] in begin
      match fin with
      | Ret (Return pol) -> begin
        match pol with
        | Var v -> lookup st v (* make sure store is being threaded through properly *)
        | _ -> pol
      end
      | _ -> raise (IllformedExpression "must return a policy")
    end
  | _ -> raise (IllformedExpression "first statement must be declaration of classes")
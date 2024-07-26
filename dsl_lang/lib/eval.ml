open Ast

exception UnboundVariable of string
exception UndeclaredClass of clss
exception IllformedExpression of string

(* A function to look up the binding for a variable in a store.
   `lookup s x` returns `s(x)` or raises `UnboundVariable x` if `x` is not defined on `s`. *)
let lookup s x =
  match List.assoc_opt x s with
  | Some v -> v
  | None -> raise (UnboundVariable x)

(* Evaluates a policy, looking up any variables and substituting them in. *)
let rec eval_pol st cl p =
  (* Helper function that evaulates a policy list. *)
  let eval_plst st cl = List.map (eval_pol st cl) in

  (* Helper function that evaluates a weighted policy list. *)
  let eval_weighted_plst st cl =
    List.map (fun (x, i) -> (eval_pol st cl x, i))
  in

  match p with
  | Class c -> if List.mem c cl then p else raise (UndeclaredClass c)
  | Var x -> eval_pol st cl (lookup st x)
  | Fifo plst -> Fifo (eval_plst st cl plst)
  | RoundRobin plst -> RoundRobin (eval_plst st cl plst)
  | Strict plst -> Strict (eval_plst st cl plst)
  | WeightedFair wplst -> WeightedFair (eval_weighted_plst st cl wplst)
  | EarliestDeadline plst -> EarliestDeadline (eval_plst st cl plst)
  | ShortestJobNext plst -> ShortestJobNext (eval_plst st cl plst)
  | ShortestRemaining plst -> ShortestRemaining (eval_plst st cl plst)
  | RateControlled plst -> RateControlled (eval_plst st cl plst)
  | LeakyBucket (plst, n1, n2) -> LeakyBucket (eval_plst st cl plst, n1, n2)
  | TokenBucket (plst, n1, n2) -> TokenBucket (eval_plst st cl plst, n1, n2)
  | StopAndGo (plst, n) -> StopAndGo (eval_plst st cl plst, n)

(* Outermost function that is called by main.ml *)
let eval (clist, alist, ret) = eval_pol alist clist ret

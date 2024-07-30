open Ast

exception UnboundVariable of var
exception UndeclaredClass of clss

(* A function to look up the binding for a variable in a store.
   `lookup s x` returns `s(x)` or raises `UnboundVariable x` if `x` is not defined on `s`. *)
let lookup s x =
  match List.assoc_opt x s with
  | Some v -> v
  | None -> raise (UnboundVariable x)

(* Evaluates a policy, looking up any variables and substituting them in. *)
let rec eval_pol cl st p =
  (* Helper function that evaulates a policy list. *)
  let eval_plst cl st = List.map (eval_pol cl st) in

  (* Helper function that evaluates a weighted policy list. *)
  let eval_weighted_plst cl st =
    List.map (fun (x, i) -> (eval_pol cl st x, i))
  in

  match p with
  | Class c -> if List.mem c cl then p else raise (UndeclaredClass c)
  | Var x -> eval_pol cl st (lookup st x)
  | Fifo plst -> Fifo (eval_plst cl st plst)
  | RoundRobin plst -> RoundRobin (eval_plst cl st plst)
  | Strict plst -> Strict (eval_plst cl st plst)
  | WeightedFair wplst -> WeightedFair (eval_weighted_plst cl st wplst)
  | EarliestDeadline plst -> EarliestDeadline (eval_plst cl st plst)
  | ShortestJobNext plst -> ShortestJobNext (eval_plst cl st plst)
  | ShortestRemaining plst -> ShortestRemaining (eval_plst cl st plst)
  | RateControlled plst -> RateControlled (eval_plst cl st plst)
  | LeakyBucket (plst, n1, n2) -> LeakyBucket (eval_plst cl st plst, n1, n2)
  | TokenBucket (plst, n1, n2) -> TokenBucket (eval_plst cl st plst, n1, n2)
  | StopAndGo (plst, n) -> StopAndGo (eval_plst cl st plst, n)

(* Outermost function that is called by main.ml *)
let eval (cl, alst, ret) = eval_pol cl alst ret

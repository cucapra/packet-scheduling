(* Changes to this type must also be reflected in `Ast.policy` in ast.ml *)
type t =
  | Class of Ast.clss
  | Fifo of t list
  | RoundRobin of t list
  | Strict of t list
  | WeightedFair of (t * float) list
(*
   | EarliestDeadline of t list
   | ShortestJobNext of t list
   | ShortestRemaining of t list
   | RateControlled of t list
   | LeakyBucket of t list * int * int
   | TokenBucket of t list * int * int
   | StopAndGo of t list * int
*)

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss

let lookup s x =
  match List.assoc_opt x s with
  | Some v -> v
  | None -> raise (UnboundVariable x)

let rec eval cl st (p : Ast.policy) =
  (* Helper function that evaluates a policy list. *)
  let eval_plst cl st = List.map (eval cl st) in

  (* Helper function that evaluates a weighted policy list. *)
  let eval_weighted_plst cl st = List.map (fun (x, i) -> (eval cl st x, i)) in

  match p with
  | Class c -> if List.mem c cl then Class c else raise (UndeclaredClass c)
  | Var x -> eval cl st (lookup st x)
  | Fifo plst -> Fifo (eval_plst cl st plst)
  | RoundRobin plst -> RoundRobin (eval_plst cl st plst)
  | Strict plst -> Strict (eval_plst cl st plst)
  | WeightedFair wplst -> WeightedFair (eval_weighted_plst cl st wplst)
  (*
  | EarliestDeadline plst -> EarliestDeadline (eval_plst cl st plst)
  | ShortestJobNext plst -> ShortestJobNext (eval_plst cl st plst)
  | ShortestRemaining plst -> ShortestRemaining (eval_plst cl st plst)
  | RateControlled plst -> RateControlled (eval_plst cl st plst)
  | LeakyBucket (plst, n1, n2) -> LeakyBucket (eval_plst cl st plst, n1, n2)
  | TokenBucket (plst, n1, n2) -> TokenBucket (eval_plst cl st plst, n1, n2)
  | StopAndGo (plst, n) -> StopAndGo (eval_plst cl st plst, n)
  *)
  | _ -> failwith "ERROR: unsupported policy"

(* Evaluates a program, looking up any variables and substituting them in. *)
let of_program (cl, alst, ret) = eval cl alst ret

let rec to_string p =
  let sprintf = Printf.sprintf in

  (* Helper function to compactly join policy lists by comma *)
  let join lst =
    sprintf "[%s]" (lst |> List.map to_string |> String.concat ", ")
  in

  (* Helper function to compactly join weighted policy lists by comma *)
  let join_weighted lst =
    sprintf "[%s]"
      (lst
      |> List.map (fun (x, y) -> sprintf "(%s, %.2f)" (to_string x) y)
      |> String.concat ", ")
  in

  match p with
  | Class c -> c
  | Fifo lst -> sprintf "fifo%s" (join lst)
  | RoundRobin lst -> sprintf "rr%s" (join lst)
  | Strict lst -> sprintf "strict%s" (join lst)
  | WeightedFair lst -> sprintf "wfq%s" (join_weighted lst)
(*
  | EarliestDeadline lst -> sprintf "edf%s" (join lst)
  | ShortestJobNext lst -> sprintf "sjn%s" (join lst)
  | ShortestRemaining lst -> sprintf "srtf%s" (join lst)
  | RateControlled lst -> sprintf "rcsp%s" (join lst)
  | LeakyBucket (lst, width, buffer) ->
      sprintf "leaky[%s, width = %d, buffer = %d]" (join lst) width buffer
  | TokenBucket (lst, width, buffer) ->
      sprintf "token[%s, width = %d, time = %d]" (join lst) width buffer
  | StopAndGo (lst, width) ->
      sprintf "stopandgo[%s, width = %d]" (join lst) width
  *)

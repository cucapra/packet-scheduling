(* Changes to this type must also be reflected in `Ast.policy` in ast.ml *)
type t =
  | Class of Ast.clss
  | Fifo of t list
  | RoundRobin of t list
  | Strict of t list
  | WeightedFair of (t * int) list
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
  | Ast.ShortestRemaining plst -> ShortestRemaining (eval_plst cl st plst)
  | Ast.RateControlled plst -> RateControlled (eval_plst cl st plst)
  | Ast.LeakyBucket (plst, n1, n2) -> LeakyBucket (eval_plst cl st plst, n1, n2)
  | Ast.TokenBucket (plst, n1, n2) -> TokenBucket (eval_plst cl st plst, n1, n2)
  | Ast.StopAndGo (plst, n) -> StopAndGo (eval_plst cl st plst, n)
  *)
  | _ -> failwith "ERROR: unsupported policy"

(* Evaluates a program, looking up any variables and substituting them in. *)
let of_program (cl, alst, ret) = eval cl alst ret

let rec to_string p =
  let bracket_wrap s = "[" ^ s ^ "]" in

  (* Helper function to compactly join policy lists by comma *)
  let join lst =
    lst |> List.map to_string |> String.concat ", " |> bracket_wrap
  in

  (* Helper function to compactly join weighted policy lists by comma *)
  let join_weighted lst =
    lst
    |> List.map (fun (x, y) -> "(" ^ to_string x ^ ", " ^ string_of_int y ^ ")")
    |> String.concat ", " |> bracket_wrap
  in

  match p with
  | Class c -> c
  | Fifo lst -> "fifo" ^ join lst
  | RoundRobin lst -> "rr" ^ join lst
  | Strict lst -> "strict" ^ join lst
  | WeightedFair lst -> "strict" ^ join_weighted lst
(*
  | EarliestDeadline lst -> "edf" ^ join lst
  | ShortestJobNext lst -> "sjn" ^ join lst
  | ShortestRemaining lst -> "srtf" ^ join lst
  | RateControlled lst -> "rcsp" ^ join lst
  | LeakyBucket (lst, width, buffer) ->
      "leaky[" ^ join lst ^ ", width = " ^ string_of_int width ^ ", buffer = "
      ^ string_of_int buffer ^ "]"
  | TokenBucket (lst, width, buffer) ->
      "token[" ^ join lst ^ ", width = " ^ string_of_int width ^ ", time = "
      ^ string_of_int buffer ^ "]"
  | StopAndGo (lst, width) ->
      "stopandgo[" ^ join lst ^ ", width = " ^ string_of_int width ^ "]"
  *)

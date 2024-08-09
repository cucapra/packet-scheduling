(* Changes to this type must also be reflected in `Ast.policy` in ast.ml *)
type t =
  | Class of Ast.clss
  | Fifo of t list
  | RoundRobin of t list
  | Strict of t list
  | WeightedFair of (t * int) list
  | EarliestDeadline of t list
  | ShortestJobNext of t list
  | ShortestRemaining of t list
  | RateControlled of t list
  | LeakyBucket of t list * int * int
  | TokenBucket of t list * int * int
  | StopAndGo of t list * int

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

let lookup s x =
  match List.assoc_opt x s with
  | Some v -> v
  | None -> raise (UnboundVariable x)

let rec eval cl st (used : string list ref) p =
  (* Helper function that evaulates a policy list. *)
  let eval_plst cl st used = List.map (eval cl st used) in

  (* Helper function that evaluates a weighted policy list. *)
  let eval_weighted_plst cl st used =
    List.map (fun (x, i) -> (eval cl st used x, i))
  in

  match p with
  | Ast.Class c ->
      if List.mem c !used then raise (DuplicateClass c)
      else if List.mem c cl then (
        used := c :: !used;
        Class c)
      else raise (UndeclaredClass c)
  | Ast.Var x -> eval cl st used (lookup st x)
  | Ast.Fifo plst -> Fifo (eval_plst cl st used plst)
  | Ast.RoundRobin plst -> RoundRobin (eval_plst cl st used plst)
  | Ast.Strict plst -> Strict (eval_plst cl st used plst)
  | Ast.WeightedFair wplst -> WeightedFair (eval_weighted_plst cl st used wplst)
  | Ast.EarliestDeadline plst -> EarliestDeadline (eval_plst cl st used plst)
  | Ast.ShortestJobNext plst -> ShortestJobNext (eval_plst cl st used plst)
  | Ast.ShortestRemaining plst -> ShortestRemaining (eval_plst cl st used plst)
  | Ast.RateControlled plst -> RateControlled (eval_plst cl st used plst)
  | Ast.LeakyBucket (plst, n1, n2) ->
      LeakyBucket (eval_plst cl st used plst, n1, n2)
  | Ast.TokenBucket (plst, n1, n2) ->
      TokenBucket (eval_plst cl st used plst, n1, n2)
  | Ast.StopAndGo (plst, n) -> StopAndGo (eval_plst cl st used plst, n)

(* Evaluates a program, looking up any variables and substituting them in. It returns
   the final policy with any variables substituted in. *)
let from_program (cl, alst, ret) : t = eval cl alst (ref []) ret

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

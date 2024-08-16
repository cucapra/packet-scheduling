(* Changes to this type must also be reflected in `Ast.policy` in ast.ml *)
type t =
  | Class of Ast.clss
  | Fifo of t list
  | RoundRobin of t list
  | Strict of t list
  | WeightedFair of (t * float) list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss

let lookup s x =
  match List.assoc_opt x s with
  | Some v -> v
  | None -> raise (UnboundVariable x)

let rec sub cl st (p : Ast.policy) =
  (* Helper function that evaluates a policy list. *)
  let sub_plst cl st = List.map (sub cl st) in

  (* Helper function that evaluates a weighted policy list. *)
  let sub_weighted_plst cl st = List.map (fun (x, i) -> (sub cl st x, i)) in

  match p with
  | Class c -> if List.mem c cl then Class c else raise (UndeclaredClass c)
  | Var x -> sub cl st (lookup st x)
  | Fifo plst -> Fifo (sub_plst cl st plst)
  | RoundRobin plst -> RoundRobin (sub_plst cl st plst)
  | Strict plst -> Strict (sub_plst cl st plst)
  | WeightedFair wplst -> WeightedFair (sub_weighted_plst cl st wplst)
  | _ -> failwith "ERROR: unsupported policy"

(* Look up any variables and substitute them in. *)
let of_program (cl, alst, ret) = sub cl alst ret

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

(* Changes to this type must also be reflected in `Ast.policy` in ast.ml *)
type t =
  | Class of Ast.clss
  | Fifo of t list
  | RoundRobin of t list
  | Strict of t list
  | WeightedFair of (t * float) list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

let lookup s x =
  match List.assoc_opt x s with
  | Some v -> v
  | None -> raise (UnboundVariable x)

let rec sub_set cl st (p : Ast.set) used =
  match p with
  | Class c ->
      if List.mem c !used then raise (DuplicateClass c)
      else if List.mem c cl then (
        used := c :: !used;
        [ Class c ])
      else raise (UndeclaredClass c)
  | Union slst ->
      let subbed = List.map (fun x -> sub_set cl st x used) slst in
      List.flatten subbed

let rec sub cl st (p : Ast.policy) used =
  let sub_plst cl st = List.map (fun x -> sub cl st x used) in
  let sub_weighted_plst cl st =
    List.map (fun (x, i) -> (sub cl st x used, i))
  in

  match p with
  | Var x -> sub cl st (lookup st x) used
  | Fifo p -> Fifo (sub_set cl st p used)
  | RoundRobin plst -> RoundRobin (sub_plst cl st plst)
  | Strict plst -> Strict (sub_plst cl st plst)
  | WeightedFair (plst, wts) ->
      WeightedFair
        (sub_weighted_plst cl st
           (List.combine plst (List.map float_of_int wts)))
  | _ -> failwith "ERROR: unsupported policy"

(* Look up any variables and substitute them in. *)
let of_program (cl, alst, ret) : t = sub cl alst ret (ref [])

let rec to_string p =
  let sprintf = Printf.sprintf in
  let join lst =
    sprintf "[%s]" (lst |> List.map to_string |> String.concat ", ")
  in
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

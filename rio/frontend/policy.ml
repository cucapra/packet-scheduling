(* Changes to this type must also be reflected in `Ast.policy` in ast.ml *)
type set =
  | Class of Ast.clss
  | Union of set list

type t =
  | Fifo of set
  | EarliestDeadline of set
  | ShortestJobNext of set
  | ShortestRemaining of set
  | RoundRobin of t list
  | Strict of t list
  | WeightedFair of t list * int list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

let lookup s x =
  match List.assoc_opt x s with
  | Some v -> v
  | None -> raise (UnboundVariable x)

let rec sub_set cl (p : Ast.set) used : set =
  let sub_slst = List.map (fun x -> sub_set cl x used) in

  match p with
  | Class c ->
      if List.mem c !used then raise (DuplicateClass c)
      else if List.mem c cl then (
        used := c :: !used;
        (Class c : set))
      else raise (UndeclaredClass c)
  | Union clst -> Union (sub_slst clst)

let rec sub cl st (p : Ast.policy) used =
  let sub_plst cl st = List.map (fun x -> sub cl st x used) in

  match p with
  | Var x -> sub cl st (lookup st x) used
  | Fifo set -> Fifo (sub_set cl set used)
  | EarliestDeadline set -> EarliestDeadline (sub_set cl set used)
  | ShortestJobNext set -> ShortestJobNext (sub_set cl set used)
  | ShortestRemaining set -> ShortestRemaining (sub_set cl set used)
  | RoundRobin plst -> RoundRobin (sub_plst cl st plst)
  | Strict plst -> Strict (sub_plst cl st plst)
  | WeightedFair (plst, wts) -> WeightedFair (sub_plst cl st plst, wts)
  | _ -> failwith "ERROR: unsupported policy"

(* Look up any variables and substitute them in. *)
let of_program (cl, alst, ret) : t = sub cl alst ret (ref [])

let rec set_to_string p =
  let sprintf = Printf.sprintf in
  match p with
  | Class c -> c
  | Union lst ->
      sprintf "union[%s]" (lst |> List.map set_to_string |> String.concat ",")

let rec to_string p =
  let sprintf = Printf.sprintf in
  let join lst =
    sprintf "[%s]" (lst |> List.map to_string |> String.concat ", ")
  in
  let join_ints lst =
    sprintf "[%s]" (lst |> List.map string_of_int |> String.concat ", ")
  in

  match p with
  | Fifo set -> sprintf "fifo%s" (set_to_string set)
  | EarliestDeadline set -> sprintf "edf%s" (set_to_string set)
  | ShortestJobNext set -> sprintf "sjn%s" (set_to_string set)
  | ShortestRemaining set -> sprintf "fifo%s" (set_to_string set)
  | RoundRobin lst -> sprintf "rr%s" (join lst)
  | Strict lst -> sprintf "strict%s" (join lst)
  | WeightedFair (lst, wts) -> sprintf "wfq%s %s" (join lst) (join_ints wts)

type set2stream = EDF | FIFO
type stream2stream = RR | Strict

type t =
  | Leaf of (set2stream * Ast.clss list)
  | Node of (stream2stream * t list)

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

let rec lookup x = function
  | (a, b) :: t when a = x -> b, t
  | _ :: t -> lookup x t
  | [] -> raise (UnboundVariable x)


let rec sub cl st (p : Ast.policy) used =
  let rec sub_set cl (s : Ast.set) =
    match s with
    | Union ss -> ss |> List.map (sub_set cl) |> List.flatten
    | Class c ->
        if List.mem c !used then 
          raise (DuplicateClass c)
        else if not (List.mem c cl) then 
          raise (UndeclaredClass c)
        else 
          (used := c :: !used; [ c ])
  in
  let sub_ps cl st = List.map (fun x -> sub cl st x used) in

  match p with
  | Var x -> let v, t = (lookup x st) in sub cl t v used
  | Fifo s -> Leaf (FIFO, sub_set cl s)
  | EarliestDeadline s -> Leaf (EDF, sub_set cl s)
  | RoundRobin ps -> Node (RR, sub_ps cl st ps)
  | Strict ps -> Node (Strict, sub_ps cl st ps)
  | _ -> failwith "ERROR: unsupported policy"

(* Look up any variables and substitute them in. *)
let of_program (cl, alst, ret) = sub cl alst ret (ref [])

let to_string _ = "NOT COMPLETE YET! SORRY"
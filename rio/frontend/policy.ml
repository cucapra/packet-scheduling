type t =
  | Fifo of Ast.clss list
  | EDF of Ast.clss list
  | RoundRobin of t list
  | Strict of t list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

let rec lookup x = function
  | [] -> raise (UnboundVariable x)
  | (v, p) :: t when v = x -> (p, t)
  | (_, _) :: t -> lookup x t

let rec sub cl st used (p : Ast.stream) =
  let sub_ps = List.map (sub cl st used) in
  let rec sub_set = function
    | Ast.Class c ->
        if List.mem c !used then raise (DuplicateClass c)
        else if not (List.mem c cl) then raise (UndeclaredClass c)
        else (
          used := c :: !used;
          [ c ])
    | Ast.Union sets -> sets |> List.map sub_set |> List.flatten
  in

  match p with
  | Var x ->
      let p, st = lookup x st in
      sub cl st used p
  | Fifo s -> Fifo (sub_set s)
  | EarliestDeadline s -> EDF (sub_set s)
  | RoundRobin ps -> RoundRobin (sub_ps ps)
  | Strict ps -> Strict (sub_ps ps)
  | _ -> failwith "ERROR: unsupported policy"

(* Look up any variables and substitute them in. *)
let of_program (classes, assigns, ret) = sub classes assigns (ref []) ret

let rec to_string p =
  let fmt = Printf.sprintf in
  let join lst to_string = lst |> List.map to_string |> String.concat ", " in

  match p with
  | Fifo cs when List.length cs > 1 -> fmt "fifo[union[%s]]" (join cs Fun.id)
  | EDF cs when List.length cs > 1 -> fmt "edf[union[%s]]" (join cs Fun.id)
  | Fifo cs -> fmt "fifo[%s]" (join cs Fun.id)
  | EDF cs -> fmt "edf[%s]" (join cs Fun.id)
  | RoundRobin ps -> fmt "rr[%s]" (join ps to_string)
  | Strict ps -> fmt "strict[%s]" (join ps to_string)

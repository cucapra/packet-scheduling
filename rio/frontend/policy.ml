type t =
  | FIFO of Ast.clss
  | UNION of t list
  | Strict of t list
  | RR of t list
  | WFQ of t list * float list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

let rec lookup x = function
  | [] -> raise (UnboundVariable x)
  | (v, p) :: t when v = x -> (p, t)
  | (_, _) :: t -> lookup x t

let rec sub cl st used (p : Ast.stream) =
  let sub_ps = List.map (sub cl st used) in
  let rec sub_set wrap = function
    | Ast.Class c ->
        if List.mem c !used then raise (DuplicateClass c)
        else if not (List.mem c cl) then raise (UndeclaredClass c)
        else (
          used := c :: !used;
          wrap c)
    | Ast.Union sets -> UNION (sets |> List.map (sub_set wrap))
  in

  match p with
  | Var x ->
      let p, st = lookup x st in
      sub cl st used p
  | Fifo s -> sub_set (fun c -> FIFO c) s
  | Strict ps -> Strict (sub_ps ps)
  | RoundRobin ps -> RR (sub_ps ps)
  | WeightedFair pws ->
      let ps, ws = List.split pws in
      WFQ (sub_ps ps, List.map float_of_int ws)
  | _ -> failwith "ERROR: unsupported policy"

(* Look up any variables and substitute them in. *)
let of_program (classes, assigns, ret) = sub classes assigns (ref []) ret

let rec to_string p =
  let fmt = Printf.sprintf in
  let join lst to_string = lst |> List.map to_string |> String.concat ", " in

  match p with
  | FIFO c -> fmt "fifo[%s]" c
  | UNION ps -> fmt "union[%s]" (join ps to_string)
  | Strict ps -> fmt "strict[%s]" (join ps to_string)
  | RR ps -> fmt "rr[%s]" (join ps to_string)
  | WFQ (ps, ws) ->
      let pws = List.combine ps ws in
      let to_string (p, w) = fmt "(%s, %f)" (to_string p) w in
      fmt "wfq[%s]" (join pws to_string)

type t =
  | FIFO of Ast.clss list
  | EDF of Ast.clss list
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
  | Fifo s -> FIFO (sub_set s)
  | EarliestDeadline s -> EDF (sub_set s)
  | Strict ps -> Strict (sub_ps ps)
  | RoundRobin ps -> RR (sub_ps ps)
  | WeightedFair pws ->
      let ps, ws = List.split pws in
      WFQ (sub_ps ps, List.map float_of_int ws)
  | _ -> failwith "ERROR: unsupported policy"

let rec normalize p =
  match p with
  | FIFO cs -> FIFO (List.sort String.compare cs)
  | EDF cs -> EDF (List.sort String.compare cs)
  | Strict ps -> Strict (List.map normalize ps)
  | RR ps ->
      let ps = List.map normalize ps in
      RR (List.sort compare ps)
  | WFQ (ps, ws) ->
      let ps = List.map normalize ps in
      let pairs =
        List.sort
          (fun (p1, w1) (p2, w2) ->
            let c = compare p1 p2 in
            if c <> 0 then c else compare w1 w2)
          (List.combine ps ws)
      in
      let ps, ws = List.split pairs in
      WFQ (ps, ws)

(* Look up any variables and substitute them in. Then normalize the resulting policy. *)
let of_program (classes, assigns, ret) =
  sub classes assigns (ref []) ret |> normalize

let rec to_string p =
  let fmt = Printf.sprintf in
  let join lst to_string = lst |> List.map to_string |> String.concat ", " in
  match p with
  | FIFO cs when List.length cs > 1 -> fmt "fifo[union[%s]]" (join cs Fun.id)
  | EDF cs when List.length cs > 1 -> fmt "edf[union[%s]]" (join cs Fun.id)
  | FIFO cs -> fmt "fifo[%s]" (join cs Fun.id)
  | EDF cs -> fmt "edf[%s]" (join cs Fun.id)
  | Strict ps -> fmt "strict[%s]" (join ps to_string)
  | RR ps -> fmt "rr[%s]" (join ps to_string)
  | WFQ (ps, ws) ->
      let pws = List.combine ps ws in
      let to_string (p, w) = fmt "(%s, %f)" (to_string p) w in
      fmt "wfq[%s]" (join pws to_string)

let rec to_json p : Yojson.Basic.t =
  match p with
  | FIFO cs -> `Assoc [ ("FIFO", `List (List.map (fun c -> `String c) cs)) ]
  | EDF cs -> `Assoc [ ("EDF", `List (List.map (fun c -> `String c) cs)) ]
  | Strict ps -> `Assoc [ ("Strict", `List (List.map to_json ps)) ]
  | RR ps -> `Assoc [ ("RR", `List (List.map to_json ps)) ]
  | WFQ (ps, ws) ->
      let pairs = List.map2 (fun p w -> `List [ to_json p; `Float w ]) ps ws in
      `Assoc [ ("WFQ", `List pairs) ]

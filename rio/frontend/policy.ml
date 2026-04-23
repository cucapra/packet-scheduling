type t =
  | FIFO of Ast.clss
  | UNION of t list
  | SP of t list
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
  | Strict ps -> SP (sub_ps ps)
  | RoundRobin ps -> RR (sub_ps ps)
  | WeightedFair pws ->
      let ps, ws = List.split pws in
      WFQ (sub_ps ps, List.map float_of_int ws)
  | _ -> failwith "ERROR: unsupported policy"

let rec normalize p =
  match p with
  | FIFO _ -> p
  | UNION ps -> UNION (List.map normalize ps |> List.sort compare)
  | SP ps -> SP (List.map normalize ps)
  | RR ps -> RR (List.map normalize ps |> List.sort compare)
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
  | FIFO c -> fmt "fifo[%s]" c
  | UNION ps -> fmt "union[%s]" (join ps to_string)
  | SP ps -> fmt "strict[%s]" (join ps to_string)
  | RR ps -> fmt "rr[%s]" (join ps to_string)
  | WFQ (ps, ws) ->
      let pws = List.combine ps ws in
      let to_string (p, w) = fmt "(%s, %f)" (to_string p) w in
      fmt "wfq[%s]" (join pws to_string)

let rec to_json p : Yojson.Basic.t =
  match p with
  | FIFO c -> `Assoc [ ("FIFO", `String c) ]
  | UNION ps -> `Assoc [ ("Union", `List (List.map to_json ps)) ]
  | SP ps -> `Assoc [ ("Strict", `List (List.map to_json ps)) ]
  | RR ps -> `Assoc [ ("RR", `List (List.map to_json ps)) ]
  | WFQ (ps, ws) ->
      let pairs = List.map2 (fun p w -> `List [ to_json p; `Float w ]) ps ws in
      `Assoc [ ("WFQ", `List pairs) ]

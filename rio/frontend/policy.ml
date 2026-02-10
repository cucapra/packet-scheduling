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

(* Look up any variables and substitute them in. *)
let of_program (classes, assigns, ret) = sub classes assigns (ref []) ret

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

let to_normalized_json p : Yojson.Basic.t =
  let rec encode p =
    match p with
    | FIFO cs -> `Assoc [ ("FIFO", `List (List.map (fun c -> `String c) cs)) ]
    | EDF cs -> `Assoc [ ("EDF", `List (List.map (fun c -> `String c) cs)) ]
    | Strict ps -> `Assoc [ ("Strict", `List (List.map encode ps)) ]
    | RR ps -> `Assoc [ ("RR", `List (List.map encode ps)) ]
    | WFQ (ps, ws) ->
        let f p w = `List [ encode p; `Float w ] in
        `Assoc [ ("WFQ", `List (List.map2 f ps ws)) ]
  in
  let rec normalize json =
    match json with
    | `Assoc pairs -> (
        let normalized_pairs =
          List.map (fun (k, v) -> (k, normalize v)) pairs
        in
        match normalized_pairs with
        | [ ("FIFO", `List items) ] ->
            let sorted = List.sort compare items in
            `Assoc [ ("FIFO", `List sorted) ]
        | [ ("EDF", `List items) ] ->
            let sorted = List.sort compare items in
            `Assoc [ ("EDF", `List sorted) ]
        | [ ("RR", `List items) ] ->
            let sorted =
              List.sort (fun a b -> compare (normalize a) (normalize b)) items
            in
            `Assoc [ ("RR", `List sorted) ]
        | [ ("WFQ", `List pairs) ] ->
            let sorted =
              List.sort (fun a b -> compare (normalize a) (normalize b)) pairs
            in
            `Assoc [ ("WFQ", `List sorted) ]
        | _ -> `Assoc normalized_pairs)
    | `List items -> `List (List.map normalize items)
    | other -> other
  in
  p |> encode |> normalize

let equiv (p1 : t) (p2 : t) : bool =
  Yojson.Basic.equal (to_normalized_json p1) (to_normalized_json p2)

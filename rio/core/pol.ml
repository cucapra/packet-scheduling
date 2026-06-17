type t =
  | FIFO of Ast.clss
  | SP of (t * float) list * bool
  | RR of t list
  | WFQ of (t * float) list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

let rec lookup x = function
  | [] -> raise (UnboundVariable x)
  | (v, p) :: t when v = x -> (p, t)
  | (_, _) :: t -> lookup x t

let rec sub cl st used (p : Ast.stream) =
  let sub_ps = List.map (sub cl st used) in
  let claim c =
    if List.mem c !used then raise (DuplicateClass c)
    else if not (List.mem c cl) then raise (UndeclaredClass c)
    else used := c :: !used
  in
  match p with
  | Var x ->
      let p, st = lookup x st in
      sub cl st used p
  | Fifo c ->
      claim c;
      FIFO c
  | Strict prs ->
      let ps, rs = List.split prs in
      SP (List.combine (sub_ps ps) rs, false)
  | RoundRobin ps -> RR (sub_ps ps)
  | WeightedFair pws ->
      let ps, ws = List.split pws in
      WFQ (List.combine (sub_ps ps) ws)
  | _ -> failwith "ERROR: unsupported policy"

let rec normalize p =
  match p with
  | FIFO _ -> p
  | SP (prs, designated) ->
      (* SP arms canonicalize by rank ascending (the priority order is the
         discipline-defining datum); ties break by arm content. *)
      SP
        ( List.map (fun (p, r) -> (normalize p, r)) prs
          |> List.sort (fun (p1, r1) (p2, r2) ->
              let c = compare r1 r2 in
              if c <> 0 then c else compare p1 p2),
          designated )
  | RR ps -> RR (List.map normalize ps |> List.sort compare)
  | WFQ pws ->
      (* WFQ arms canonicalize by arm content (weights are independent
         shares; same-arm collision is the rare case); ties break by weight. *)
      WFQ
        (List.map (fun (p, w) -> (normalize p, w)) pws
        |> List.sort (fun (p1, w1) (p2, w2) ->
            let c = compare p1 p2 in
            if c <> 0 then c else compare w1 w2))

(* Look up any variables and substitute them in. Then normalize the resulting policy. *)
let of_program (classes, assigns, ret) =
  sub classes assigns (ref []) ret |> normalize

let rec to_string p =
  let fmt = Printf.sprintf in
  let join lst to_string = lst |> List.map to_string |> String.concat ", " in
  match p with
  | FIFO c -> fmt "fifo[%s]" c
  | SP (prs, _) ->
      let to_string (p, r) = fmt "(%s, %g)" (to_string p) r in
      fmt "strict[%s]" (join prs to_string)
  | RR ps -> fmt "rr[%s]" (join ps to_string)
  | WFQ pws ->
      let to_string (p, w) = fmt "(%s, %f)" (to_string p) w in
      fmt "wfq[%s]" (join pws to_string)

let rec walk p path =
  match (p, path) with
  | _, [] -> p
  | FIFO _, _ :: _ -> failwith "Pol.walk: path through FIFO leaf"
  | (SP (prs, _) | WFQ prs), i :: rest -> walk (fst (List.nth prs i)) rest
  | RR ps, i :: rest -> walk (List.nth ps i) rest

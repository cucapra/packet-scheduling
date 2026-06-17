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

let rec sub cl st used (p : Ast.stream) : Ast.clss list * t =
  let claim used c =
    if List.mem c used then raise (DuplicateClass c)
    else if not (List.mem c cl) then raise (UndeclaredClass c)
    else c :: used
  in
  match p with
  | Var x ->
      let p, st = lookup x st in
      sub cl st used p
  | Fifo c -> (claim used c, FIFO c)
  | Strict prs ->
      let ps, rs = List.split prs in
      let used, qs = List.fold_left_map (sub cl st) used ps in
      (used, SP (List.combine qs rs, false))
  | RoundRobin ps ->
      let used, qs = List.fold_left_map (sub cl st) used ps in
      (used, RR qs)
  | WeightedFair pws ->
      let ps, ws = List.split pws in
      let used, qs = List.fold_left_map (sub cl st) used ps in
      (used, WFQ (List.combine qs ws))
  | _ -> failwith "ERROR: unsupported policy"

(* Within an SP, RR, or WFQ node, sibling order carries no semantics: the
   discipline is defined by ranks, round-robin fairness, or per-arm weights,
   not by source position. [normalize] uses that arm-order freedom to pick a
   canonical sibling ordering, and the runtime echoes the normalized form
   back to the user as the committed shape. Edits that only permute siblings
   become no-ops after normalization (see "merely jumbled" tests in
   tests/planner/test_planner.ml); edits that reorder *and* add or remove an
   arm sniff as just the structural change, keeping the edit's footprint
   small. *)
let rec normalize p =
  match p with
  | FIFO _ -> p
  | SP (prs, designated) ->
      (* SP: rank ascending (the priority order is the discipline-defining
         datum); ties break by arm content. *)
      SP
        ( List.map (fun (p, r) -> (normalize p, r)) prs
          |> List.sort (fun (p1, r1) (p2, r2) ->
              let c = compare r1 r2 in
              if c <> 0 then c else compare p1 p2),
          designated )
  | RR ps -> RR (List.map normalize ps |> List.sort compare)
  | WFQ pws ->
      (* WFQ: arm content first (weights are independent shares; same-arm
         collisions are rare); ties break by weight. *)
      WFQ
        (List.map (fun (p, w) -> (normalize p, w)) pws
        |> List.sort (fun (p1, w1) (p2, w2) ->
            let c = compare p1 p2 in
            if c <> 0 then c else compare w1 w2))

(* Look up any variables and substitute them in. Then normalize the resulting policy. *)
let of_program (classes, assigns, ret) =
  let _, p = sub classes assigns [] ret in
  normalize p

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
      let to_string (p, w) = fmt "(%s, %g)" (to_string p) w in
      fmt "wfq[%s]" (join pws to_string)

let rec depth = function
  | FIFO _ -> 0
  | SP (prs, _) | WFQ prs ->
      1 + List.fold_left max 0 (List.map (fun (p, _) -> depth p) prs)
  | RR ps -> 1 + List.fold_left max 0 (List.map depth ps)

let rec walk p path =
  match (p, path) with
  | _, [] -> p
  | FIFO _, _ :: _ -> failwith "Pol.walk: path through FIFO leaf"
  | (SP (prs, _) | WFQ prs), i :: rest -> walk (fst (List.nth prs i)) rest
  | RR ps, i :: rest -> walk (List.nth ps i) rest

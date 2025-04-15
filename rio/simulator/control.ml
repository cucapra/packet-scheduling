open Frontend

let ( let* ) = Option.bind
let fmt = Printf.sprintf
let ( mod ) a b = (a mod b) + if a < 0 then b else 0

module type Control = sig
  type t

  val init : t
  val push : t -> Packet.t -> t
  val pop : t -> (Packet.t * t) option
end

module type Policy = sig
  val policy : Policy.t
end

type addr =
  | Eps
  | Ptr of int * addr

let rec addr_to_string = function
  | Eps -> "ε"
  | Ptr (i, t) -> fmt "%d ∙ %s" i (addr_to_string t)

let route_pkt (policy : Policy.t) pkt =
  let rec route_pkt_aux (p : Policy.t) pt =
    match p with
    | (FIFO cs | EDF cs) when List.exists (fun c -> Packet.flow pkt = c) cs ->
        Some (List.rev pt)
    | Strict ps | RR ps | WFQ (ps, _) ->
        List.find_mapi (fun i p -> route_pkt_aux p (i :: pt)) ps
    | FIFO _ | EDF _ -> None
  in

  match route_pkt_aux policy [] with
  | Some directions -> directions
  | None -> failwith (fmt "ERROR: cannot route flow %s" (Packet.flow pkt))

module Make_PIFOControl (P : Policy) : Control = struct
  open P

  type t = {
    s : State.t;
    q : Packet.t Pifotree.t;
  }

  let state =
    let rec state_aux (p : Policy.t) addr s =
      let prefix = addr_to_string addr in

      let join ps s =
        let f (i, s) p = (i + 1, state_aux p (Ptr (i, addr)) s) in
        snd (List.fold_left f (0, s) ps)
      in

      match p with
      | RR ps ->
          List.mapi (fun i _ -> (fmt "%s_r_%d" prefix i, float_of_int i)) ps
          |> Fun.flip State.rebind_all s
          |> State.rebind (fmt "%s_turn" prefix) 0.0
          |> join ps
      | WFQ (ps, _) ->
          List.mapi (fun i _ -> (fmt "%s_f_%d" prefix i, 0.0)) ps
          |> Fun.flip State.rebind_all s
          |> join ps
      | Strict ps -> join ps s
      | FIFO _ | EDF _ -> s
    in

    state_aux policy Eps State.empty

  let z_pre_push s pkt =
    let rec z_pre_push_aux (p : Policy.t) directions addr s =
      let prefix = addr_to_string addr in

      match (p, directions) with
      | Strict ps, h :: t ->
          let pt, s' = z_pre_push_aux (List.nth ps h) t (Ptr (h, addr)) s in
          (Pifotree.Path (h, float_of_int h, pt), s')
      | RR ps, h :: t ->
          let n = ps |> List.length |> float_of_int in
          let r_var = fmt "%s_r_%d" prefix h in
          let r_i = State.lookup r_var s in
          let s' = State.rebind r_var (r_i +. n) s in
          let pt, s'' = z_pre_push_aux (List.nth ps h) t (Ptr (h, addr)) s' in
          (Pifotree.Path (h, r_i, pt), s'')
      | WFQ (ps, ws), h :: t ->
          let f_var = fmt "%s_f_%d" prefix h in
          let rank = max (Packet.time pkt) (State.lookup f_var s) in
          let pkt_len = Packet.len pkt in
          let s' = State.rebind f_var (rank +. (pkt_len /. List.nth ws h)) s in
          let pt, s'' = z_pre_push_aux (List.nth ps h) t (Ptr (h, addr)) s' in
          (Pifotree.Path (h, rank, pt), s'')
      | FIFO _, [] -> (Pifotree.Foot 0.0, s)
      | _ -> failwith "ERROR: unreachable branch"
    in

    z_pre_push_aux policy (route_pkt policy pkt) Eps s

  let z_post_pop s pkt =
    let rec z_post_pop_aux (p : Policy.t) directions addr s =
      let prefix = addr_to_string addr in

      match (p, directions) with
      | FIFO _, [] | EDF _, [] -> s
      | WFQ (ps, _), h :: t | Strict ps, h :: t ->
          z_post_pop_aux (List.nth ps h) t (Ptr (h, addr)) s
      | RR ps, h :: t ->
          let n = List.length ps in
          let turn, turn' =
            ( prefix |> fmt "%s_turn" |> Fun.flip State.lookup s,
              (h + 1) mod n |> float_of_int )
          in
          let who_skip =
            let rec who_skip_aux t acc =
              if t = h then acc else who_skip_aux ((t + 1) mod n) (t :: acc)
            in
            who_skip_aux (int_of_float turn) []
          in
          let increment s i =
            let r_var = fmt "%s_r_%d" prefix i in
            let r_i = s |> State.lookup r_var |> int_of_float in
            let r_i' = float_of_int (r_i + n) in
            State.rebind r_var r_i' s
          in
          let s' =
            s
            |> Fun.flip (List.fold_left increment) who_skip
            |> State.rebind (fmt "%s_turn" prefix) turn'
          in
          z_post_pop_aux (List.nth ps h) t (Ptr (h, addr)) s'
      | _ -> failwith "ERROR: unreachable branch"
    in

    z_post_pop_aux policy (route_pkt policy pkt) Eps s

  let init =
    { s = state; q = policy |> Topo.of_policy Topo.Enq |> Pifotree.create }

  let push t pkt =
    let pt, s' = z_pre_push t.s pkt in
    let q' = Pifotree.push t.q pkt pt in
    { q = q'; s = s' }

  let pop t =
    let* pkt, q' = Pifotree.pop t.q in
    let s' = z_post_pop t.s pkt in
    Some (pkt, { q = q'; s = s' })
end

module Make_RioControl (P : Policy) : Control = struct
  open P

  type t = {
    s : State.t;
    q : (Packet.t, Ast.clss) Riotree.t;
  }

  let state =
    let rec state_aux (p : Policy.t) addr s =
      let prefix = addr_to_string addr in

      let join ps s =
        let f (i, s) p = (i + 1, state_aux p (Ptr (i, addr)) s) in
        snd (List.fold_left f (0, s) ps)
      in

      match p with
      | RR ps -> State.rebind (fmt "%s_turn" prefix) 0.0 s |> join ps
      | Strict ps -> join ps s
      | FIFO _ | EDF _ -> s
      | _ -> failwith "WFQ NOT DONE YET"
    in

    state_aux policy Eps State.empty

  let z_pre_push s pkt =
    let rec z_pre_push_aux (p : Policy.t) directions addr s =
      match (p, directions) with
      | Strict ps, h :: t | RR ps, h :: t ->
          z_pre_push_aux (List.nth ps h) t (Ptr (h, addr)) s
      | FIFO _, [] -> (0.0, s)
      | _ -> failwith "ERROR: unreachable branch"
    in

    z_pre_push_aux policy (route_pkt policy pkt) Eps s

  let z_pre_pop s =
    let rec z_pre_pop_aux (p : Policy.t) addr s =
      let prefix = addr_to_string addr in

      let join compute_rank s ps =
        let f s (i, r, p) =
          let order, s' = z_pre_pop_aux p (Ptr (i, addr)) s in
          (s', (order, r))
        in
        let s', order =
          ps
          |> List.mapi (fun i p -> (i, compute_rank i, p))
          |> List.fold_left_map f s
        in
        (Riotree.Order order, s')
      in

      match p with
      | RR ps ->
          let turn = State.lookup (fmt "%s_turn" prefix) s in
          let compute_rank i =
            float_of_int i -. turn
            |> int_of_float
            |> Fun.flip ( mod ) (List.length ps)
            |> float_of_int
          in
          join compute_rank s ps
      | Strict ps -> join float_of_int s ps
      | FIFO _ | EDF _ -> (Foot, s)
      | _ -> failwith "WFQ NOT READY YET"
    in

    z_pre_pop_aux policy Eps s

  let z_post_pop s pkt =
    let rec z_pre_pop_aux (p : Policy.t) directions addr s =
      let prefix = addr_to_string addr in

      match (p, directions) with
      | FIFO _, [] | EDF _, [] -> s
      | Strict ps, h :: t -> z_pre_pop_aux (List.nth ps h) t (Ptr (h, addr)) s
      | RR ps, h :: t ->
          let turn_var = fmt "%s_turn" prefix in
          let turn' = (h + 1) mod List.length ps |> float_of_int in
          let s' = State.rebind turn_var turn' s in
          z_pre_pop_aux (List.nth ps h) t (Ptr (h, addr)) s'
      | _ -> failwith "ERROR: unreachable branch"
    in

    z_pre_pop_aux policy (route_pkt policy pkt) Eps s

  let init =
    {
      s = state;
      q =
        policy |> Topo.of_policy Topo.Deq |> Fun.flip Riotree.create Packet.flow;
    }

  let push t pkt =
    let r, s' = z_pre_push t.s pkt in
    let q' = Riotree.push t.q pkt r in
    { q = q'; s = s' }

  let pop t =
    let order, s' = z_pre_pop t.s in
    let* pkt, q' = Riotree.pop t.q order in
    let s'' = z_post_pop s' pkt in
    Some (pkt, { q = q'; s = s'' })
end

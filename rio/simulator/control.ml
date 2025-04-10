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
  | Ptr (i, t) -> Printf.sprintf "%d ∙ %s" i (addr_to_string t)

let route_pkt (policy : Policy.t) pkt =
  let rec route_pkt_aux (p : Policy.t) pt =
    match p with
    | (Fifo cs | EDF cs) when List.exists (fun c -> Packet.flow pkt = c) cs ->
        Some (List.rev pt)
    | RoundRobin ps | Strict ps ->
        List.find_mapi (fun i p -> route_pkt_aux p (i :: pt)) ps
    | Fifo _ | EDF _ -> None
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
      | RoundRobin ps ->
          List.mapi (fun i _ -> (fmt "%s_r_%d" prefix i, i)) ps
          |> Fun.flip State.rebind_all s
          |> State.rebind (fmt "%s_turn" prefix) 0
          |> join ps
      | Strict ps -> join ps s
      | Fifo _ | EDF _ -> s
    in

    state_aux policy Eps State.empty

  let z_pre_push s pkt =
    let rec z_pre_push_aux (p : Policy.t) directions addr s =
      let prefix = addr_to_string addr in

      match (p, directions) with
      | Strict ps, h :: t ->
          let pt, s' = z_pre_push_aux (List.nth ps h) t (Ptr (h, addr)) s in
          (Pifotree.Path (h, h, pt), s')
      | RoundRobin ps, h :: t ->
          let r_i = fmt "%s_r_%d" prefix h in
          let rank = State.lookup r_i s in
          let s' = State.rebind r_i (rank + List.length ps) s in
          let pt, s'' = z_pre_push_aux (List.nth ps h) t (Ptr (h, addr)) s' in
          (Pifotree.Path (h, rank, pt), s'')
      | Fifo _, [] -> (Pifotree.Foot 0, s)
      | _ -> failwith "ERROR: unreachable branch"
    in

    z_pre_push_aux policy (route_pkt policy pkt) Eps s

  let z_post_pop s pkt =
    let rec z_post_pop_aux (p : Policy.t) directions addr s =
      let prefix = addr_to_string addr in

      match (p, directions) with
      | Fifo _, [] | EDF _, [] -> s
      | Strict ps, h :: t -> z_post_pop_aux (List.nth ps h) t (Ptr (h, addr)) s
      | RoundRobin ps, h :: t ->
          let n = List.length ps in
          let turn = State.lookup (fmt "%s_turn" prefix) s in
          let who_skip =
            let rec who_skip_aux t acc =
              if t = h then acc else who_skip_aux ((t + 1) mod n) (t :: acc)
            in
            who_skip_aux turn []
          in
          let increment s i =
            let r_i = fmt "%s_r_%d" prefix i in
            State.rebind r_i (State.lookup r_i s + n) s
          in
          let s' =
            s
            |> Fun.flip (List.fold_left increment) who_skip
            |> State.rebind (fmt "%s_turn" prefix) ((h + 1) mod n)
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
      | RoundRobin ps -> State.rebind (fmt "%s_turn" prefix) 0 s |> join ps
      | Strict ps -> join ps s
      | Fifo _ | EDF _ -> s
    in

    state_aux policy Eps State.empty

  let z_pre_push s pkt =
    let rec z_pre_push_aux (p : Policy.t) directions addr s =
      match (p, directions) with
      | Strict ps, h :: t | RoundRobin ps, h :: t ->
          z_pre_push_aux (List.nth ps h) t (Ptr (h, addr)) s
      | Fifo _, [] -> (0, s)
      | _ -> failwith "ERROR: unreachable branch"
    in

    z_pre_push_aux policy (route_pkt policy pkt) Eps s

  let z_pre_pop s =
    let rec z_pre_pop_aux (p : Policy.t) addr s =
      let prefix = addr_to_string addr in

      let join (compute_rank : int -> int) s ps =
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
      | RoundRobin ps ->
          let turn = State.lookup (fmt "%s_turn" prefix) s in
          let compute_rank i = (i - turn) mod List.length ps in
          join compute_rank s ps
      | Strict ps -> join Fun.id s ps
      | Fifo _ | EDF _ -> (Foot, s)
    in

    z_pre_pop_aux policy Eps s

  let z_post_pop s pkt =
    let rec z_pre_pop_aux (p : Policy.t) directions addr s =
      let prefix = addr_to_string addr in

      match (p, directions) with
      | Fifo _, [] | EDF _, [] -> s
      | Strict ps, h :: t -> z_pre_pop_aux (List.nth ps h) t (Ptr (h, addr)) s
      | RoundRobin ps, h :: t ->
          let turn = fmt "%s_turn" prefix in
          let s' =
            s |> fun s ->
            State.lookup turn s + 1 |> Fun.flip (State.rebind turn) s
          in
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
    let s' = z_post_pop s' pkt in
    Some (pkt, { q = q'; s = s' })
end

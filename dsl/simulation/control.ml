type t = {
  s : State.t;
  q : Pieotree.t;
  z_in : State.t -> Packet.t -> Path.t * State.t * Time.t;
  z_out : State.t -> Packet.t -> State.t;
}

type addr =
  | Eps
  | Cons of int * addr

exception RoutingError of Packet.t

let sprintf = Printf.sprintf

let rec addr_to_string = function
  | Eps -> "ε"
  | Cons (i, addr) -> Printf.sprintf "%d ∙ %s" i (addr_to_string addr)

let init_state p =
  let rec init_state_aux (p : Frontend.Policy.t) addr s =
    let join plst addr s =
      let f (i, s) p = (i + 1, init_state_aux p (Cons (i, addr)) s) in
      match List.fold_left f (0, s) plst with
      | _, s' -> s'
    in

    let prefix = addr_to_string addr in

    match p with
    | Class _ -> s
    | RoundRobin plst ->
        let ranks =
          List.mapi
            (fun i _ -> (sprintf "%s_r_%d" prefix i, float_of_int i))
            plst
        in
        s
        |> State.rebind (sprintf "%s_turn" prefix) 0.0
        |> State.rebind_all ranks |> join plst addr
    | WeightedFair wplst ->
        let weights =
          List.mapi (fun i (_, w) -> (sprintf "%s_w_%d" prefix i, w)) wplst
        in
        s |> State.rebind_all weights |> join (List.map fst wplst) addr
    | Fifo plst | Strict plst -> join plst addr s
  in
  let buckets = (p |> Topo.of_policy |> Topo.size) * 10 in
  init_state_aux p Eps (State.create buckets)

let route_pkt_opt p pkt =
  let rec route_pkt_opt_aux (p : Frontend.Policy.t) pt pkt =
    match p with
    | Class c -> if Packet.flow pkt = c then Some (List.rev pt) else None
    | Fifo plst | RoundRobin plst | Strict plst ->
        List.find_mapi (fun i p -> route_pkt_opt_aux p (i :: pt) pkt) plst
    | WeightedFair wplst ->
        List.find_mapi (fun i (p, _) -> route_pkt_opt_aux p (i :: pt) pkt) wplst
  in
  route_pkt_opt_aux p [] pkt

let z_in p s pkt =
  let rec z_in_aux (p : Frontend.Policy.t) rank_less_path addr s pkt =
    let prefix = addr_to_string addr in

    match (p, rank_less_path) with
    | Class _, [] ->
        ([ (Path.foot, Rank.create_for_pkt 0.0 pkt) ], s, Time.epoch)
    | Fifo plst, h :: t ->
        let pt, s', time =
          z_in_aux (List.nth plst h) t (Cons (h, addr)) s pkt
        in
        ((h, Rank.create_for_pkt 0.0 pkt) :: pt, s', time)
    | Strict plst, h :: t ->
        let pt, s', time =
          z_in_aux (List.nth plst h) t (Cons (h, addr)) s pkt
        in
        ((h, Rank.create_for_pkt (float_of_int h) pkt) :: pt, s', time)
    | RoundRobin plst, h :: t ->
        let n = List.length plst in
        let r_i = sprintf "%s_r_%d" prefix h in
        let rank = State.lookup r_i s in
        let s' = State.rebind r_i (rank +. float_of_int n) s in
        let pt, s'', time =
          z_in_aux (List.nth plst h) t (Cons (h, addr)) s' pkt
        in
        ((h, Rank.create_for_pkt rank pkt) :: pt, s'', time)
    | WeightedFair plst, h :: t ->
        let lf, w = (sprintf "%s_lf_%d" prefix h, sprintf "%s_w_%d" prefix h) in
        let weight = State.lookup w s in
        let rank =
          match State.lookup_opt lf s with
          | Some v -> max (pkt |> Packet.time |> Time.to_float) v
          | None -> pkt |> Packet.time |> Time.to_float
        in
        let s' = State.rebind lf (rank +. (Packet.len pkt /. weight)) s in
        let pt, s'', time =
          z_in_aux (List.nth plst h |> fst) t (Cons (h, addr)) s' pkt
        in
        ((h, Rank.create_for_pkt rank pkt) :: pt, s'', time)
    | _ -> failwith "ERROR: unreachable branch"
  in
  match route_pkt_opt p pkt with
  | Some rank_less_path -> z_in_aux p rank_less_path Eps s pkt
  | None -> raise (RoutingError pkt)

let z_out p s pkt =
  let rec z_out_aux (p : Frontend.Policy.t) rank_less_path addr s pkt =
    let prefix = addr_to_string addr in

    match (p, rank_less_path) with
    | Class _, [] -> s
    | Fifo plst, h :: t | Strict plst, h :: t ->
        z_out_aux (List.nth plst h) t (Cons (h, addr)) s pkt
    | WeightedFair plst, h :: t ->
        z_out_aux (List.nth plst h |> fst) t (Cons (h, addr)) s pkt
    | RoundRobin plst, h :: t ->
        let n = List.length plst in
        let who_skip pop turn =
          let rec who_skip_aux t acc =
            if t = pop then acc else who_skip_aux ((t + 1) mod n) (t :: acc)
          in
          who_skip_aux turn []
        in
        let turn = State.lookup (sprintf "%s_turn" prefix) s in
        let s' =
          State.rebind (sprintf "%s_turn" prefix)
            ((h + 1) mod n |> float_of_int)
            s
        in
        let skipped = who_skip h (int_of_float turn) in
        let f s i =
          let r_i = sprintf "%s_r_%d" prefix i in
          State.rebind r_i (State.lookup r_i s +. float_of_int n) s
        in
        let s'' = List.fold_left f s' skipped in
        z_out_aux (List.nth plst h) t (Cons (h, addr)) s'' pkt
    | _ -> failwith "ERROR: unreachable branch"
  in
  match route_pkt_opt p pkt with
  | Some rank_less_path -> z_out_aux p rank_less_path Eps s pkt
  | None -> raise (RoutingError pkt)

let of_policy p =
  {
    q = p |> Topo.of_policy |> Pieotree.create;
    s = init_state p;
    z_in = z_in p;
    z_out = z_out p;
  }

let to_topo c = Pieotree.to_topo c.q

let compile (topo, map) c =
  let z_in' s pkt =
    let f_tilde = Topo.lift_tilde map (to_topo c) in
    let pt, s', ts = c.z_in s pkt in
    (f_tilde pt, s', ts)
  in
  {
    q = Pieotree.create topo;
    s = State.clone c.s;
    z_in = z_in';
    z_out = c.z_out;
  }

type t = {
  s : State.t;
  q : Pieotree.t;
  z_in : State.t -> Packet.t -> Path.t * State.t * Time.t;
  z_out : State.t -> Packet.t -> State.t;
}

let find_mapi f lst =
  let rec aux i = function
    | [] -> None
    | x :: xs ->
      match f i x with
      | Some _ as result -> result
      | None -> aux (i + 1) xs
  in
  aux 0 lst


let sprintf = Printf.sprintf

(* `init_state_of_policy p` is `(of_policy p).s`, i.e. the initial state for
   `p`'s control.

   `init_state_of_policy` initalizes values bound to keys managed by each node
   of `p` according to the semantics of their policies. To prevent name clashes,
   each key is prefixed with the address of its associated node. *)
let init_state_of_policy p =
  let rec init_state_of_policy_aux (p : Frontend.Policy.t) addr s =
    let join plst addr s =
      let f (i, s) p = (i + 1, init_state_of_policy_aux p (i :: addr) s) in
      match List.fold_left f (0, s) plst with
      | _, s' -> s'
    in

    let prefix = Topo.addr_to_string addr in

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
  init_state_of_policy_aux p [] State.empty

let route_pkt p pkt =
  let rec route_pkt_aux (p : Frontend.Policy.t) pt =
    match p with
    | Class c -> if Packet.flow pkt = c then Some (List.rev pt) else None
    | Fifo plst | RoundRobin plst | Strict plst ->
        find_mapi (fun i p -> route_pkt_aux p (i :: pt)) plst
    | WeightedFair wplst ->
        find_mapi (fun i (p, _) -> route_pkt_aux p (i :: pt)) wplst
  in
  match route_pkt_aux p [] with
  | Some rankless_path -> rankless_path
  | None -> failwith (sprintf "ERROR: cannot route flow %s" (Packet.flow pkt))

(* `z_in_of_policy p` is `(of_policy p).z_in`, i.e. the _scheduling transaction_
   for `p`'s control.

   More specifically, consider packet `pkt`, belonging to the flow living at
   `p`'s leaf with address `i · _`. Call `p`'s `i`th child policy `p_i`.
   We define `z_in_of_policy p s pkt` inductively:
   - Update `s` to `s'` and compute the readiness time `t` and rank `r`, as per
     the semantics of the policy at `p`'s root.
   - Next, recursively compute `z_in_of_policy p_i s' pkt` = (path, s'', t') and
     return `((i, r) :: path, s'', max t t')`
   Instead, if `p` has no children, i.e. `pkt` belongs to the flow living at
   address `ε`, then let `z_in_of_policy p s pkt` be
    `([ (_, pkt's arrival time) ], s, -\infty)`.
*)
let z_in_of_policy p s pkt =
  let rec z_in_of_policy_aux (p : Frontend.Policy.t) rankless_path addr s =
    let prefix = Topo.addr_to_string addr in

    match (p, rankless_path) with
    | Class _, [] ->
        ([ (Path.foot, Rank.create_for_pkt 0.0 pkt) ], s, Time.epoch)
    | Fifo plst, h :: t ->
        let pt, s', time =
          z_in_of_policy_aux (List.nth plst h) t (h :: addr) s
        in
        ((h, Rank.create_for_pkt 0.0 pkt) :: pt, s', time)
    | Strict plst, h :: t ->
        let pt, s', time =
          z_in_of_policy_aux (List.nth plst h) t (h :: addr) s
        in
        ((h, Rank.create_for_pkt (float_of_int h) pkt) :: pt, s', time)
    | RoundRobin plst, h :: t ->
        let n = List.length plst in
        let r_i = sprintf "%s_r_%d" prefix h in
        let rank = State.lookup r_i s in
        let s' = State.rebind r_i (rank +. float_of_int n) s in
        let pt, s'', time =
          z_in_of_policy_aux (List.nth plst h) t (h :: addr) s'
        in
        ((h, Rank.create_for_pkt rank pkt) :: pt, s'', time)
    | WeightedFair plst, h :: t ->
        let lf, w = (sprintf "%s_lf_%d" prefix h, sprintf "%s_w_%d" prefix h) in
        let weight = State.lookup w s in
        let rank =
          let time = pkt |> Packet.time |> Time.to_float in
          match State.lookup_opt lf s with
          | Some v -> max time v
          | None -> time
        in
        let s' = State.rebind lf (rank +. (Packet.len pkt /. weight)) s in
        let pt, s'', time =
          z_in_of_policy_aux (List.nth plst h |> fst) t (h :: addr) s'
        in
        ((h, Rank.create_for_pkt rank pkt) :: pt, s'', time)
    | _ -> failwith "ERROR: unreachable branch"
  in
  z_in_of_policy_aux p (route_pkt p pkt) [] s

(* `z_out_of_policy p` is `(of_policy p).z_out`, i.e. the state updating
   function at dequeue for `p`'s control.

   More specifically, consider packet `pkt`, belonging to the flow living at
   `p`'s leaf with address `i · _`. Call `p`'s `i`th child policy `p_i`.
   We define state `z_out_of_policy p s pkt` inductively:
   - Update `s` to `s'`, as per the semantics of the policy at `p`'s root.
   - Next, recursively compute `z_out_of_policy p_i s' pkt`
   Instead, if `p` has no children, i.e. `pkt` belongs to the flow living at
   address `ε`, then let `z_out_of_policy p s pkt` be `s`. *)
let z_out_of_policy p s pkt =
  let rec z_out_of_policy_aux (p : Frontend.Policy.t) rankless_path addr s =
    let prefix = Topo.addr_to_string addr in

    match (p, rankless_path) with
    | Class _, [] -> s
    | Fifo plst, h :: t | Strict plst, h :: t ->
        z_out_of_policy_aux (List.nth plst h) t (h :: addr) s
    | WeightedFair plst, h :: t ->
        z_out_of_policy_aux (List.nth plst h |> fst) t (h :: addr) s
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
        z_out_of_policy_aux (List.nth plst h) t (h :: addr) s''
    | _ -> failwith "ERROR: unreachable branch"
  in
  z_out_of_policy_aux p (route_pkt p pkt) [] s

let of_policy p =
  {
    q = p |> Topo.of_policy |> Pieotree.create;
    s = init_state_of_policy p;
    z_in = z_in_of_policy p;
    z_out = z_out_of_policy p;
  }

let to_topo c = Pieotree.to_topo c.q

let compile (topo, map) c =
  let z_in' s pkt =
    let f_tilde = Topo.lift_tilde map (to_topo c) in
    let pt, s', ts = c.z_in s pkt in
    (f_tilde pt, s', ts)
  in
  { q = Pieotree.create topo; s = c.s; z_in = z_in'; z_out = c.z_out }

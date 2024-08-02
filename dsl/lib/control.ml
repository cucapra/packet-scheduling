type t = {
  s : State.t;
  q : Pieotree.t;
  z_in : State.t -> Packet.t -> Path.t * State.t * Time.t;
  z_out : State.t -> Packet.t -> State.t;
}

type addr = Eps | Cons of int * addr

let rec addr_to_string = function
  | Eps -> "ε"
  | Cons (i, addr) -> Printf.sprintf "%d ∙ %s" i (addr_to_string addr)

let rec init_state_of_policy (p : Policy.t) s addr =
  let join plst addr s =
    let f (i, s) p = (i + 1, init_state_of_policy p s (Cons (i, addr))) in
    match List.fold_left f (0, s) plst with _, s' -> s'
  in

  let prefix = addr_to_string addr in

  match p with
  | Class _ -> s
  | Fifo plst | Strict plst -> join plst addr s
  | RoundRobin plst ->
      let init_rank_ptrs =
        List.mapi
          (fun i _ -> (Printf.sprintf "%s_r_%d" prefix i, float_of_int i))
          plst
      in
      s
      |> State.rebind (Printf.sprintf "%s_turn" prefix) 0.0
      |> State.rebind_all init_rank_ptrs
      |> join plst addr
  | WeightedFair wplst ->
      let weights =
        List.mapi
          (fun i (_, w) -> (Printf.sprintf "%s_w_%d" prefix i, float_of_int w))
          wplst
      in
      s |> State.rebind_all weights |> join (List.map fst wplst) addr
  | _ -> failwith "TBD"

let z_in_of_policy _ = failwith "TBD"
let z_out_of_policy _ = failwith "TBD"

let of_policy p =
  {
    q = p |> Topo.of_policy |> Pieotree.create;
    s = init_state_of_policy p (State.create 10) Eps;
    z_in = z_in_of_policy p;
    z_out = z_out_of_policy p;
  }

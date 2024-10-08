type t = {
  s : State.t;
  q : Pieotree.t;
  z_in : State.t -> Packet.t -> Path.t * State.t * Time.t;
  z_out : State.t -> Packet.t -> State.t;
}

val of_policy : Frontend.Policy.t -> t
val to_topo : t -> Topo.t
val compile : Topo.t * Topo.map -> t -> t

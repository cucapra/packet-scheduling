module type Parameters = sig
  val sim_len : float
  val sleep : float
  val pop_tick : float
end

module type Sim = sig
  val simulate : Packet.t list -> Packet.t list
end

module Make_Sim (_ : Control.Control) (_ : Parameters) : Sim

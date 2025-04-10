module type Parameters = sig
  val sim_len : float
  val sleep : float
  val pop_tick : float
end

module type Sim = sig
  val simulate : Packet.t list -> Packet.t list
end

module Make_Sim (C : Control.Control) (P : Parameters) : Sim = struct
  open P

  let simulate packets =
    let start_time = Packet.time (List.hd packets) in
    let end_time = start_time +. sim_len in

    let rec simulate_aux packets time tsp ctrl popped =
      match (packets, time >= end_time, tsp >= pop_tick) with
      (* sim over *)
      | _, true, _ -> popped
      (* pop time! *)
      | _, false, true when C.pop ctrl = None ->
          simulate_aux packets time 0.0 ctrl popped
      | _, false, true ->
          let[@warning "-8"] (Some (pkt, ctrl')) = C.pop ctrl in
          let pkt' = Packet.punch_out pkt time in
          simulate_aux packets time 0.0 ctrl' (pkt' :: popped)
      (* push time! *)
      | [], false, false ->
          simulate_aux [] (time +. sleep) (tsp +. sleep) ctrl popped
      | pkt :: _, false, false when time < Packet.time pkt ->
          simulate_aux packets (time +. sleep) (tsp +. sleep) ctrl popped
      | pkt :: t, false, false ->
          let pkt' = Packet.punch_in pkt time in
          simulate_aux t time tsp (C.push ctrl pkt') popped
    in

    simulate_aux packets start_time 0.0 C.init []
end

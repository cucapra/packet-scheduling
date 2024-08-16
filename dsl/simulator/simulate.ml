let simulate sim_length sleep pop_tick flow (ctrl : Control.t) =
  (* The user gives us:
     - sim_length: after how many seconds to stop simulating.
     - sleep: how long to sleep when there's no work to do.
     - pop_tick: a threshold for when next to try a pop.
     - flow: a list of packets to simulate.
     - ctrl: the ctrl to simulate over.

      We assume that flow is ordered by packet time. We start the simulation at
      the time of the first packet in flow. We simulate for `sim_length` seconds.

      We need to become sensitive to _time_. We cannot just push packets as fast
      as possible, and we cannot pop the tree as fast as possible.

      A packet can be pushed only once its time has arrived. For instance, if
      packet `n` is registered in flow as arriving 5 seconds after the first
      packet, it will only be pushed into the tree 5 (or more) seconds after the
      simulation starts. The tree can be popped only if the time since the last
      pop is greater than `pop_tick`. This allows us to play with `pop_tick` and
      therefore saturate the tree. *)
  let start_time = Packet.time (List.hd flow) in
  let end_time = Time.add_float start_time sim_length in

  let rec helper flow time tsp state tree ans =
    if time >= end_time then (
      if flow <> [] then
        Printf.printf
          "Warning: not every packet was pushed at the time simulation ended. \
           The flow has %d packet(s).\n"
          (List.length flow);
      let size = Pieotree.size tree Time.terminus in
      if size > 0 then
        Printf.printf
          "Warning: not every packet was popped at the time simulation ended. \
           The tree has %d packet(s).\n"
          size;
      List.rev ans)
    else if tsp >= pop_tick then
      if Pieotree.size tree time = 0 then helper flow time 0.0 state tree ans
      else
        match Pieotree.pop tree time with
        | None -> failwith "The tree was nonempty, but pop returned None."
        | Some (pkt, tree') ->
            let state' = ctrl.z_out state pkt in
            helper flow time 0.0 state' tree' (Packet.punch_out pkt time :: ans)
    else
      match flow with
      | [] ->
          helper flow (Time.add_float time sleep) (tsp +. sleep) state tree ans
      | pkt :: flow' ->
          if time >= Packet.time pkt then
            let path, state', ts = ctrl.z_in state pkt in
            let tree' = Pieotree.push tree ts (Packet.punch_in pkt time) path in
            helper flow' time tsp state' tree' ans
          else
            helper flow
              (Time.add_float time sleep)
              (tsp +. sleep) state tree ans
  in
  helper flow start_time 0.0 ctrl.s ctrl.q []

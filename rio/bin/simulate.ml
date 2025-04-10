open Frontend
open Simulator

let () =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "usage: ./%s path/to/prog path/to/pcap\n" Sys.argv.(0);
    exit 1)
  else ()

let prog_path, pcap_path = (Sys.argv.(1), Sys.argv.(2))

let prog_name, pcap_name =
  let file_name = Fun.compose Filename.remove_extension Filename.basename in
  (file_name prog_path, file_name pcap_path)

let prog, pcap =
  ( prog_path |> Parser.parse_file |> Policy.of_program,
    Packet.pkts_from_file pcap_path )

module Pol : Control.Policy = struct
  let policy = prog
end

module PIFOCtrl : Control.Control = Control.Make_PIFOControl (Pol)
module RioCtrl : Control.Control = Control.Make_RioControl (Pol)

module Params : Simulate.Parameters = struct
  let sim_len = 30.0
  let sleep = 0.001
  let pop_tick = 0.25
end

module PIFOSim : Simulate.Sim = Simulate.Make_Sim (PIFOCtrl) (Params)
module RioSim : Simulate.Sim = Simulate.Make_Sim (RioCtrl) (Params)

let () =
  pcap |> PIFOSim.simulate
  |> Fun.flip Packet.write_to_csv
       (Printf.sprintf "../graphs/enq_%s_%s.csv" prog_name pcap_name)

let () =
  pcap |> RioSim.simulate
  |> Fun.flip Packet.write_to_csv
       (Printf.sprintf "../graphs/deq_%s_%s.csv" prog_name pcap_name)

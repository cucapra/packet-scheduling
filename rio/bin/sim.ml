open Frontend
open Simulator

type enqdeq =
  | RIO
  | PIFO

let error () =
  Printf.printf "Usage: ./%s <prog> <pcap> <RIO|PIFO> \n" Sys.argv.(0);
  exit 1

let root_dir = "../"
let graph_dir = root_dir ^ "graphs/"
let parse_prog path = path |> Parser.parse_file |> Policy.of_program
let parse_pcap path = path |> Packet.pkts_from_file

let prog_path, pcap_path, enqdeq =
  match Array.length Sys.argv with
  | 4 ->
      ( Sys.argv.(1),
        Sys.argv.(2),
        match Sys.argv.(3) with
        | "RIO" -> RIO
        | "PIFO" -> PIFO
        | _ -> error () )
  | _ -> error ()

module Params : Simulate.Parameters = struct
  let sim_len = 30.0
  let sleep = 0.001
  let pop_tick = 0.25
end

let run () =
  let prog_no_ext, pcap_no_ext =
    ( prog_path |> Filename.basename |> Filename.remove_extension,
      prog_path |> Filename.basename |> Filename.remove_extension )
  in
  let prog, pcap = (parse_prog prog_path, parse_pcap pcap_path) in

  let module Pol : Control.Policy = struct
    let policy = prog
  end in
  match enqdeq with
  | PIFO ->
      let module PIFOCtrl : Control.Control = Control.Make_PIFOControl (Pol) in
      let module PIFOSim : Simulate.Sim = Simulate.Make_Sim (PIFOCtrl) (Params)
      in
      let pops = PIFOSim.simulate pcap in
      Packet.write_to_csv pops
        (Printf.sprintf "%senq_%s_%s.csv" graph_dir prog_no_ext pcap_no_ext)
  | RIO ->
      let module RioCtrl : Control.Control = Control.Make_RioControl (Pol) in
      let module RioSim : Simulate.Sim = Simulate.Make_Sim (RioCtrl) (Params) in
      let pops = RioSim.simulate pcap in
      Packet.write_to_csv pops
        (Printf.sprintf "%sdeq_%s_%s.csv" graph_dir prog_no_ext pcap_no_ext)

let () = run ()

open Frontend
open Simulator

let () =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "usage: ./%s path/to/pcap" Sys.argv.(0);
    exit 1)
  else ()

let policy = Sys.argv.(1) |> Parser.parse_file |> Policy.of_program
let _ = "prog : " ^ (policy |> Policy.to_string) |> print_endline
let pcap = Sys.argv.(2) |> Packet.pkts_from_file

module P : Control.Policy = struct
  let policy = policy
end

module C : Control.Control = Control.Make_PIFOControl (P)

module Params : Simulate.Parameters = struct
  let sim_len = 30.0
  let sleep = 0.001
  let pop_tick = 0.25
end

module S : Simulate.Sim = Simulate.Make_Sim (C) (Params)

let () = pcap |> S.simulate |> Fun.flip Packet.write_to_csv "pcap.csv"

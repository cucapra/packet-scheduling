open Frontend
open Simulation

let prefix = "../../../../"
let parse filename = prefix ^ filename |> Parser.parse_file |> Policy.of_program
let compute_control filename = filename |> parse |> Control.of_policy
let parse_pcap s = Packet.pkts_from_file (prefix ^ s)

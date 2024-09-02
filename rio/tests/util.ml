open Frontend
open Simulator

let prefix = "../../../../"
let parse filename = prefix ^ filename |> Parser.parse_file |> Policy.of_program
let compute_ctrl filename = filename |> parse |> Control.of_policy
let parse_pcap s = Packet.pkts_from_file (prefix ^ s)

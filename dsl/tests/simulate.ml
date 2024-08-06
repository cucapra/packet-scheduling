open Frontend
open Simulation

let prefix = "../../../../"
let pkts_from_file s = Packet.pkts_from_file (prefix ^ "pcaps/" ^ s)

(* flows *)
let fcfs_flow = pkts_from_file "fcfs_generated.pcap"
let two_then_three = pkts_from_file "two_then_three.pcap"
let wfq_flow = pkts_from_file "wfq_generated.pcap"
let strict_flow = pkts_from_file "strict_generated.pcap"

let run prog flow name =
  let control =
    prefix ^ "progs/" ^ prog
    |> Parser.parse_file |> Policy.of_program |> Control.of_policy
  in
  let popped_pkts = Simulate.simulate 30.0 0.001 0.25 flow control in
  Packet.write_to_csv popped_pkts (Printf.sprintf "../../%s.csv" name)

let _ =
  run "work_conserving/fifo_n_classes.sched" fcfs_flow "fcfs";
  run "work_conserving/rr_n_classes.sched" two_then_three "rr";
  run "work_conserving/strict_n_classes.sched" strict_flow "strict";
  run "work_conserving/wfq_n_classes.sched" wfq_flow "wfq"

open Simulation
open OUnit2

let fcfs_flow, two_then_three, strict_flow, wfq_flow =
  Util.
    ( parse_pcap "pcaps/fcfs_generated.pcap",
      parse_pcap "pcaps/two_then_three.pcap",
      parse_pcap "pcaps/strict_generated.pcap",
      parse_pcap "pcaps/wfq_generated.pcap" )

let fifo, rr, strict, wfq =
  Util.
    ( compute_ctrl "progs/work_conserving/fifo_n_classes.sched",
      compute_ctrl "progs/work_conserving/rr_n_classes.sched",
      compute_ctrl "progs/work_conserving/strict_n_classes.sched",
      compute_ctrl "progs/work_conserving/wfq_n_classes.sched" )

let d_ary_ctrl d c =
  Control.compile (c |> Control.to_topo |> Topo.build_d_ary d) c

let fifo_bin, rr_bin, strict_bin, wfq_bin =
  (d_ary_ctrl 2 fifo, d_ary_ctrl 2 rr, d_ary_ctrl 2 strict, d_ary_ctrl 2 wfq)

let run control flow name =
  Packet.write_to_csv
    (Simulate.simulate 30.0 0.001 0.25 flow control)
    (Util.prefix ^ "graphs/" ^ name ^ ".csv")

let () =
  run fifo fcfs_flow "fcfs";
  run rr two_then_three "rr";
  run strict strict_flow "strict";
  run wfq wfq_flow "wfq"

let () =
  run fifo_bin fcfs_flow "fcfs_bin";
  run rr_bin two_then_three "rr_bin";
  run strict_bin strict_flow "strict_bin";
  run wfq_bin wfq_flow "wfq_bin"

let diff_test file file' =
  Printf.sprintf "%s = %s" file file' >:: fun ctxt ->
  assert_command ~ctxt "diff" [ Util.prefix ^ file; Util.prefix ^ file' ]

let diff_tests =
  [
    diff_test "graphs/fcfs.csv" "graphs/fcfs_bin.csv";
    diff_test "graphs/rr.csv" "graphs/rr_bin.csv";
    diff_test "graphs/strict.csv" "graphs/strict_bin.csv";
    diff_test "graphs/wfq.csv" "graphs/wfq_bin.csv";
  ]

let suite = "T2T compilation tests" >::: diff_tests
let () = run_test_tt_main suite

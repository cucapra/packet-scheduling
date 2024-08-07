open Simulation
open OUnit2

let fcfs_flow, two_then_three, wfq_flow, strict_flow =
  Util.
    ( parse_pcap "pcaps/fcfs_generated.pcap",
      parse_pcap "pcaps/two_then_three.pcap",
      parse_pcap "pcaps/wfq_generated.pcap",
      parse_pcap "pcaps/strict_generated.pcap" )

let fifo, rr, strict, wfq =
  Util.
    ( compute_control "progs/work_conserving/fifo_n_classes.sched",
      compute_control "progs/work_conserving/rr_n_classes.sched",
      compute_control "progs/work_conserving/strict_n_classes.sched",
      compute_control "progs/work_conserving/wfq_n_classes.sched" )

let run control flow name =
  Packet.write_to_csv
    (Simulate.simulate 30.0 0.001 0.25 flow control)
    (Util.prefix ^ "graphs/" ^ name ^ ".csv")

let run_on_binary control flow name =
  let control' =
    Control.compile control (control.q |> Pieotree.to_topo |> Topo.build_d_ary 2)
  in
  run control' flow name

let () =
  let dir = Util.prefix ^ "graphs" in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o777 else ()

let () =
  run fifo fcfs_flow "fcfs";
  run rr two_then_three "rr";
  run strict strict_flow "strict";
  run wfq wfq_flow "wfq"

let () =
  run_on_binary fifo fcfs_flow "fcfs_bin";
  run_on_binary rr two_then_three "rr_bin";
  run_on_binary strict strict_flow "strict_bin";
  run_on_binary wfq wfq_flow "wfq_bin"

let diff_test file file' =
  Printf.sprintf "%s = %s" file file' >:: fun ctxt ->
  assert_command ~ctxt "diff" [ Util.prefix ^ file; Util.prefix ^ file' ]

let suite =
  "T2T compilation tests"
  >::: [
         diff_test "graphs/fcfs.csv" "graphs/fcfs_bin.csv";
         diff_test "graphs/rr.csv" "graphs/rr_bin.csv";
         diff_test "graphs/strict.csv" "graphs/strict_bin.csv";
         diff_test "graphs/wfq.csv" "graphs/wfq_bin.csv";
       ]

let () = run_test_tt_main suite

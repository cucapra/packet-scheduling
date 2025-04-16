open Frontend
open Simulator
open OUnit2

let fmt = Printf.sprintf
let root_dir = "../../../../../"

let prog_dir, pcap_dir, graph_dir =
  (root_dir ^ "progs/enq-deq-equiv/", root_dir ^ "pcaps/", root_dir ^ "graphs/")

let parse_prog name = prog_dir ^ name |> Parser.parse_file |> Policy.of_program
let parse_pcap name = pcap_dir ^ name |> Packet.pkts_from_file

let progs_and_pcaps =
  (* just file names, not paths *)
  [
    ("fifo.sched", "fcfs_generated.pcap");
    ("rr_3_classes.sched", "two_then_three.pcap");
    ("strict_3_classes.sched", "two_then_three.pcap");
    ("wfq_3_classes.sched", "two_then_three.pcap");
    ("mix_5_classes.sched", "five_flows.pcap");
    ("mix_7_classes.sched", "seven_flows.pcap");
    ("mix_weight.sched", "seven_flows.pcap");
    ("bin_tree_pol.sched", "seven_flows.pcap");
  ]

let gen_csv = Sys.getenv_opt "GEN_CSV"

module Params : Simulate.Parameters = struct
  let sim_len = 30.0
  let sleep = 0.001
  let pop_tick = 0.25
end

let make_sim_test (prog_name, pcap_name) =
  let prog_no_ext, pcap_no_ext =
    (Filename.remove_extension prog_name, Filename.remove_extension pcap_name)
  in
  let prog, pcap = (parse_prog prog_name, parse_pcap pcap_name) in

  let module Pol : Control.Policy = struct
    let policy = prog
  end in
  let module PIFOCtrl : Control.Control = Control.Make_PIFOControl (Pol) in
  let module RioCtrl : Control.Control = Control.Make_RioControl (Pol) in
  let module PIFOSim : Simulate.Sim = Simulate.Make_Sim (PIFOCtrl) (Params) in
  let module RioSim : Simulate.Sim = Simulate.Make_Sim (RioCtrl) (Params) in
  let enq_pops, deq_pops = (PIFOSim.simulate pcap, RioSim.simulate pcap) in

  let () =
    match gen_csv with
    | Some "YES" ->
        Packet.write_to_csv enq_pops
          (fmt "%senq_%s_%s.csv" graph_dir prog_no_ext pcap_no_ext);
        Packet.write_to_csv deq_pops
          (fmt "%sdeq_%s_%s.csv" graph_dir prog_no_ext pcap_no_ext)
    | Some _ | None -> ()
  in

  prog_name ^ " over " ^ pcap_name >:: fun _ -> assert_equal enq_pops deq_pops

let suite = "enq-deq equiv tests" >::: List.map make_sim_test progs_and_pcaps
let () = run_test_tt_main suite

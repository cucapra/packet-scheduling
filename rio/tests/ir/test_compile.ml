open Rio_core
open Frontend
open OUnit2
open Ir

let prog_dir = "../progs/"

(* [.sched] file -> [Ir.commit]. Stops short of pretty-printing — that's
   [test_pretty]'s job. We project [Ir.of_policy]'s [compiled] result down to
   its [.commit] field; the [decorated] tree and counter snapshots are exercised
   in [test_patch]. *)
let compile filename =
  let c =
    prog_dir ^ filename |> Parser.parse_file |> Pol.of_program |> Ir.of_policy
  in
  c.commit

let make_test name filename (expected : commit) =
  name >:: fun _ ->
  assert_equal ~printer:Ir.string_of_commit expected (compile filename)

(* Single FIFO leaf [A] living on PE0 as v100, wrapped in the port root
   [v99] on PE -1. *)
let fifo_a_expected : commit =
  [
    Spawn (99, -1);
    Spawn (100, 0);
    Adopt (999, 99, 100);
    Assoc (99, "A");
    Assoc (100, "A");
    Map (99, "A", 999);
    Set_policy (99, RR, 1);
  ]

let strict_abc_expected : commit =
  [
    Spawn (99, -1);
    Spawn (100, 0);
    Spawn (101, 1);
    Spawn (102, 1);
    Spawn (103, 1);
    Adopt (999, 99, 100);
    Adopt (1000, 100, 101);
    Adopt (1001, 100, 102);
    Adopt (1002, 100, 103);
    Assoc (99, "A");
    Assoc (99, "B");
    Assoc (99, "C");
    Assoc (100, "A");
    Assoc (100, "B");
    Assoc (100, "C");
    Assoc (101, "A");
    Assoc (102, "B");
    Assoc (103, "C");
    Map (99, "A", 999);
    Map (99, "B", 999);
    Map (99, "C", 999);
    Map (100, "A", 1000);
    Map (100, "B", 1001);
    Map (100, "C", 1002);
    Set_policy (99, RR, 1);
    Set_policy (100, SP, 3);
    (* A is highest priority, then B, then C *)
    Set_arm_meta (100, 1000, 1.0);
    Set_arm_meta (100, 1001, 2.0);
    Set_arm_meta (100, 1002, 3.0);
  ]

let compile_tests =
  [
    make_test "fifo[A]" "work_conserving/fifo_A.sched" fifo_a_expected;
    make_test "strict[A, B, C]" "work_conserving/strict_ABC.sched"
      strict_abc_expected;
  ]

(* The remaining nested programs (complex_tree.sched, rr_strict_hier.sched)
   now compile too — adding goldens for them is a follow-up. *)
let suite = "compile tests" >::: compile_tests
let () = run_test_tt_main suite

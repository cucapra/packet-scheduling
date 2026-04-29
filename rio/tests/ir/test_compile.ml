open Frontend
open OUnit2
open Ir

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

(* [.sched] file -> [Ir.program]. Stops short of pretty-printing — that's
   [test_pretty]'s job. We project [Ir.of_policy]'s [compiled] result down to
   its [.prog] field; the [decorated] tree and counter snapshots are exercised
   in [test_patch]. *)
let compile filename =
  let c =
    prog_dir ^ filename |> Parser.parse_file |> Policy.of_program
    |> Ir.of_policy
  in
  c.prog

let make_test name filename (expected : program) =
  name >:: fun _ ->
  assert_equal ~printer:Ir.string_of_program expected (compile filename)

(* Single FIFO leaf [A] living on PE0 as v100, wrapped in the fake root
   [v99] on PE -1. *)
let fifo_a_expected : program =
  [
    Spawn (99, -1);
    Spawn (100, 0);
    Adopt (999, 99, 100);
    Assoc (99, "A");
    Assoc (100, "A");
    Map (99, "A", 999);
    Change_pol (99, UNION, 1);
  ]

let strict_abc_expected : program =
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
    Change_pol (99, UNION, 1);
    Change_pol (100, SP, 3);
    (* A is highest priority, then B, then C *)
    Change_weight (100, 1000, 1.0);
    Change_weight (100, 1001, 2.0);
    Change_weight (100, 1002, 3.0);
  ]

let compile_tests =
  [
    make_test "fifo[A]" "work_conserving/fifo_A.sched" fifo_a_expected;
    make_test "drop unused class" "work_conserving/drop_class.sched"
      fifo_a_expected;
    make_test "strict[A, B, C]" "work_conserving/strict_ABC.sched"
      strict_abc_expected;
  ]

(* The remaining nested programs (complex_tree.sched, rr_strict_hier.sched)
   now compile too — adding goldens for them is a follow-up. *)
let suite = "compile tests" >::: compile_tests
let () = run_test_tt_main suite

open Frontend
open OUnit2
open Ir

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

(* [.sched] file -> [Ir.program]. Stops short of pretty-printing — that's
   [test_pretty]'s job. We project [Ir.of_policy]'s [compiled] result down to
   its [.prog] field; the [identities] / counter snapshots are exercised in
   [test_json]. *)
let compile filename =
  let c =
    prog_dir ^ filename |> Parser.parse_file |> Policy.of_program
    |> Ir.of_policy
  in
  c.prog

let make_test name filename (expected : program) =
  name >:: fun _ ->
  assert_equal ~printer:Ir.string_of_program expected (compile filename)

(* -------- Expected [Ir.program]s for each supported shape. --------

   Numbering reminder:
   - PEs are depth-based, starting at 0 (root).
   - vPIFO IDs come from a counter starting at 100.
   - Step IDs come from a counter starting at 1000.
   - vPIFOs are allocated PRE-order: each subtree-root gets its ID before its
     children do, so the WFQ/RR/UNION/SP root sits at the lowest number in
     its subtree.
   - Step IDs are allocated at adopt-time (one per child of each parent).
     Children are compiled before the parent's adopts run, so inner-tree
     adopts claim the lower step IDs and the outer adopts come later. *)

(* Single FIFO leaf [A] living on PE0 as v100. *)
let fifo_a_expected : program = [ Spawn (100, 0); Assoc (100, "A") ]

let union_gh_expected : program =
  [
    Spawn (100, 0);
    Spawn (101, 1);
    Spawn (102, 1);
    Adopt (1000, 100, 101);
    Adopt (1001, 100, 102);
    Assoc (100, "G");
    Assoc (100, "H");
    Assoc (101, "G");
    Assoc (102, "H");
    Map (100, "G", 1000);
    Map (100, "H", 1001);
    Change_pol (100, UNION, 2);
  ]

let rr_abc_expected : program =
  [
    Spawn (100, 0);
    Spawn (101, 1);
    Spawn (102, 1);
    Spawn (103, 1);
    Adopt (1000, 100, 101);
    Adopt (1001, 100, 102);
    Adopt (1002, 100, 103);
    Assoc (100, "A");
    Assoc (100, "B");
    Assoc (100, "C");
    Assoc (101, "A");
    Assoc (102, "B");
    Assoc (103, "C");
    Map (100, "A", 1000);
    Map (100, "B", 1001);
    Map (100, "C", 1002);
    Change_pol (100, RR, 3);
  ]

let strict_abc_expected : program =
  [
    Spawn (100, 0);
    Spawn (101, 1);
    Spawn (102, 1);
    Spawn (103, 1);
    Adopt (1000, 100, 101);
    Adopt (1001, 100, 102);
    Adopt (1002, 100, 103);
    Assoc (100, "A");
    Assoc (100, "B");
    Assoc (100, "C");
    Assoc (101, "A");
    Assoc (102, "B");
    Assoc (103, "C");
    Map (100, "A", 1000);
    Map (100, "B", 1001);
    Map (100, "C", 1002);
    Change_pol (100, SP, 3);
    (* A is highest priority, then B, then C *)
    Change_weight (100, 1000, 1.0);
    Change_weight (100, 1001, 2.0);
    Change_weight (100, 1002, 3.0);
  ]

let wfq_abc_expected : program =
  [
    Spawn (100, 0);
    Spawn (101, 1);
    Spawn (102, 1);
    Spawn (103, 1);
    Adopt (1000, 100, 101);
    Adopt (1001, 100, 102);
    Adopt (1002, 100, 103);
    Assoc (100, "A");
    Assoc (100, "B");
    Assoc (100, "C");
    Assoc (101, "A");
    Assoc (102, "B");
    Assoc (103, "C");
    Map (100, "A", 1000);
    Map (100, "B", 1001);
    Map (100, "C", 1002);
    Change_pol (100, WFQ, 3);
    Change_weight (100, 1000, 2.0);
    Change_weight (100, 1001, 1.0);
    Change_weight (100, 1002, 3.0);
  ]

let fifo_camel_vars_expected : program =
  [
    Spawn (100, 0);
    Spawn (101, 1);
    Spawn (102, 2);
    Spawn (103, 2);
    Spawn (104, 1);
    Spawn (105, 2);
    Spawn (106, 2);
    Adopt (1004, 100, 101);
    Adopt (1005, 100, 104);
    Adopt (1000, 101, 102);
    Adopt (1001, 101, 103);
    Adopt (1002, 104, 105);
    Adopt (1003, 104, 106);
    Assoc (100, "CLASS_A");
    Assoc (100, "CLASS_B");
    Assoc (100, "CLASS_C");
    Assoc (100, "CLASS_D");
    Assoc (101, "CLASS_A");
    Assoc (101, "CLASS_B");
    Assoc (102, "CLASS_A");
    Assoc (103, "CLASS_B");
    Assoc (104, "CLASS_C");
    Assoc (104, "CLASS_D");
    Assoc (105, "CLASS_C");
    Assoc (106, "CLASS_D");
    Map (100, "CLASS_A", 1004);
    Map (100, "CLASS_B", 1004);
    Map (100, "CLASS_C", 1005);
    Map (100, "CLASS_D", 1005);
    Map (101, "CLASS_A", 1000);
    Map (101, "CLASS_B", 1001);
    Map (104, "CLASS_C", 1002);
    Map (104, "CLASS_D", 1003);
    Change_pol (100, RR, 2);
    Change_pol (101, UNION, 2);
    Change_pol (104, UNION, 2);
  ]

let compile_tests =
  [
    make_test "fifo[A]" "work_conserving/fifo_A.sched" fifo_a_expected;
    make_test "drop unused class" "work_conserving/drop_class.sched"
      fifo_a_expected;
    make_test "union[G, H]" "work_conserving/union_GH.sched" union_gh_expected;
    make_test "rr[A, B, C]" "work_conserving/rr_ABC.sched" rr_abc_expected;
    make_test "strict[A, B, C]" "work_conserving/strict_ABC.sched"
      strict_abc_expected;
    make_test "wfq[(A,1),(B,2),(C,3)]" "work_conserving/wfq_ABC.sched"
      wfq_abc_expected;
    make_test "rr over two unions" "work_conserving/fifo_camel_vars.sched"
      fifo_camel_vars_expected;
  ]

(* The remaining nested programs (complex_tree.sched, rr_strict_hier.sched)
   now compile too — adding goldens for them is a follow-up. *)
let suite = "compile tests" >::: compile_tests
let () = run_test_tt_main suite

open Frontend
open OUnit2
open Ir

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

(* [.sched] file -> [Ir.program]. Stops short of pretty-printing — that's
   [test_pretty]'s job. *)
let compile filename =
  prog_dir ^ filename |> Parser.parse_file |> Policy.of_program |> Ir.of_policy

let make_test name filename (expected : program) =
  name >:: fun _ ->
  assert_equal ~printer:Ir.string_of_program expected (compile filename)

(* -------- Expected [Ir.program]s for each supported shape. --------

   Numbering reminder:
   - PEs are depth-based, starting at 0 (root).
   - vPIFO IDs come from a counter starting at 100.
   - Step IDs come from a counter starting at 1000.
   - vPIFOs are allocated post-order: children get IDs first, then parent.
   - Step IDs are allocated at adopt-time (one per child of each parent),
     so inner-tree adopts get smaller step IDs than outer-tree adopts. *)

(* Single FIFO leaf [A] living on PE0 as v100. *)
let fifo_a_expected : program = [ Spawn (100, 0); Assoc (100, "A") ]

(* union[A, B, C]: a UNION root with three FIFO leaves.
   Children are compiled first, so the leaves get v100/v101/v102; the root
   gets v103 last. The root sits on PE0; the leaves all share PE1. *)
let union_abc_expected : program =
  [
    (* Spawns: root first, then the three leaves. *)
    Spawn (103, 0);
    Spawn (100, 1);
    Spawn (101, 1);
    Spawn (102, 1);
    (* Root v103 adopts each leaf in source order; each adopt mints a fresh
       step ID (1000, 1001, 1002). *)
    Adopt (1000, 103, 100);
    Adopt (1001, 103, 101);
    Adopt (1002, 103, 102);
    (* Root accepts every class its subtree handles; each leaf accepts only
       its own. *)
    Assoc (103, "A");
    Assoc (103, "B");
    Assoc (103, "C");
    Assoc (100, "A");
    Assoc (101, "B");
    Assoc (102, "C");
    (* Routing: at the root, class X goes to the step that adopted X's leaf. *)
    Map (103, "A", 1000);
    Map (103, "B", 1001);
    Map (103, "C", 1002);
    (* Set the root's policy. *)
    Change_pol (103, UNION, 3);
  ]

(* rr[A, B, C]: identical shape to [union_abc] except the root runs RR. *)
let rr_abc_expected : program =
  [
    Spawn (103, 0);
    Spawn (100, 1);
    Spawn (101, 1);
    Spawn (102, 1);
    Adopt (1000, 103, 100);
    Adopt (1001, 103, 101);
    Adopt (1002, 103, 102);
    Assoc (103, "A");
    Assoc (103, "B");
    Assoc (103, "C");
    Assoc (100, "A");
    Assoc (101, "B");
    Assoc (102, "C");
    Map (103, "A", 1000);
    Map (103, "B", 1001);
    Map (103, "C", 1002);
    Change_pol (103, RR, 3);
  ]

(* strict[C, B, A]: strict priority where the first child (C) is highest
   priority. Same shape as the flat goldens above; the only differences are
   the class order and the [Change_weight]s that encode the priorities
   (1.0 = highest). *)
let strict_cba_expected : program =
  [
    Spawn (103, 0);
    Spawn (100, 1);
    Spawn (101, 1);
    Spawn (102, 1);
    Adopt (1000, 103, 100);
    Adopt (1001, 103, 101);
    Adopt (1002, 103, 102);
    Assoc (103, "C");
    Assoc (103, "B");
    Assoc (103, "A");
    Assoc (100, "C");
    Assoc (101, "B");
    Assoc (102, "A");
    Map (103, "C", 1000);
    Map (103, "B", 1001);
    Map (103, "A", 1002);
    Change_pol (103, SP, 3);
    (* Priorities: C is 1.0 (highest), then B, then A. *)
    Change_weight (103, 1000, 1.0);
    Change_weight (103, 1001, 2.0);
    Change_weight (103, 1002, 3.0);
  ]

(* wfq[(A,1), (B,2), (C,3)]: same shape as [union_abc], with WFQ as the root
   policy and per-arm weights. *)
let wfq_abc_expected : program =
  [
    Spawn (103, 0);
    Spawn (100, 1);
    Spawn (101, 1);
    Spawn (102, 1);
    Adopt (1000, 103, 100);
    Adopt (1001, 103, 101);
    Adopt (1002, 103, 102);
    Assoc (103, "A");
    Assoc (103, "B");
    Assoc (103, "C");
    Assoc (100, "A");
    Assoc (101, "B");
    Assoc (102, "C");
    Map (103, "A", 1000);
    Map (103, "B", 1001);
    Map (103, "C", 1002);
    Change_pol (103, WFQ, 3);
    (* Per-arm WFQ weights, in source order. *)
    Change_weight (103, 1000, 1.0);
    Change_weight (103, 1001, 2.0);
    Change_weight (103, 1002, 3.0);
  ]

(* rr[union[CLASS_A, CLASS_B], union[CLASS_C, CLASS_D]]
       v106 = rr root (PE0)
       v102 = first union (PE1); v100/v101 = its leaves (PE2)
       v105 = second union (PE1); v103/v104 = its leaves (PE2)
   Step IDs are allocated at adopt-time, post-order, so the inner unions take
   1000..1003 and the outer rr's adopts take 1004 and 1005. *)
let fifo_camel_vars_expected : program =
  [
    (* Spawns, in compile order: outer rr root, then the first union and
       its leaves, then the second union and its leaves. *)
    Spawn (106, 0);
    Spawn (102, 1);
    Spawn (100, 2);
    Spawn (101, 2);
    Spawn (105, 1);
    Spawn (103, 2);
    Spawn (104, 2);
    (* Adopts: the outer rr's two adopts come first in the emitted list,
       even though their step IDs (1004/1005) were allocated last. *)
    Adopt (1004, 106, 102);
    Adopt (1005, 106, 105);
    (* Then the first union adopts its leaves (steps 1000/1001)... *)
    Adopt (1000, 102, 100);
    Adopt (1001, 102, 101);
    (* ...and the second union adopts its leaves (steps 1002/1003). *)
    Adopt (1002, 105, 103);
    Adopt (1003, 105, 104);
    (* Root accepts every class in its subtree. *)
    Assoc (106, "CLASS_A");
    Assoc (106, "CLASS_B");
    Assoc (106, "CLASS_C");
    Assoc (106, "CLASS_D");
    (* First union accepts its two classes; its leaves accept their own. *)
    Assoc (102, "CLASS_A");
    Assoc (102, "CLASS_B");
    Assoc (100, "CLASS_A");
    Assoc (101, "CLASS_B");
    (* Second union accepts its two classes; its leaves accept their own. *)
    Assoc (105, "CLASS_C");
    Assoc (105, "CLASS_D");
    Assoc (103, "CLASS_C");
    Assoc (104, "CLASS_D");
    (* At the root: A/B both route via step 1004 (toward the first union),
       and C/D both route via step 1005 (toward the second union). *)
    Map (106, "CLASS_A", 1004);
    Map (106, "CLASS_B", 1004);
    Map (106, "CLASS_C", 1005);
    Map (106, "CLASS_D", 1005);
    (* At the inner unions: route to the appropriate leaf. *)
    Map (102, "CLASS_A", 1000);
    Map (102, "CLASS_B", 1001);
    Map (105, "CLASS_C", 1002);
    Map (105, "CLASS_D", 1003);
    (* Set policies on the three internal nodes. *)
    Change_pol (106, RR, 2);
    Change_pol (102, UNION, 2);
    Change_pol (105, UNION, 2);
  ]

(* rr[union[BX, BY], rr[RP, RT]] — same shape as above, second inner is RR. *)
let rr_union_hier_expected : program =
  [
    (* Spawns: outer rr, first union and its leaves, then the inner rr
       and its leaves. *)
    Spawn (106, 0);
    Spawn (102, 1);
    Spawn (100, 2);
    Spawn (101, 2);
    Spawn (105, 1);
    Spawn (103, 2);
    Spawn (104, 2);
    (* Outer rr adopts the two inner subtrees. *)
    Adopt (1004, 106, 102);
    Adopt (1005, 106, 105);
    (* Inner union and inner rr each adopt their two leaves. *)
    Adopt (1000, 102, 100);
    Adopt (1001, 102, 101);
    Adopt (1002, 105, 103);
    Adopt (1003, 105, 104);
    (* Root accepts all four classes. *)
    Assoc (106, "BX");
    Assoc (106, "BY");
    Assoc (106, "RP");
    Assoc (106, "RT");
    (* Inner union and its leaves. *)
    Assoc (102, "BX");
    Assoc (102, "BY");
    Assoc (100, "BX");
    Assoc (101, "BY");
    (* Inner rr and its leaves. *)
    Assoc (105, "RP");
    Assoc (105, "RT");
    Assoc (103, "RP");
    Assoc (104, "RT");
    (* Root routing: BX/BY share step 1004; RP/RT share step 1005. *)
    Map (106, "BX", 1004);
    Map (106, "BY", 1004);
    Map (106, "RP", 1005);
    Map (106, "RT", 1005);
    (* Inner routing. *)
    Map (102, "BX", 1000);
    Map (102, "BY", 1001);
    Map (105, "RP", 1002);
    Map (105, "RT", 1003);
    (* Policies. *)
    Change_pol (106, RR, 2);
    Change_pol (102, UNION, 2);
    Change_pol (105, RR, 2);
  ]

let compile_tests =
  [
    make_test "fifo[A]" "work_conserving/fifo_A.sched" fifo_a_expected;
    make_test "drop unused class" "work_conserving/drop_class.sched"
      fifo_a_expected;
    make_test "union[A, B, C]" "work_conserving/union_ABC.sched"
      union_abc_expected;
    make_test "rr[A, B, C]" "work_conserving/rr_ABC.sched" rr_abc_expected;
    make_test "strict[C, B, A]" "work_conserving/strict_CBA.sched"
      strict_cba_expected;
    make_test "wfq[(A,1),(B,2),(C,3)]" "work_conserving/wfq_ABC.sched"
      wfq_abc_expected;
    make_test "rr over two unions" "work_conserving/fifo_camel_vars.sched"
      fifo_camel_vars_expected;
    make_test "rr[union[BX,BY], rr[RP,RT]]"
      "work_conserving/rr_union_hier.sched" rr_union_hier_expected;
  ]

(* The remaining nested programs (complex_tree.sched, rr_strict_hier.sched)
   now compile too — adding goldens for them is a follow-up. *)
let suite = "compile tests" >::: compile_tests
let () = run_test_tt_main suite

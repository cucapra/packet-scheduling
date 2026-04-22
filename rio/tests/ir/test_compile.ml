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

(* Asserts that compilation raises [Ir.UnsupportedPolicy]. The carried
   message is not checked. *)
let make_unsupported_test name filename =
  name >:: fun _ ->
  try
    let _ = compile filename in
    assert_failure
      (Printf.sprintf "%s: expected UnsupportedPolicy, got a result" name)
  with Ir.UnsupportedPolicy _ -> ()

(* -------- Expected [Ir.program]s for each supported shape. -------- *)

let fifo_a_expected : program = [ Spawn (0, 1); Assoc (0, "A") ]

let union_abc_expected : program =
  [
    Spawn (0, 1);
    Spawn (1, 2);
    Spawn (2, 2);
    Spawn (3, 2);
    Adopt (0, 0, 1);
    Adopt (1, 0, 2);
    Adopt (2, 0, 3);
    Assoc (0, "A");
    Assoc (0, "B");
    Assoc (0, "C");
    Assoc (1, "A");
    Assoc (2, "B");
    Assoc (3, "C");
    Map (0, "A", 0);
    Map (0, "B", 1);
    Map (0, "C", 2);
    Change_pol (0, UNION, 3);
  ]

let rr_abc_expected : program =
  [
    Spawn (0, 1);
    Spawn (1, 2);
    Spawn (2, 2);
    Spawn (3, 2);
    Adopt (0, 0, 1);
    Adopt (1, 0, 2);
    Adopt (2, 0, 3);
    Assoc (0, "A");
    Assoc (0, "B");
    Assoc (0, "C");
    Assoc (1, "A");
    Assoc (2, "B");
    Assoc (3, "C");
    Map (0, "A", 0);
    Map (0, "B", 1);
    Map (0, "C", 2);
    Change_pol (0, RR, 3);
  ]

let strict_cba_expected : program =
  [
    Spawn (0, 1);
    Spawn (1, 2);
    Spawn (2, 2);
    Spawn (3, 2);
    Adopt (0, 0, 1);
    Adopt (1, 0, 2);
    Adopt (2, 0, 3);
    Assoc (0, "C");
    Assoc (0, "B");
    Assoc (0, "A");
    Assoc (1, "C");
    Assoc (2, "B");
    Assoc (3, "A");
    Map (0, "C", 0);
    Map (0, "B", 1);
    Map (0, "A", 2);
    Change_pol (0, SP, 3);
    Change_weight (0, 0, 1);
    Change_weight (0, 1, 2);
    Change_weight (0, 2, 3);
  ]

let wfq_abc_expected : program =
  [
    Spawn (0, 1);
    Spawn (1, 2);
    Spawn (2, 2);
    Spawn (3, 2);
    Adopt (0, 0, 1);
    Adopt (1, 0, 2);
    Adopt (2, 0, 3);
    Assoc (0, "A");
    Assoc (0, "B");
    Assoc (0, "C");
    Assoc (1, "A");
    Assoc (2, "B");
    Assoc (3, "C");
    Map (0, "A", 0);
    Map (0, "B", 1);
    Map (0, "C", 2);
    Change_pol (0, WFQ, 3);
    Change_weight (0, 0, 1);
    Change_weight (0, 1, 2);
    Change_weight (0, 2, 3);
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
  ]

let error_tests =
  [
    make_unsupported_test "nested: strict[A, B, rr[...]]"
      "work_conserving/rr_strict_hier.sched";
    make_unsupported_test "nested: wfq over strict/rr/union children"
      "work_conserving/complex_tree.sched";
    make_unsupported_test "nested: rr with an rr child"
      "work_conserving/rr_union_hier.sched";
    (* rr[union[...], union[...]] — children must now be FIFOs. *)
    make_unsupported_test "nested: rr over two unions"
      "work_conserving/fifo_camel_vars.sched";
  ]

let suite = "compile tests" >::: compile_tests @ error_tests
let () = run_test_tt_main suite

open Frontend
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

let compile filename =
  prog_dir ^ filename
  |> Parser.parse_file
  |> Policy.of_program
  |> Ir.of_policy
  |> Ir.string_of_program

let make_test name filename expected =
  name >:: fun _ -> assert_equal ~printer:Fun.id expected (compile filename)

(* Asserts that compilation raises [Ir.UnsupportedPolicy]. The carried
   message is not checked. *)
let make_unsupported_test name filename =
  name >:: fun _ ->
  try
    let _ = compile filename in
    assert_failure
      (Printf.sprintf "%s: expected UnsupportedPolicy, got a result" name)
  with Ir.UnsupportedPolicy _ -> ()

(* -------- Golden strings for each supported shape. -------- *)

let fifo_a_expected = String.concat "\n" [ "v0 = spawn(pe1)"; "assoc(v0, A)" ]

let union_abc_expected =
  String.concat "\n"
    [
      "v0 = spawn(pe1)";
      "assoc(v0, A)";
      "assoc(v0, B)";
      "assoc(v0, C)";
    ]

let rr_abc_expected =
  String.concat "\n"
    [
      "v0 = spawn(pe1)";
      "v1 = spawn(pe2)";
      "v2 = spawn(pe2)";
      "v3 = spawn(pe2)";
      "step_0 = adopt(v0, v1)";
      "step_1 = adopt(v0, v2)";
      "step_2 = adopt(v0, v3)";
      "assoc(v0, A)";
      "assoc(v0, B)";
      "assoc(v0, C)";
      "assoc(v1, A)";
      "assoc(v2, B)";
      "assoc(v3, C)";
      "map(v0, A, step_0)";
      "map(v0, B, step_1)";
      "map(v0, C, step_2)";
      "change_pol(v0, RR, 3)";
    ]

let strict_cba_expected =
  String.concat "\n"
    [
      "v0 = spawn(pe1)";
      "v1 = spawn(pe2)";
      "v2 = spawn(pe2)";
      "v3 = spawn(pe2)";
      "step_0 = adopt(v0, v1)";
      "step_1 = adopt(v0, v2)";
      "step_2 = adopt(v0, v3)";
      "assoc(v0, C)";
      "assoc(v0, B)";
      "assoc(v0, A)";
      "assoc(v1, C)";
      "assoc(v2, B)";
      "assoc(v3, A)";
      "map(v0, C, step_0)";
      "map(v0, B, step_1)";
      "map(v0, A, step_2)";
      "change_pol(v0, SP, 3)";
      "change_weight(v0, step_0, 1)";
      "change_weight(v0, step_1, 2)";
      "change_weight(v0, step_2, 3)";
    ]

let wfq_abc_expected =
  String.concat "\n"
    [
      "v0 = spawn(pe1)";
      "v1 = spawn(pe2)";
      "v2 = spawn(pe2)";
      "v3 = spawn(pe2)";
      "step_0 = adopt(v0, v1)";
      "step_1 = adopt(v0, v2)";
      "step_2 = adopt(v0, v3)";
      "assoc(v0, A)";
      "assoc(v0, B)";
      "assoc(v0, C)";
      "assoc(v1, A)";
      "assoc(v2, B)";
      "assoc(v3, C)";
      "map(v0, A, step_0)";
      "map(v0, B, step_1)";
      "map(v0, C, step_2)";
      "change_pol(v0, WFQ, 3)";
      "change_weight(v0, step_0, 1)";
      "change_weight(v0, step_1, 2)";
      "change_weight(v0, step_2, 3)";
    ]

let fifo_camel_vars_expected =
  (* rr[union[CLASS_A, CLASS_B], union[CLASS_C, CLASS_D]] *)
  String.concat "\n"
    [
      "v0 = spawn(pe1)";
      "v1 = spawn(pe2)";
      "v2 = spawn(pe2)";
      "step_0 = adopt(v0, v1)";
      "step_1 = adopt(v0, v2)";
      "assoc(v0, CLASS_A)";
      "assoc(v0, CLASS_B)";
      "assoc(v0, CLASS_C)";
      "assoc(v0, CLASS_D)";
      "assoc(v1, CLASS_A)";
      "assoc(v1, CLASS_B)";
      "assoc(v2, CLASS_C)";
      "assoc(v2, CLASS_D)";
      "map(v0, CLASS_A, step_0)";
      "map(v0, CLASS_B, step_0)";
      "map(v0, CLASS_C, step_1)";
      "map(v0, CLASS_D, step_1)";
      "change_pol(v0, RR, 2)";
    ]

let compile_tests =
  [
    make_test "fifo[A]" "work_conserving/fifo_A.sched" fifo_a_expected;
    (* classes A, B; policy = A — B is declared but unused, dropped. *)
    make_test "drop unused class" "work_conserving/drop_class.sched"
      fifo_a_expected;
    make_test "union[A, B, C]" "work_conserving/union_ABC.sched"
      union_abc_expected;
    make_test "rr[A, B, C]" "work_conserving/rr_ABC.sched" rr_abc_expected;
    make_test "strict[C, B, A]" "work_conserving/strict_CBA.sched"
      strict_cba_expected;
    make_test "wfq[(A,1),(B,2),(C,3)]" "work_conserving/wfq_ABC.sched"
      wfq_abc_expected;
    (* Top-level rr with union-shaped leaves (the leaf vPIFOs take multiple
       [Assoc]s). *)
    make_test "rr over two unions" "work_conserving/fifo_camel_vars.sched"
      fifo_camel_vars_expected;
  ]

let error_tests =
  [
    make_unsupported_test "nested: strict[A, B, rr[...]]"
      "work_conserving/rr_strict_hier.sched";
    make_unsupported_test "nested: wfq over strict/rr/union children"
      "work_conserving/complex_tree.sched";
    make_unsupported_test "nested: rr with an rr child"
      "work_conserving/rr_union_hier.sched";
  ]

let suite = "compile tests" >::: compile_tests @ error_tests
let () = run_test_tt_main suite

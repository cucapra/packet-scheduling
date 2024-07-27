open Dsl_core
open OUnit2

(* The test suite for running our interpreter. *)

let prefix = "../../../../dsl/progs/"

let eval_prog (filename : string) =
  let res = Parse.parse_file (prefix ^ filename) in
  Eval.eval res

let make_test (name : string) (filename : string) (val_str : string) =
  name >:: fun _ ->
  assert_equal val_str
    (Util.string_of_policy (eval_prog filename))
    ~printer:(fun x -> x)

let make_error_test (name : string) (filename : string) (exn : exn) =
  name >:: fun _ ->
  assert_raises exn (fun () ->
      try Util.string_of_policy (eval_prog filename)
      with Eval.UnboundVariable _ -> raise exn)

let tests =
  [
    make_test "single class policy" "now/drop_a_class.sched" "A";
    make_test "fifo sugar 1 class" "now/fifo_1_class_sugar.sched" "A";
    make_test "fifo 1 class" "now/fifo_1_class.sched" "A";
    make_test "fifo of 3" "now/fifo_n_classes.sched" "fifo[A, B, C]";
    make_test "rr of 1" "now/rr_1_class.sched" "rr[A]";
    make_test "rr of 2" "now/rr_2_classes.sched" "rr[A, B]";
    make_test "multiple assignments" "now/rr_hier_merge_sugar.sched"
      "rr[fifo[BX, BY], rr[RP, RT]]";
    make_test "2 assignments w/ substitutions" "now/rr_hier.sched"
      "rr[B, rr[RP, RT]]";
    make_test "3 classes with substitutions" "soon/rr_n_class_hier.sched"
      "rr[A, B, rr[rr[CU, CV], rr[CW, CX]]]";
    make_test "rr of 3" "soon/rr_n_classes.sched" "rr[A, B, C]";
    make_test "rr and strict substitutions"
      "soon/rr_strict_n_classes_hier.sched"
      "strict[A, B, rr[rr[CU, CV], strict[CW, CX]]]";
    make_test "strict of 3" "soon/strict_n_classes.sched" "strict[A, B, C]";
  ]

let error_tests =
  [
    make_error_test "undeclared class" "incorrect/undeclared_classes.sched"
      (Eval.UnboundVariable "...");
    make_error_test "unbound variable" "incorrect/unbound_var.sched"
      (Eval.UnboundVariable "...");
    make_error_test "unbound var in middle of list of assignments"
      "incorrect/unbound_var_hier.sched" (Eval.UnboundVariable "...");
  ]

let suite = "suite" >::: tests @ error_tests
let () = run_test_tt_main suite

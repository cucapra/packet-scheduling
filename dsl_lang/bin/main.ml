open Dsl_core
open OUnit2

(* The test suite for our interpreter. *)

let eval_prog (filename : string) =
  let res = Parse.parse_file filename in
  Eval.eval res

let make_test (name : string) (filename : string) (val_str : string) =
  name >:: fun _ ->
  assert_equal val_str
    (Util.string_of_policy (eval_prog filename))
    ~printer:(fun x -> x)

let make_error_test (name : string) (filename : string) (exn : exn) =
  name >:: fun _ ->
  assert_raises exn (fun () -> Util.string_of_policy (eval_prog filename))

let tests =
  [
    make_test "single class policy" "../../dsl/progs/now/drop_a_class.sched" "A";
    make_test "fifo sugar 1 class"
      "../../dsl/progs/now/fifo_1_class_sugar.sched" "A";
    make_test "fifo 1 class" "../../dsl/progs/now/fifo_1_class.sched" "A";
    make_test "fifo of 3" "../../dsl/progs/now/fifo_n_classes.sched"
      "fifo[A, B, C]";
    make_test "rr of 1" "../../dsl/progs/now/rr_1_class.sched" "rr[A]";
    make_test "rr of 2" "../../dsl/progs/now/rr_2_classes.sched" "rr[A, B]";
    make_test "multiple assignments"
      "../../dsl/progs/now/rr_hier_merge_sugar.sched"
      "rr[fifo[BX, BY], rr[RP, RT]]";
    make_test "2 assignments w/ substitutions"
      "../../dsl/progs/now/rr_hier.sched" "rr[B, rr[RP, RT]]";
    make_test "3 classes with substitutions"
      "../../dsl/progs/soon/rr_n_class_hier.sched"
      "rr[A, B, rr[rr[CU, CV], rr[CW, CX]]]";
    make_test "rr of 3" "../../dsl/progs/soon/rr_n_classes.sched" "rr[A, B, C]";
    make_test "rr and strict substitutions"
      "../../dsl/progs/soon/rr_strict_n_classes_hier.sched"
      "strict[A, B, rr[rr[CU, CV], strict[CW, CX]]]";
    make_test "strict of 3" "../../dsl/progs/soon/strict_n_classes.sched"
      "strict[A, B, C]";
    make_test "leaky bucket of 2" "../../dsl/progs/nwc/leaky_2_classes.sched"
      "leaky[[A, B], width = 5, buffer = 10]";
    make_test "token bucket of 2 round robins"
      "../../dsl/progs/nwc/token_2_rr_children.sched"
      "token[[rr[A, B], rr[C, D]], width = 20, time = 50]";
    make_test "stop and go with 3 classes"
      "../../dsl/progs/nwc/sg_3_classes.sched"
      "stopandgo[[stopandgo[[A, B], width = 10], stopandgo[[C], width = 10]], \
       width = 5]";
    make_test "rcsp for 4 classes" "../../dsl/progs/nwc/rcsp_4_classes.sched"
      "rcsp[A, B, C, D]";
  ]

let error_tests =
  [
    make_error_test "undeclared class"
      "../../dsl/progs/incorrect/undeclared_classes.sched"
      (Eval.UndeclaredClass "Z");
    make_error_test "unbound variable"
      "../../dsl/progs/incorrect/unbound_var.sched"
      (Eval.UnboundVariable "policy");
    make_error_test "unbound var in middle of list of assignments"
      "../../dsl/progs/incorrect/unbound_var_hier.sched"
      (Eval.UnboundVariable "r_polic");
  ]

let suite = "suite" >::: tests @ error_tests
let () = run_test_tt_main suite

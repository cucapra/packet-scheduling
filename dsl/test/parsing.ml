open Frontend
open OUnit2

let prefix = "../../../../progs/"

let parse filename =
  prefix ^ filename |> Parser.parse_file |> Policy.of_program
  |> Policy.to_string

let make_test name filename val_str =
  name >:: fun _ -> assert_equal val_str (parse filename) ~printer:Fun.id

let make_error_test name filename exn =
  name >:: fun _ -> assert_raises exn (fun () -> parse filename)

let wc_tests =
  [
    make_test "single class policy" "work_conserving/drop_a_class.sched" "A";
    make_test "fifo 1 class" "work_conserving/fifo_1_class_sugar.sched" "A";
    make_test "fifo 1 class" "work_conserving/fifo_1_class.sched" "A";
    make_test "fifo of 3" "work_conserving/fifo_n_classes.sched" "fifo[A, B, C]";
    make_test "rr of 1" "work_conserving/rr_1_class.sched" "rr[A]";
    make_test "rr of 2" "work_conserving/rr_2_classes.sched" "rr[A, B]";
    make_test "multiple assignments" "work_conserving/rr_hier_merge_sugar.sched"
      "rr[fifo[BX, BY], rr[RP, RT]]";
    make_test "2 assignments w/ substitutions" "work_conserving/rr_hier.sched"
      "rr[B, rr[RP, RT]]";
    make_test "3 classes with substitutions"
      "work_conserving/rr_n_class_hier.sched"
      "rr[A, B, rr[rr[CU, CV], rr[CW, CX]]]";
    make_test "rr of 3" "work_conserving/rr_n_classes.sched" "rr[A, B, C]";
    make_test "rr and strict substitutions"
      "work_conserving/rr_strict_n_classes_hier.sched"
      "strict[A, B, rr[rr[CU, CV], strict[CW, CX]]]";
    make_test "strict of 3" "work_conserving/strict_n_classes.sched"
      "strict[A, B, C]";
  ]

let _nwc_tests =
  [
    make_test "leaky bucket of 2" "non_work_conserving/leaky_2_classes.sched"
      "leaky[[A, B], width = 5, buffer = 10]";
    make_test "token bucket of 2 round robins"
      "non_work_conserving/token_2_rr_children.sched"
      "token[[rr[A, B], rr[C, D]], width = 20, time = 50]";
    make_test "stop and go with 3 classes"
      "non_work_conserving/sg_3_classes.sched"
      "stopandgo[[stopandgo[[A, B], width = 10], stopandgo[[C], width = 10]], \
       width = 5]";
    make_test "rcsp for 4 classes" "non_work_conserving/rcsp_4_classes.sched"
      "rcsp[A, B, C, D]";
  ]

let error_tests =
  [
    make_error_test "undeclared class" "incorrect/undeclared_classes.sched"
      (Policy.UndeclaredClass "Z");
    make_error_test "unbound variable" "incorrect/unbound_var.sched"
      (Policy.UnboundVariable "policy");
    make_error_test "unbound var in middle of list of assignments"
      "incorrect/unbound_var_hier.sched" (Policy.UnboundVariable "r_polic");
  ]

let suite = "suite" >::: wc_tests @ error_tests (* @ nwc_tests *)
let () = run_test_tt_main suite

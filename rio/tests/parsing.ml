open Frontend
open OUnit2

let make_test name filename val_str =
  name >:: fun _ ->
  assert_equal val_str (Util.parse filename |> Policy.to_string) ~printer:Fun.id

let make_error_test name filename exn =
  name >:: fun _ -> assert_raises exn (fun () -> Util.parse filename)

let wc_tests =
  [
    make_test "single class policy" "progs/work_conserving/drop_a_class.sched"
      "fifo[A]";
    make_test "fifo 1 class" "progs/work_conserving/fifo_1_class_sugar.sched"
      "fifo[A]";
    make_test "fifo 1 class" "progs/work_conserving/fifo_1_class.sched"
      "fifo[A]";
    make_test "fifo of 3" "progs/work_conserving/fifo_n_classes.sched"
      "fifo[A, B, C]";
    make_test "rr of 1" "progs/work_conserving/rr_1_class.sched" "rr[A]";
    make_test "rr of 2" "progs/work_conserving/rr_2_classes.sched"
      "rr[fifo[A, B]]";
    make_test "multiple assignments"
      "progs/work_conserving/rr_hier_merge_sugar.sched"
      "rr[fifo[BX, BY], rr[RP, RT]]";
    make_test "2 assignments w/ substitutions"
      "progs/work_conserving/rr_hier.sched" "rr[B, rr[RP, RT]]";
    make_test "3 classes with substitutions"
      "progs/work_conserving/rr_n_class_hier.sched"
      "rr[A, B, rr[rr[CU, CV], rr[CW, CX]]]";
    make_test "rr of 3" "progs/work_conserving/rr_n_classes.sched" "rr[A, B, C]";
    make_test "rr and strict substitutions"
      "progs/work_conserving/rr_strict_n_classes_hier.sched"
      "strict[A, B, rr[rr[CU, CV], strict[CW, CX]]]";
    make_test "strict of 3" "progs/work_conserving/strict_n_classes.sched"
      "strict[C, B, A]";
    make_test "wfq of 3" "progs/work_conserving/wfq_n_classes.sched"
      "wfq[(A, 1.00), (B, 2.00), (C, 3.00)]";
    make_test "fifo with non-capitalized classes"
      "progs/work_conserving/fifo_noncap_classes.sched"
      "fifo[CLASS_A, CLASS_B, CLASS_C]";
  ]

let _nwc_tests =
  [
    make_test "leaky bucket of 2"
      "progs/non_work_conserving/leaky_2_classes.sched"
      "leaky[[A, B], width = 5, buffer = 10]";
    make_test "token bucket of 2 round robins"
      "progs/non_work_conserving/token_2_rr_children.sched"
      "token[[rr[A, B], rr[C, D]], width = 20, time = 50]";
    make_test "stop and go with 3 classes"
      "progs/non_work_conserving/sg_3_classes.sched"
      "stopandgo[[stopandgo[[A, B], width = 10], stopandgo[[C], width = 10]], \
       width = 5]";
    make_test "rcsp for 4 classes"
      "progs/non_work_conserving/rcsp_4_classes.sched" "rcsp[A, B, C, D]";
  ]

let error_tests =
  [
    make_error_test "undeclared class"
      "progs/incorrect/undeclared_classes.sched" (Policy.UndeclaredClass "Z");
    make_error_test "unbound variable" "progs/incorrect/unbound_var.sched"
      (Policy.UnboundVariable "policy");
    make_error_test "unbound var in middle of list of assignments"
      "progs/incorrect/unbound_var_hier.sched"
      (Policy.UnboundVariable "r_polic");
    make_error_test "class used twice in policy"
      "progs/incorrect/duplicate_classes.sched" (Policy.DuplicateClass "B");
    make_error_test "class used twice in one fifo"
      "progs/incorrect/duplicate_samepol.sched" (Policy.DuplicateClass "A");
    make_error_test "fifo for multiple classes without union"
      "progs/incorrect/set_multiple.sched"
      (Parser.ParserError "Syntax error at line 4, character 17");
    make_error_test "rr for classes without fifo'ing first"
      "progs/incorrect/set_hierarchical.sched"
      (Parser.ParserError "Syntax error at line 3, character 14");
  ]

let suite = "parsing tests" >::: wc_tests @ error_tests (* @ nwc_tests *)
let () = run_test_tt_main suite

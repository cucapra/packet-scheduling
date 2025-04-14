open Frontend
open OUnit2

let make_test name filename val_str =
  let f_name = "../../../../progs/work_conserving/" ^ filename ^ ".sched" in
  name >:: fun _ -> assert_equal val_str (Json.main f_name) ~printer:Fun.id

let make_error_test name filename exn =
  let f_name = "../../../../progs/incorrect/" ^ filename ^ ".sched" in
  name >:: fun _ -> assert_raises exn (fun () -> Json.main f_name)

let tests =
  [
    make_test "single class policy" "drop_a_class"
      "{\"fifo\":[{\"class\":\"A\"}]}";
    make_test "fifo 1 class" "fifo_1_class_sugar"
      "{\"fifo\":[{\"class\":\"A\"}]}";
    make_test "fifo 2 class union" "fifo_2_class_union"
      "{\"fifo\":[{\"class\":\"A\"},{\"class\":\"B\"}]}";
    make_test "fifo n classes" "fifo_n_classes"
      "{\"fifo\":[{\"class\":\"A\"},{\"class\":\"B\"},{\"class\":\"C\"}]}";
    make_test "rr 1 class" "rr_1_class" "{\"rr\":[{\"class\":\"A\"}]}";
    make_test "rr 2 classes" "rr_2_classes"
      "{\"rr\":[{\"fifo\":[{\"class\":\"A\"},{\"class\":\"B\"}]}]}";
    make_test "rr hierarchy" "rr_hier_merge_sugar"
      "{\"rr\":[{\"fifo\":[{\"class\":\"BX\"},{\"class\":\"BY\"}]},{\"rr\":[{\"class\":\"RP\"},{\"class\":\"RT\"}]}]}";
    make_test "rr even hierarchy" "rr_hier"
      "{\"rr\":[{\"class\":\"B\"},{\"rr\":[{\"class\":\"RP\"},{\"class\":\"RT\"}]}]}";
    make_test "rr n classes hierarchy" "rr_n_class_hier"
      "{\"rr\":[{\"class\":\"A\"},{\"class\":\"B\"},{\"rr\":[{\"rr\":[{\"class\":\"CU\"},{\"class\":\"CV\"}]},{\"rr\":[{\"class\":\"CW\"},{\"class\":\"CX\"}]}]}]}";
    make_test "rr n classes" "rr_n_classes"
      "{\"rr\":[{\"class\":\"A\"},{\"class\":\"B\"},{\"class\":\"C\"}]}";
    make_test "rr and strict" "rr_strict_n_classes_hier"
      "{\"strict\":[{\"class\":\"A\"},{\"class\":\"B\"},{\"rr\":[{\"rr\":[{\"class\":\"CU\"},{\"class\":\"CV\"}]},{\"strict\":[{\"class\":\"CW\"},{\"class\":\"CX\"}]}]}]}";
    make_test "strict n classes" "strict_n_classes"
      "{\"strict\":[{\"class\":\"C\"},{\"class\":\"B\"},{\"class\":\"A\"}]}";
    make_test "complex tree" "complex_tree"
      "{\"rr\":[{\"strict\":[{\"class\":\"A\"},{\"class\":\"B\"},{\"class\":\"C\"}]},{\"rr\":[{\"class\":\"D\"},{\"class\":\"E\"},{\"class\":\"F\"}]},{\"rr\":[{\"class\":\"G\"},{\"class\":\"H\"}]}]}";
  ]

let error_tests =
  [
    make_error_test "undeclared class" "undeclared_classes"
      (Policy.UndeclaredClass "Z");
    make_error_test "unbound variable" "unbound_var"
      (Policy.UnboundVariable "policy");
    make_error_test "unbound var in middle of list of assignments"
      "unbound_var_hier" (Policy.UnboundVariable "r_polic");
    make_error_test "class used twice in policy" "duplicate_classes"
      (Policy.DuplicateClass "B");
    make_error_test "class used twice in one fifo" "duplicate_samepol"
      (Policy.DuplicateClass "A");
    make_error_test "fifo for multiple classes without union" "set_multiple"
      (Parser.ParserError "Syntax error at line 4, character 17");
    make_error_test "rr for classes without fifo'ing first" "set_hierarchical"
      (Parser.ParserError "Syntax error at line 3, character 14");
  ]

let suite = "serialization tests" >::: tests @ error_tests
let () = run_test_tt_main suite

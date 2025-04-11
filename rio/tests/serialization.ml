open Frontend
open OUnit2

let make_test name filename val_str =
  name >:: fun _ -> assert_equal val_str (Json.main filename) ~printer:Fun.id

let tests =
  [
    make_test "single class policy"
      "../../../../progs/work_conserving/drop_a_class.sched"
      "{\"fifo\":[{\"class\":\"A\"}]}";
    make_test "fifo 1 class"
      "../../../../progs/work_conserving/fifo_1_class_sugar.sched"
      "{\"fifo\":[{\"class\":\"A\"}]}";
    make_test "fifo 2 class union"
      "../../../../progs/work_conserving/fifo_2_class_union.sched"
      "{\"fifo\":[{\"class\":\"A\"},{\"class\":\"B\"}]}";
    make_test "fifo n classes"
      "../../../../progs/work_conserving/fifo_n_classes.sched"
      "{\"fifo\":[{\"class\":\"A\"},{\"class\":\"B\"},{\"class\":\"C\"}]}";
    make_test "rr 1 class" "../../../../progs/work_conserving/rr_1_class.sched"
      "{\"rr\":[{\"class\":\"A\"}]}";
    make_test "rr 2 classes"
      "../../../../progs/work_conserving/rr_2_classes.sched"
      "{\"rr\":[{\"fifo\":[{\"class\":\"A\"},{\"class\":\"B\"}]}]}";
    make_test "rr hierarchy"
      "../../../../progs/work_conserving/rr_hier_merge_sugar.sched"
      "{\"rr\":[{\"fifo\":[{\"class\":\"BX\"},{\"class\":\"BY\"}]},{\"rr\":[{\"class\":\"RP\"},{\"class\":\"RT\"}]}]}";
    make_test "rr even hierarchy"
      "../../../../progs/work_conserving/rr_hier.sched"
      "{\"rr\":[{\"class\":\"B\"},{\"rr\":[{\"class\":\"RP\"},{\"class\":\"RT\"}]}]}";
    make_test "rr n classes hierarchy"
      "../../../../progs/work_conserving/rr_n_class_hier.sched"
      "{\"rr\":[{\"class\":\"A\"},{\"class\":\"B\"},{\"rr\":[{\"rr\":[{\"class\":\"CU\"},{\"class\":\"CV\"}]},{\"rr\":[{\"class\":\"CW\"},{\"class\":\"CX\"}]}]}]}";
    make_test "rr n classes"
      "../../../../progs/work_conserving/rr_n_classes.sched"
      "{\"rr\":[{\"class\":\"A\"},{\"class\":\"B\"},{\"class\":\"C\"}]}";
    make_test "rr and strict"
      "../../../../progs/work_conserving/rr_strict_n_classes_hier.sched"
      "{\"strict\":[{\"class\":\"A\"},{\"class\":\"B\"},{\"rr\":[{\"rr\":[{\"class\":\"CU\"},{\"class\":\"CV\"}]},{\"strict\":[{\"class\":\"CW\"},{\"class\":\"CX\"}]}]}]}";
    make_test "strict n classes"
      "../../../../progs/work_conserving/strict_n_classes.sched"
      "{\"strict\":[{\"class\":\"C\"},{\"class\":\"B\"},{\"class\":\"A\"}]}";
  ]

let suite = "serialization tests" >::: tests
let () = run_test_tt_main suite

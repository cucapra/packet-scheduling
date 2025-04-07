open Frontend
open OUnit2

let make_test name filename val_str =
  name >:: fun _ ->
  assert_equal val_str (To_json.main filename) ~printer:Fun.id

(* let make_error_test name filename exn =
  name >:: fun _ -> assert_raises exn (fun () -> Util.parse filename) *)

let tests =
  [
    make_test "single class policy" "progs/work_conserving/drop_a_class.sched"
      "fifo[A]";
    make_test "fifo 1 class" "progs/work_conserving/fifo_1_class_sugar.sched"
      "fifo[A]";
  ]

let suite = "serialization tests" >::: tests  (* @ nwc_tests *)
let () = run_test_tt_main suite
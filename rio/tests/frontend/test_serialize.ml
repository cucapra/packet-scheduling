open Frontend
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"
let json_dir = "jsons/"

let prog_to_json file =
  prog_dir ^ file |> Parser.parse_file |> Policy.of_program |> Json.from_policy

let make_test name file =
  let base = file |> Filename.basename |> Filename.remove_extension in
  let json = json_dir ^ Printf.sprintf "%s.json" base in

  name >:: fun _ ->
  assert_equal
    (Yojson.Basic.from_file json)
    (prog_to_json file) ~printer:Yojson.Basic.pretty_to_string
    ~cmp:Yojson.Basic.equal

let tests =
  [
    make_test "single class policy" "work_conserving/drop_class.sched";
    make_test "fifo 1 class" "work_conserving/fifo_A.sched";
    make_test "union ABC" "work_conserving/union_ABC.sched";
    make_test "rr hierarchy" "work_conserving/rr_union_hier.sched";
    make_test "rr n classes" "work_conserving/rr_ABC.sched";
    make_test "rr and strict" "work_conserving/rr_strict_hier.sched";
    make_test "strict n classes" "work_conserving/strict_CBA.sched";
    make_test "complex tree" "work_conserving/complex_tree.sched";
  ]

let suite = "serialization tests" >::: tests
let () = run_test_tt_main suite

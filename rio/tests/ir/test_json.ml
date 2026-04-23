open Frontend
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"
let json_dir = "jsons/"

(* [.sched] file -> JSON value, going via the IR. *)
let compile_to_json filename =
  prog_dir ^ filename |> Parser.parse_file |> Policy.of_program |> Ir.of_policy
  |> Ir.Json.from_program

let make_golden_test name sched_file json_file =
  name >:: fun _ ->
  let expected = Yojson.Basic.from_file (json_dir ^ json_file) in
  let actual = compile_to_json sched_file in
  let printer = Yojson.Basic.pretty_to_string in
  (* Structural equality — whitespace and key order in the golden file don't
     matter, since [Yojson.Basic.equal] compares the parsed values. *)
  assert_equal ~printer ~cmp:Yojson.Basic.equal expected actual

let json_tests =
  [
    make_golden_test "rr[A, B, C]" "work_conserving/rr_ABC.sched"
      "rr_ABC.json";
    make_golden_test "complex_tree" "work_conserving/complex_tree.sched"
      "complex_tree.json";
  ]

let suite = "json tests" >::: json_tests
let () = run_test_tt_main suite

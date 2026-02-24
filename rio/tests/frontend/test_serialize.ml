open Frontend
open OUnit2

let prog_dir = "../../../../../progs/"
let json_dir = "jsons/"

let prog_to_json filename =
  (* Actually convert to JSON *)
  let filewithpath = prog_dir ^ "work_conserving/" ^ filename ^ ".sched" in
  filewithpath |> Parser.parse_file |> Policy.of_program |> Policy.to_json

let expected_json file =
  (* Grab the _expected_ JSON object *)
  file |> Printf.sprintf "%s.json" |> ( ^ ) json_dir |> Yojson.Basic.from_file

let make_test filename =
  filename >:: fun _ ->
  assert_equal (expected_json filename) (prog_to_json filename)
    ~printer:Yojson.Basic.pretty_to_string ~cmp:Yojson.Basic.equal

let serialize_tests =
  [
    make_test "drop_class";
    (* If a class is not mentioned in the policy, it is dropped *)
    make_test "fifo_A";
    (* fifo[A] is stable *)
    make_test "union_ABC";
    (* union[A,B,C] is converted into fifo[A,B,C] *)
    make_test "strict_BA";
    (* strict[B,A] is stable *)
    make_test "rr_BAC";
    (* rr[B,A,C] is convered into rr[A,B,C] *)
    make_test "complex_tree";
    (* a complex tree wit wfq, rr, and strict policies and union used to merge classes
     * The children of the rr get sorted, the children of the strict stay in
     * the same order, and the union gets converted to fifo *)
  ]

let suite = "serialization tests" >::: serialize_tests
let () = run_test_tt_main suite

open Frontend
open OUnit2

let prog_dir = "../../../../../progs/"

let parse filename =
  filename |> ( ^ ) prog_dir |> Parser.parse_file |> Policy.of_program

let make_fail name exn =
  let filename = "incorrect/" ^ name ^ ".sched" in
  name >:: fun _ -> assert_raises exn (fun () -> parse filename)

let error_tests =
  [
    make_fail "unbound_class" (Policy.UndeclaredClass "Z");
    make_fail "unbound_var" (Policy.UnboundVariable "r_police");
    make_fail "duplicate_classes" (Policy.DuplicateClass "B");
    make_fail "duplicate_samepol" (Policy.DuplicateClass "A");
  ]

let suite = "parsing tests" >::: error_tests
let () = run_test_tt_main suite

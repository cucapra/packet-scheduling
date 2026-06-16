open Rio_core
open Frontend
open OUnit2

let prog_dir = "../progs/"

let parse filename =
  filename |> ( ^ ) prog_dir |> Parser.parse_file |> Pol.of_program

let make_fail name exn =
  let filename = "incorrect/" ^ name ^ ".sched" in
  name >:: fun _ -> assert_raises exn (fun () -> parse filename)

let error_tests =
  [
    make_fail "unbound_class" (Pol.UndeclaredClass "Z");
    make_fail "unbound_var" (Pol.UnboundVariable "r_police");
    make_fail "duplicate_classes" (Pol.DuplicateClass "B");
  ]

let suite = "parsing tests" >::: error_tests
let () = run_test_tt_main suite

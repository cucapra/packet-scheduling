open Rio_core
open Frontend
open OUnit2

let prog_dir = "../progs/"
let json_dir = "jsons/"

(* [.sched] file -> JSON value, going via the IR. *)
let compile_to_json filename =
  let c =
    prog_dir ^ filename |> Parser.parse_file |> Policy.of_program
    |> Ir.of_policy
  in
  Ir.Json.from_commit c.commit

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
    make_golden_test "rr[A, B, C]" "work_conserving/rr_ABC.sched" "rr_ABC.json";
    make_golden_test "complex_tree" "work_conserving/complex_tree.sched"
      "complex_tree.json";
  ]

(* The substrate consumes the JSON stream to allocate hardware. When a [v]
   becomes a designated super-node head, the lowering emits [Set_policy (v,
   SP*, 2)] right after [Designate]; the JSON must surface "SP*" verbatim
   so the hardware designer sees the super-node and provisions for it. *)
let sp_star_marker_test =
  "Set_policy with SP* surfaces \"SP*\" in JSON" >:: fun _ ->
  let j = Ir.Json.from_instr (Ir.Set_policy (100, Ir.SP_star, 2)) in
  let expected =
    `Assoc
      [
        ("op", `String "set_policy");
        ("v", `Int 100);
        ("pol", `String "SP*");
        ("n", `Int 2);
      ]
  in
  assert_equal ~printer:Yojson.Basic.pretty_to_string ~cmp:Yojson.Basic.equal
    expected j

let designate_round_trip_test =
  "Designate + Set_policy(SP*) JSON for a designate commit" >:: fun _ ->
  let commit : Ir.commit =
    [ Ir.Designate (100, 103); Ir.Set_policy (100, Ir.SP_star, 2) ]
  in
  let j = Ir.Json.from_commit commit in
  let expected =
    `List
      [
        `List
          [
            `Assoc
              [
                ("op", `String "designate");
                ("v", `Int 100);
                ("survivor", `Int 103);
              ];
            `Assoc
              [
                ("op", `String "set_policy");
                ("v", `Int 100);
                ("pol", `String "SP*");
                ("n", `Int 2);
              ];
          ];
      ]
  in
  assert_equal ~printer:Yojson.Basic.pretty_to_string ~cmp:Yojson.Basic.equal
    expected j

let sp_star_tests = [ sp_star_marker_test; designate_round_trip_test ]
let suite = "json tests" >::: json_tests @ sp_star_tests
let () = run_test_tt_main suite

open Ir
open OUnit2

(* Hand-built rr[A, B] *)
let rr_ab_program : program =
  let root = 0 in
  let a_leaf = 1 in
  let b_leaf = 2 in
  let s_a = 0 in
  let s_b = 1 in
  let pe1 = 1 in 
  let pe2 = 2 in
  [
    Spawn (root, pe1);
    Spawn (a_leaf, pe2);
    Spawn (b_leaf, pe2);
    Adopt (s_a, root, a_leaf);
    Adopt (s_b, root, b_leaf);
    Assoc (root, "A");
    Assoc (root, "B");
    Assoc (a_leaf, "A");
    Assoc (b_leaf, "B");
    Map (root, "A", s_a);
    Map (root, "B", s_b);
    Change_pol (root, RR, 2);
  ]

let expected_rr_ab =
  String.concat "\n"
    [
      "v0 = spawn(pe1)";
      "v1 = spawn(pe2)";
      "v2 = spawn(pe2)";
      "step_0 = adopt(v0, v1)";
      "step_1 = adopt(v0, v2)";
      "assoc(v0, A)";
      "assoc(v0, B)";
      "assoc(v1, A)";
      "assoc(v2, B)";
      "map(v0, A, step_0)";
      "map(v0, B, step_1)";
      "change_pol(v0, RR, 2)";
    ]

let test_rr_ab_printer =
  "rr[A, B] pretty-prints" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_rr_ab (string_of_program rr_ab_program)

let suite = "ir printer tests" >::: [ test_rr_ab_printer ]
let () = run_test_tt_main suite

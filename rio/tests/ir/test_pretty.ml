open Ir
open OUnit2

(* Handwritten rr[A, B] *)
let rr_ab_program : program =
  let root = 100 in
  let a_leaf = 101 in
  let b_leaf = 102 in
  let s_a = 1000 in
  let s_b = 1001 in
  let pe0 = 0 in
  let pe1 = 1 in
  [
    Spawn (root, pe0);
    Spawn (a_leaf, pe1);
    Spawn (b_leaf, pe1);
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
      "v100 = spawn(pe0)";
      "v101 = spawn(pe1)";
      "v102 = spawn(pe1)";
      "step_1000 = adopt(v100, v101)";
      "step_1001 = adopt(v100, v102)";
      "assoc(v100, A)";
      "assoc(v100, B)";
      "assoc(v101, A)";
      "assoc(v102, B)";
      "map(v100, A, step_1000)";
      "map(v100, B, step_1001)";
      "change_pol(v100, RR, 2)";
    ]

let test_rr_ab_printer =
  "rr[A, B] pretty-prints" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_rr_ab (string_of_program rr_ab_program)

let suite = "ir printer tests" >::: [ test_rr_ab_printer ]
let () = run_test_tt_main suite

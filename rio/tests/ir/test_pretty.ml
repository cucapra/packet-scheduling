open Frontend
open Ir
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

(* [.sched] file -> pretty-printed IR string. We project [Ir.of_policy]'s
   [compiled] result down to its [.prog] field, since [string_of_program]
   only knows about the bare instruction list. *)
let compile_to_pretty filename =
  let c =
    prog_dir ^ filename |> Parser.parse_file |> Policy.of_program
    |> Ir.of_policy
  in
  string_of_program c.prog

(* ----------------------------------------------------------------------- *)
(* rr[A, B]                                                                *)
(* ----------------------------------------------------------------------- *)

(* Handwritten IR for rr[A, B] — mirrors what [Ir.of_policy] produces for
   [progs/work_conserving/rr_AB.sched]. IDs follow the content-addressed
   encoding: root path [] → v1; children at [0],[1] → v10,v11; adopt steps
   at ([],0..1) → 20,21. *)
let rr_ab_program : program =
  let root = 1 in
  let a_leaf = 10 in
  let b_leaf = 11 in
  let s_a = 20 in
  let s_b = 21 in
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
      "v1 = spawn(pe0)";
      "v10 = spawn(pe1)";
      "v11 = spawn(pe1)";
      "step_20 = adopt(v1, v10)";
      "step_21 = adopt(v1, v11)";
      "assoc(v1, A)";
      "assoc(v1, B)";
      "assoc(v10, A)";
      "assoc(v11, B)";
      "map(v1, A, step_20)";
      "map(v1, B, step_21)";
      "change_pol(v1, RR, 2)";
    ]

let test_rr_ab_handwritten =
  "rr[A, B] handwritten IR pretty-prints correctly" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_rr_ab (string_of_program rr_ab_program)

let test_rr_ab_pipeline =
  "rr[A, B] compiled from .sched matches the same golden" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_rr_ab
    (compile_to_pretty "work_conserving/rr_AB.sched")

let suite =
  "ir printer tests" >::: [ test_rr_ab_handwritten; test_rr_ab_pipeline ]

let () = run_test_tt_main suite

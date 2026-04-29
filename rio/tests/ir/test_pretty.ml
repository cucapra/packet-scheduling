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
   [progs/work_conserving/rr_AB.sched] *)
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

(* Pretty-print of [rr_ab_program] above — the handwritten IR without a
   fake root, used to exercise [string_of_program] in isolation. *)
let expected_rr_ab_handwritten =
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

(* Pretty-print of what [Ir.of_policy] actually emits for rr[A, B]: the
   real tree above plus the fake root [v99] on PE -1 wrapping it. *)
let expected_rr_ab_compiled =
  String.concat "\n"
    [
      "v99 = spawn(pe-1)";
      "v100 = spawn(pe0)";
      "v101 = spawn(pe1)";
      "v102 = spawn(pe1)";
      "step_999 = adopt(v99, v100)";
      "step_1000 = adopt(v100, v101)";
      "step_1001 = adopt(v100, v102)";
      "assoc(v99, A)";
      "assoc(v99, B)";
      "assoc(v100, A)";
      "assoc(v100, B)";
      "assoc(v101, A)";
      "assoc(v102, B)";
      "map(v99, A, step_999)";
      "map(v99, B, step_999)";
      "map(v100, A, step_1000)";
      "map(v100, B, step_1001)";
      "change_pol(v99, UNION, 1)";
      "change_pol(v100, RR, 2)";
    ]

let test_rr_ab_handwritten =
  "rr[A, B] handwritten IR pretty-prints correctly" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_rr_ab_handwritten
    (string_of_program rr_ab_program)

let test_rr_ab_pipeline =
  "rr[A, B] compiled from .sched matches the same golden" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_rr_ab_compiled
    (compile_to_pretty "work_conserving/rr_AB.sched")

let suite =
  "ir printer tests" >::: [ test_rr_ab_handwritten; test_rr_ab_pipeline ]

let () = run_test_tt_main suite

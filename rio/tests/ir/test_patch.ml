open Frontend
open OUnit2
open Ir

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/work_conserving/"

let policy_of file =
  prog_dir ^ file ^ ".sched" |> Parser.parse_file |> Policy.of_program

let compile file = Ir.of_policy (policy_of file)

(* Patch [prev_file] toward [next_file]'s policy and return the [compiled]
   delta. Fails the test if [patch] gives up. *)
let patch_files prev_file next_file =
  let prev = compile prev_file in
  let next = policy_of next_file in
  match Ir.patch ~prev ~next with
  | Some c -> c
  | None ->
      assert_failure
        (Printf.sprintf "patch %s -> %s returned None" prev_file next_file)

let make_delta_test name prev_file next_file (expected : program) =
  name >:: fun _ ->
  let c = patch_files prev_file next_file in
  assert_equal ~printer:Ir.string_of_program expected c.prog

(* OneArmAppended *)

(* There's a walkthrough for this case in the topmatter of the PR. *)
let strict_ab_to_abc_expected : program =
  [
    Spawn (12, 1);
    Adopt (22, 1, 12);
    Assoc (1, "C");
    Assoc (12, "C");
    Map (1, "C", 22);
    Change_pol (1, SP, 3);
    Change_weight (1, 22, 3.0);
  ]

(* RR arm appended at the root. Same shape as SP but no Change_weight. *)
let rr_ab_to_abc_expected : program =
  [
    Spawn (12, 1);
    Adopt (22, 1, 12);
    Assoc (1, "C");
    Assoc (12, "C");
    Map (1, "C", 22);
    Change_pol (1, RR, 3);
  ]

(* Deep arm add. complex_tree's normalized root is WFQ with children sorted
   to (UNION, SP, RR) at indices 0, 1, 2. The inner RR (at path [2]) has
   parent vpifo [vpifo_of_path [2]] = 12 and arity 3; the new FIFO NEW lives
   at path [2; 3] (one level deeper than the RR), so its vpifo is
   [vpifo_of_path [2;3]] = 123, its adopt step is [step_of_path [2] 3] = 223,
   and PE = 2. *)
let complex_tree_add_deep_expected : program =
  [
    Spawn (123, 2);
    Adopt (223, 12, 123);
    Assoc (12, "NEW");
    Assoc (123, "NEW");
    Map (12, "NEW", 223);
    Change_pol (12, RR, 4);
  ]

let one_arm_app_tests =
  [
    make_delta_test "strict[A,B] -> strict[A,B,C]" "strict_AB" "strict_ABC"
      strict_ab_to_abc_expected;
    make_delta_test "rr[A,B] -> rr[A,B,C]" "rr_AB" "rr_ABC"
      rr_ab_to_abc_expected;
    make_delta_test "complex_tree -> complex_tree_add_arm_deep" "complex_tree"
      "complex_tree_add_arm_deep" complex_tree_add_deep_expected;
  ]

let suite = "patch tests" >::: one_arm_app_tests
let () = run_test_tt_main suite

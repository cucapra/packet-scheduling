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

(* For give-up tests: patch should return [None]. *)
let assert_gives_up prev_file next_file =
  let prev = compile prev_file in
  let next = policy_of next_file in
  match Ir.patch ~prev ~next with
  | None -> ()
  | Some _ ->
      assert_failure
        (Printf.sprintf "patch %s -> %s unexpectedly returned Some" prev_file
           next_file)

let make_delta_test name prev_file next_file (expected : program) =
  name >:: fun _ ->
  let c = patch_files prev_file next_file in
  assert_equal ~printer:Ir.string_of_program expected c.prog

let make_giveup_test name prev_file next_file =
  name >:: fun _ -> assert_gives_up prev_file next_file

(* --- Happy paths: OneArmAppended --- *)

(* ============================================================
   Walked-through example: SP arm appended at the root.

   This is the example to read first if you're new to [Ir.patch].
   Every later happy-path test uses the same skeleton; only the
   numbers change.

   prev (compiled from strict_AB.sched, i.e. SP[A, B]):

     IR program             vpifo / pe assignment         identities
     ------------------     -----------------------       ----------
     Spawn (100, 0)         v100 = SP root  on PE 0       vpifos:
     Spawn (101, 1)         v101 = FIFO A   on PE 1         []  -> 100
     Spawn (102, 1)         v102 = FIFO B   on PE 1         [0] -> 101
     Adopt (1000, 100, 101) "100 adopts 101 via step 1000"  [1] -> 102
     Adopt (1001, 100, 102) "100 adopts 102 via step 1001"
     Assoc (100, "A")       SP root forwards class A      steps:
     Assoc (100, "B")       SP root forwards class B        ([], 0) -> 1000
     Assoc (101, "A")       leaf 101 holds class A          ([], 1) -> 1001
     Assoc (102, "B")       leaf 102 holds class B
     Map   (100, "A", 1000) at the root, class A goes      next_vpifo = 103
                            via step 1000 (down to 101)    next_step  = 1002
     Map   (100, "B", 1001) class B goes via step 1001
     Change_pol (100, SP, 2)        root runs SP with 2 arms
     Change_weight (100, 1000, 1.0) class A is highest priority
     Change_weight (100, 1001, 2.0) class B is next

   next: SP[A, B, C] — one new FIFO arm appended at the end.

   What [Ir.patch] does, step by step:

   1. [Compare.analyze prev.policy next] returns
        Change ([], OneArmAppended (FIFO "C"))
      i.e. "at the root (path = []), one arm was appended; the new
      arm is FIFO C". The broader [ArmsAdded] case (mid-insert,
      multi-arm, weighted-arm) and the [VeryDifferent] / [SuperPol]
      results all cause patch to return None.

   2. Walk [prev.policy] along path = [] → the parent is the root
      itself: SP[A, B], so [pol_ty = SP] and [old_arity = 2].
      Look up the parent's vPIFO in [prev.identities.vpifos[[]]]:
      [parent_v = 100].

   3. Clone [prev.identities] (so the original tables aren't
      touched) and seed the counters from the snapshots:
      [fresh_v] starts at 103, [fresh_s] starts at 1002.

   4. Compile the new arm via the existing [compile_subtree] at
      [depth = 1], [path = [2]] (next free child slot under the
      root). FIFO C consumes one [fresh_v ()] = 103, registers
      [vpifos[[2]] = 103], and produces a tiny frag:
        spawns  = [Spawn (103, 1)]   (* PE 1 = depth *)
        assocs  = [Assoc (103, "C")]
        classes = ["C"]
        root_v  = 103
      All other frag fields are empty.

   5. Allocate the parent's adopt-step ID via [fresh_s ()] = 1002,
      register [steps[([], 2)] = 1002], and stitch the new arm onto
      the existing parent:
        Adopt (1002, 100, 103)        (* parent adopts the new arm *)
        Assoc (100, "C")              (* root now forwards class C *)
        Map   (100, "C", 1002)        (* class C goes via step 1002 *)
        Change_pol  (100, SP, 3)      (* arity bumped 2 -> 3 *)
        Change_weight (100, 1002, 3.0)
                                       (* SP-only: positional weight
                                          equals the new arity *)

   6. Concatenate the delta in the same grouped order [of_policy]
      uses (spawns -> adopts -> assocs -> maps -> change_pols ->
      change_weights), interleaving the parent's local instructions
      with the (here empty) deeper instructions from the frag.

   That's exactly the program below. RR/UNION look the same minus
   the [Change_weight] at the end (RR/UNION don't carry weights);
   deeper additions look the same with bigger IDs and a non-empty
   path. *)
let strict_ab_to_abc_expected : program =
  [
    (* From the new arm's frag.spawns: stand up the new vPIFO. *)
    Spawn (103, 1);
    (* Parent adopts the new arm via the freshly-allocated step. *)
    Adopt (1002, 100, 103);
    (* Parent learns to accept class C; the new leaf holds class C. *)
    Assoc (100, "C");
    Assoc (103, "C");
    (* Parent's routing table: class C goes through step 1002. *)
    Map (100, "C", 1002);
    (* Parent's arity grows from 2 to 3 (still SP). *)
    Change_pol (100, SP, 3);
    (* SP-only: the appended arm gets positional weight = new arity. *)
    Change_weight (100, 1002, 3.0);
  ]

(* RR arm appended at the root. Same shape as SP but no Change_weight. *)
let rr_ab_to_abc_expected : program =
  [
    Spawn (103, 1);
    Adopt (1002, 100, 103);
    Assoc (100, "C");
    Assoc (103, "C");
    Map (100, "C", 1002);
    Change_pol (100, RR, 3);
  ]

(* Deep arm add. complex_tree's normalized root is WFQ with children sorted
   to (UNION, SP, RR) at indices 0, 1, 2. The inner RR (at path [2]) has
   parent vpifo 108 and arity 3; complex_tree leaves the counters at
   next_vpifo=112, next_step=1011. New FIFO NEW lives one level below the
   RR, so PE 2. *)
let complex_tree_add_deep_expected : program =
  [
    Spawn (112, 2);
    Adopt (1011, 108, 112);
    Assoc (108, "NEW");
    Assoc (112, "NEW");
    Map (108, "NEW", 1011);
    Change_pol (108, RR, 4);
  ]

let happy_tests =
  [
    make_delta_test "strict[A,B] -> strict[A,B,C]" "strict_AB" "strict_ABC"
      strict_ab_to_abc_expected;
    make_delta_test "rr[A,B] -> rr[A,B,C]" "rr_AB" "rr_ABC"
      rr_ab_to_abc_expected;
    make_delta_test "complex_tree -> complex_tree_add_arm_deep" "complex_tree"
      "complex_tree_add_arm_deep" complex_tree_add_deep_expected;
  ]

(* --- Same: empty delta, but [policy] and counters still tracked. --- *)

let same_test =
  "rr[A,B,C] -> rr[A,B,C] is an empty delta" >:: fun _ ->
  let c = patch_files "rr_ABC" "rr_ABC" in
  assert_equal ~printer:Ir.string_of_program [] c.prog;
  (* Counters shouldn't have moved. *)
  let prev = compile "rr_ABC" in
  assert_equal ~printer:string_of_int prev.next_vpifo c.next_vpifo;
  assert_equal ~printer:string_of_int prev.next_step c.next_step

(* --- Identity tables on [prev] are untouched after a patch. --- *)

let prev_untouched_test =
  "patch leaves prev.identities untouched" >:: fun _ ->
  let prev = compile "rr_AB" in
  let prev_v_count = Hashtbl.length prev.identities.vpifos in
  let prev_s_count = Hashtbl.length prev.identities.steps in
  let next = policy_of "rr_ABC" in
  let _ = Ir.patch ~prev ~next in
  assert_equal ~printer:string_of_int prev_v_count
    (Hashtbl.length prev.identities.vpifos);
  assert_equal ~printer:string_of_int prev_s_count
    (Hashtbl.length prev.identities.steps)

(* --- Give-up cases: every change outside the supported scope returns None. *)

let giveup_tests =
  [
    make_giveup_test "rr[A,B] -> rr[D,E,F] (VeryDifferent)" "rr_AB" "rr_DEF";
    make_giveup_test "fifo[G] -> union[G,H] (SuperPol)" "fifo_G" "union_GH";
    make_giveup_test "strict[A,C] -> strict[A,B,C] (ArmsAdded mid-insert)"
      "strict_AC" "strict_ABC";
    make_giveup_test "wfq[B,A] -> wfq[A,B,C] (ArmsAdded WFQ arm-add)" "wfq_BA"
      "wfq_ABC";
    make_giveup_test "rr[A,B] -> rr[D,B,A,SP[C,E]] (ArmsAdded multi-arm)"
      "rr_AB" "rr_DBA_SP_CE";
  ]

let suite =
  "patch tests"
  >::: happy_tests @ [ same_test; prev_untouched_test ] @ giveup_tests

let () = run_test_tt_main suite

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

(* OneArmAdded *)

(* There's a walkthrough for this case in the topmatter of the PR. *)
let strict_ab_to_abc_expected : program =
  [
    Spawn (103, 1);
    Adopt (1002, 100, 103);
    Assoc (100, "C");
    Assoc (103, "C");
    Map (100, "C", 1002);
    Change_pol (100, SP, 3);
    Change_weight (100, 1002, 3.0);
  ]

(* Mid-insert in SP: prev SP(A,C) compiled with positional weights A:1, C:2.
   After inserting B at index 1, weights shift to A:1, B:2, C:3. The new arm
   itself gets weight k+1=2; the existing C, now at new index 2, gets bumped
   from 2.0 to 3.0. C's adopt step from compile is 1001 (A=1000, C=1001). *)
let strict_ac_to_abc_expected : program =
  [
    Spawn (103, 1);
    Adopt (1002, 100, 103);
    Assoc (100, "B");
    Assoc (103, "B");
    Map (100, "B", 1002);
    Change_pol (100, SP, 3);
    Change_weight (100, 1002, 2.0);
    Change_weight (100, 1001, 3.0);
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

let one_arm_added_tests =
  [
    make_delta_test "strict[A,B] -> strict[A,B,C]" "strict_AB" "strict_ABC"
      strict_ab_to_abc_expected;
    make_delta_test "strict[A,C] -> strict[A,B,C]" "strict_AC" "strict_ABC"
      strict_ac_to_abc_expected;
    make_delta_test "rr[A,B] -> rr[A,B,C]" "rr_AB" "rr_ABC"
      rr_ab_to_abc_expected;
    make_delta_test "complex_tree -> complex_tree_add_arm_deep" "complex_tree"
      "complex_tree_add_arm_deep" complex_tree_add_deep_expected;
  ]

(* WeightChanged *)

(* WFQ root with three FIFO arms: vpifo IDs 100 (root), 101/102/103 (A/B/C);
   adopt steps 1000/1001/1002. Bumping B's weight 1 -> 5 should emit a single
   Change_weight on the root for B's step. *)
let wfq_abc_to_one_weight_expected : program =
  [ Change_weight (100, 1001, 5.0) ]

let weight_changed_tests =
  [
    make_delta_test "wfq[A,B,C] -> wfq[A,B(5),C]" "wfq_ABC" "wfq_ABC_one_weight"
      wfq_abc_to_one_weight_expected;
  ]

(* OneArmRemoved *)

(* SP[A,B,C] -> SP[A,B]: drop C (last, index 2). No SP weight shifts since
   no siblings sit at j > k. C is FIFO leaf -> single GC and a single
   Deassoc inside the removed subtree. *)
let strict_abc_to_ab_expected : program =
  [
    Change_pol (100, SP, 2);
    Unmap (100, "C", 1002);
    Deassoc (100, "C");
    Deassoc (103, "C");
    Emancipate (1002, 100, 103);
    GC 103;
  ]

(* SP[A,B,C] -> SP[A,C]: drop B (mid, index 1). C, formerly at index 2 with
   weight 3.0, shifts to index 1 with weight 2.0. *)
let strict_abc_to_ac_expected : program =
  [
    Change_weight (100, 1002, 2.0);
    Change_pol (100, SP, 2);
    Unmap (100, "B", 1001);
    Deassoc (100, "B");
    Deassoc (102, "B");
    Emancipate (1001, 100, 102);
    GC 102;
  ]

(* RR[A,B,C] -> RR[A,B]: drop C from RR. No weight shifts (RR is unweighted). *)
let rr_abc_to_ab_expected : program =
  [
    Change_pol (100, RR, 2);
    Unmap (100, "C", 1002);
    Deassoc (100, "C");
    Deassoc (103, "C");
    Emancipate (1002, 100, 103);
    GC 103;
  ]

let one_arm_removed_tests =
  [
    make_delta_test "strict[A,B,C] -> strict[A,B]" "strict_ABC" "strict_AB"
      strict_abc_to_ab_expected;
    make_delta_test "strict[A,B,C] -> strict[A,C]" "strict_ABC" "strict_AC"
      strict_abc_to_ac_expected;
    make_delta_test "rr[A,B,C] -> rr[A,B]" "rr_ABC" "rr_AB"
      rr_abc_to_ab_expected;
  ]

(* OneArmReplaced *)

(* RR[A,B] -> RR[A,D]: replace B (vpifo 102, step 1001) with FIFO D. The
   new arm rides on step 1001; B's super-node is set up via Designate(102,
   103). Ancestor routing on root vpifo 100 shifts B → D (Unmap/Deassoc
   then Assoc/Map). Inner Deassoc drains the old leaf, GC marks 102. *)
let rr_ab_to_ad_expected : program =
  [
    Spawn (103, 1);
    Assoc (103, "D");
    Designate (102, 103);
    Deassoc (102, "B");
    Unmap (100, "B", 1001);
    Deassoc (100, "B");
    Assoc (100, "D");
    Map (100, "D", 1001);
    GC 102;
  ]

(* SP[A,B] -> SP[A,C]: same shape as RR but in an SP parent. Positional
   weights stay (slot 1 is still 2.0), so no Change_weight is emitted. *)
let strict_ab_to_ac_expected : program =
  [
    Spawn (103, 1);
    Assoc (103, "C");
    Designate (102, 103);
    Deassoc (102, "B");
    Unmap (100, "B", 1001);
    Deassoc (100, "B");
    Assoc (100, "C");
    Map (100, "C", 1001);
    GC 102;
  ]

let one_arm_replaced_tests =
  [
    make_delta_test "rr[A,B] -> rr[A,D]" "rr_AB" "rr_AD" rr_ab_to_ad_expected;
    make_delta_test "strict[A,B] -> strict[A,C]" "strict_AB" "strict_AC"
      strict_ab_to_ac_expected;
  ]

(* SubPol *)

(* SP[A, B, C] -> FIFO A. The FIFO leaf already lives inside prev as v101,
   adopted via step_1000 from the root v100. Re-rooting drops B (v102), C
   (v103) and the SP root (v100); we Emancipate v101 from its parent first
   so the runtime knows the relationship was severed before the parent gets
   collected. *)
let strict_abc_to_fifo_a_expected : program =
  [ Emancipate (1000, 100, 101); Change_root 101; GC 100; GC 102; GC 103 ]

let sub_pol_tests =
  [
    make_delta_test "strict[A,B,C] -> fifo[A]" "strict_ABC" "fifo_A"
      strict_abc_to_fifo_a_expected;
  ]

(* SuperPol *)

(* prev = strict[A,B,C] compiles to vpifos 100 (root)/101/102/103 with
   adopt steps 1000/1001/1002 and pes [0; 1]; counters end at next_v=104,
   next_s=1003. complex_tree normalizes to
   wfq[(union[G,H], 3); (sp[A,B,C], 1); (rr[D,E,F], 2)] (children sort by
   variant tag UNION < SP < RR), so prev sits at path [1]. The delta
   builds the new WFQ root (v=104, PE 2 — fresh, above prev's max PE 1),
   the UNION sibling (v=105 on PE 0, leaves v=106/107 on PE 1, internal
   adopt steps 1003/1004) and the RR sibling (v=108 on PE 0, leaves
   v=109/110/111 on PE 1, internal adopt steps 1005/1006/1007), then
   [Adopt]s the WFQ root's three children via steps 1008 (UNION), 1009
   (prev's root v=100), 1010 (RR). Each ancestor [Assoc]/[Map]s every
   class in its subtree; WFQ weights mirror the (pol, weight) sort
   order: 3, 1, 2. Final [Change_root 104] retargets the runtime at the
   new top. None of prev's vpifos are respawned. *)
let strict_abc_to_complex_tree_expected : program =
  [
    Spawn (104, 2);
    Spawn (105, 0);
    Spawn (106, 1);
    Spawn (107, 1);
    Spawn (108, 0);
    Spawn (109, 1);
    Spawn (110, 1);
    Spawn (111, 1);
    Adopt (1008, 104, 105);
    Adopt (1009, 104, 100);
    Adopt (1010, 104, 108);
    Adopt (1003, 105, 106);
    Adopt (1004, 105, 107);
    Adopt (1005, 108, 109);
    Adopt (1006, 108, 110);
    Adopt (1007, 108, 111);
    Assoc (104, "G");
    Assoc (104, "H");
    Assoc (104, "A");
    Assoc (104, "B");
    Assoc (104, "C");
    Assoc (104, "D");
    Assoc (104, "E");
    Assoc (104, "F");
    Assoc (105, "G");
    Assoc (105, "H");
    Assoc (106, "G");
    Assoc (107, "H");
    Assoc (108, "D");
    Assoc (108, "E");
    Assoc (108, "F");
    Assoc (109, "D");
    Assoc (110, "E");
    Assoc (111, "F");
    Map (104, "G", 1008);
    Map (104, "H", 1008);
    Map (104, "A", 1009);
    Map (104, "B", 1009);
    Map (104, "C", 1009);
    Map (104, "D", 1010);
    Map (104, "E", 1010);
    Map (104, "F", 1010);
    Map (105, "G", 1003);
    Map (105, "H", 1004);
    Map (108, "D", 1005);
    Map (108, "E", 1006);
    Map (108, "F", 1007);
    Change_pol (104, WFQ, 3);
    Change_pol (105, UNION, 2);
    Change_pol (108, RR, 3);
    Change_weight (104, 1008, 3.0);
    Change_weight (104, 1009, 1.0);
    Change_weight (104, 1010, 2.0);
    Change_root 104;
  ]

let super_pol_tests =
  [
    ( "strict[A,B,C] -> complex_tree (pes invariant)" >:: fun _ ->
      let c = patch_files "strict_ABC" "complex_tree" in
      assert_equal
        ~printer:(fun pes ->
          "[" ^ String.concat "; " (List.map string_of_int pes) ^ "]")
        [ 2; 0; 1 ] c.pes );
    make_delta_test "strict[A,B,C] -> complex_tree" "strict_ABC" "complex_tree"
      strict_abc_to_complex_tree_expected;
  ]

(* pes-extension regressions: when a OneArmAdded or OneArmReplaced inserts
   an arm whose internal depth pushes the tree deeper than [prev.pes]
   covered, [pes_extended_to_depth] should grow [pes] with fresh PEs above
   the existing max — keeping the "same depth ⇒ same PE" invariant for
   the new layers. No fixture pair fits this shape, so we build the
   policies inline. *)

(* prev = sp[A] (depth 1, pes [0; 1]); next = sp[A, rr[B, C]] (depth 2).
   The new arm at index 1 is rr[B, C]. arm_depth = 1, internal depth = 1,
   so the tree reaches depth 2 — one below prev's max. New layer takes
   PE 2 (fresh, above prev's max PE 1). *)
let one_arm_added_extends_pes_test =
  "sp[A] -> sp[A, rr[B,C]] (extends pes)" >:: fun _ ->
  let prev = Ir.of_policy (Policy.SP [ Policy.FIFO "A" ]) in
  let next =
    Policy.SP
      [ Policy.FIFO "A"; Policy.RR [ Policy.FIFO "B"; Policy.FIFO "C" ] ]
  in
  let c =
    match Ir.patch ~prev ~next with
    | Some c -> c
    | None -> assert_failure "patch returned None"
  in
  assert_equal
    ~printer:(fun pes ->
      "[" ^ String.concat "; " (List.map string_of_int pes) ^ "]")
    [ 0; 1; 2 ] c.pes;
  let expected : program =
    [
      Spawn (102, 1);
      Spawn (103, 2);
      Spawn (104, 2);
      Adopt (1003, 100, 102);
      Adopt (1001, 102, 103);
      Adopt (1002, 102, 104);
      Assoc (100, "B");
      Assoc (100, "C");
      Assoc (102, "B");
      Assoc (102, "C");
      Assoc (103, "B");
      Assoc (104, "C");
      Map (100, "B", 1003);
      Map (100, "C", 1003);
      Map (102, "B", 1001);
      Map (102, "C", 1002);
      Change_pol (100, SP, 2);
      Change_pol (102, RR, 2);
      Change_weight (100, 1003, 2.0);
    ]
  in
  assert_equal ~printer:Ir.string_of_program expected c.prog

let pes_extension_tests = [ one_arm_added_extends_pes_test ]

let suite =
  "patch tests"
  >::: one_arm_added_tests @ weight_changed_tests @ one_arm_removed_tests
       @ one_arm_replaced_tests @ sub_pol_tests @ super_pol_tests
       @ pes_extension_tests

let () = run_test_tt_main suite

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

(* prev = strict[A,B,C] (vpifos 100..103, pes [0;1]) sits inside
   complex_tree's normalized form at path [1]. The delta builds the new
   WFQ root and the UNION/RR siblings, [Adopt]s prev's root vpifo 100 in
   via a fresh step, and re-roots to the new WFQ. We assert structural
   invariants rather than an exact program: prev's vpifos must not be
   respawned, and the new pes must place the WFQ layer on a fresh PE
   (above prev's max PE 1) while reusing prev's PEs at depths >= 1. *)
let super_pol_tests =
  let prev_vpifos = [ 100; 101; 102; 103 ] in
  [
    ( "strict[A,B,C] -> complex_tree (no respawn, fresh PE above prev)"
    >:: fun _ ->
      let c = patch_files "strict_ABC" "complex_tree" in
      assert_equal
        ~printer:(fun pes ->
          "[" ^ String.concat "; " (List.map string_of_int pes) ^ "]")
        [ 2; 0; 1 ] c.pes;
      List.iter
        (fun instr ->
          match instr with
          | Spawn (v, _) ->
              assert_bool
                (Printf.sprintf
                   "delta should not respawn prev's vpifo %d (instr: %s)" v
                   (string_of_instr instr))
                (not (List.mem v prev_vpifos))
          | _ -> ())
        c.prog;
      let roots =
        List.filter_map
          (function Change_root v -> Some v | _ -> None)
          c.prog
      in
      assert_equal ~msg:"expected exactly one Change_root" 1 (List.length roots);
      assert_bool "Change_root target must not be a prev vpifo"
        (not (List.mem (List.hd roots) prev_vpifos));
      let adopts_of_prev_root =
        List.filter
          (function Adopt (_, _, child) -> child = 100 | _ -> false)
          c.prog
      in
      assert_equal ~msg:"prev's root should be Adopted exactly once" 1
        (List.length adopts_of_prev_root) );
  ]

let suite =
  "patch tests"
  >::: one_arm_added_tests @ weight_changed_tests @ one_arm_removed_tests
       @ one_arm_replaced_tests @ sub_pol_tests @ super_pol_tests

let () = run_test_tt_main suite

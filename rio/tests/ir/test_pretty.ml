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

(* Handwritten complex_tree:
   wfq[(strict[A, B, C], 1), (rr[D, E, F], 2), (union[G, H], 3)]

   Mirrors the program that [Ir.of_policy] would produce for
   [progs/work_conserving/complex_tree.sched]:
   - vPIFOs: leaves first (post-order), then subtree roots, then the wfq root.
   - Step IDs: allocated at adopt-time, post-order — inner-tree adopts get
     smaller step IDs than the outer wfq's adopts.
   - PEs: depth-based (wfq=PE0, subtree roots=PE1, leaves=PE2). *)
let complex_tree_program : program =
  let pe0 = 0 in
  let pe1 = 1 in
  let pe2 = 2 in
  (* vPIFOs *)
  let a = 100 in
  let b = 101 in
  let c = 102 in
  let strict_root = 103 in
  let d = 104 in
  let e = 105 in
  let f = 106 in
  let rr_root = 107 in
  let g = 108 in
  let h = 109 in
  let union_root = 110 in
  let wfq_root = 111 in
  (* Step IDs *)
  let s_strict_a = 1000 in
  let s_strict_b = 1001 in
  let s_strict_c = 1002 in
  let s_rr_d = 1003 in
  let s_rr_e = 1004 in
  let s_rr_f = 1005 in
  let s_union_g = 1006 in
  let s_union_h = 1007 in
  let s_wfq_strict = 1008 in
  let s_wfq_rr = 1009 in
  let s_wfq_union = 1010 in
  [
    (* Spawns: wfq root first, then each subtree (root before its leaves). *)
    Spawn (wfq_root, pe0);
    Spawn (strict_root, pe1);
    Spawn (a, pe2);
    Spawn (b, pe2);
    Spawn (c, pe2);
    Spawn (rr_root, pe1);
    Spawn (d, pe2);
    Spawn (e, pe2);
    Spawn (f, pe2);
    Spawn (union_root, pe1);
    Spawn (g, pe2);
    Spawn (h, pe2);
    (* Adopts: outer wfq's adopts first (steps 1008-1010 — allocated last but
       emitted first), then each inner subtree's adopts. *)
    Adopt (s_wfq_strict, wfq_root, strict_root);
    Adopt (s_wfq_rr, wfq_root, rr_root);
    Adopt (s_wfq_union, wfq_root, union_root);
    Adopt (s_strict_a, strict_root, a);
    Adopt (s_strict_b, strict_root, b);
    Adopt (s_strict_c, strict_root, c);
    Adopt (s_rr_d, rr_root, d);
    Adopt (s_rr_e, rr_root, e);
    Adopt (s_rr_f, rr_root, f);
    Adopt (s_union_g, union_root, g);
    Adopt (s_union_h, union_root, h);
    (* Assocs: wfq root takes every class, each subtree root takes its arms,
       each leaf takes its own. *)
    Assoc (wfq_root, "A");
    Assoc (wfq_root, "B");
    Assoc (wfq_root, "C");
    Assoc (wfq_root, "D");
    Assoc (wfq_root, "E");
    Assoc (wfq_root, "F");
    Assoc (wfq_root, "G");
    Assoc (wfq_root, "H");
    Assoc (strict_root, "A");
    Assoc (strict_root, "B");
    Assoc (strict_root, "C");
    Assoc (a, "A");
    Assoc (b, "B");
    Assoc (c, "C");
    Assoc (rr_root, "D");
    Assoc (rr_root, "E");
    Assoc (rr_root, "F");
    Assoc (d, "D");
    Assoc (e, "E");
    Assoc (f, "F");
    Assoc (union_root, "G");
    Assoc (union_root, "H");
    Assoc (g, "G");
    Assoc (h, "H");
    (* Maps: at the wfq root, every class routes via the step that adopted its
       subtree; inner roots route each class to its leaf. *)
    Map (wfq_root, "A", s_wfq_strict);
    Map (wfq_root, "B", s_wfq_strict);
    Map (wfq_root, "C", s_wfq_strict);
    Map (wfq_root, "D", s_wfq_rr);
    Map (wfq_root, "E", s_wfq_rr);
    Map (wfq_root, "F", s_wfq_rr);
    Map (wfq_root, "G", s_wfq_union);
    Map (wfq_root, "H", s_wfq_union);
    Map (strict_root, "A", s_strict_a);
    Map (strict_root, "B", s_strict_b);
    Map (strict_root, "C", s_strict_c);
    Map (rr_root, "D", s_rr_d);
    Map (rr_root, "E", s_rr_e);
    Map (rr_root, "F", s_rr_f);
    Map (union_root, "G", s_union_g);
    Map (union_root, "H", s_union_h);
    (* Policies. *)
    Change_pol (wfq_root, WFQ, 3);
    Change_pol (strict_root, SP, 3);
    Change_pol (rr_root, RR, 3);
    Change_pol (union_root, UNION, 2);
    (* Weights: wfq's per-arm weights, then strict's priorities (1.0 = highest). *)
    Change_weight (wfq_root, s_wfq_strict, 1.0);
    Change_weight (wfq_root, s_wfq_rr, 2.0);
    Change_weight (wfq_root, s_wfq_union, 3.0);
    Change_weight (strict_root, s_strict_a, 1.0);
    Change_weight (strict_root, s_strict_b, 2.0);
    Change_weight (strict_root, s_strict_c, 3.0);
  ]

let expected_complex_tree =
  String.concat "\n"
    [
      "v111 = spawn(pe0)";
      "v103 = spawn(pe1)";
      "v100 = spawn(pe2)";
      "v101 = spawn(pe2)";
      "v102 = spawn(pe2)";
      "v107 = spawn(pe1)";
      "v104 = spawn(pe2)";
      "v105 = spawn(pe2)";
      "v106 = spawn(pe2)";
      "v110 = spawn(pe1)";
      "v108 = spawn(pe2)";
      "v109 = spawn(pe2)";
      "step_1008 = adopt(v111, v103)";
      "step_1009 = adopt(v111, v107)";
      "step_1010 = adopt(v111, v110)";
      "step_1000 = adopt(v103, v100)";
      "step_1001 = adopt(v103, v101)";
      "step_1002 = adopt(v103, v102)";
      "step_1003 = adopt(v107, v104)";
      "step_1004 = adopt(v107, v105)";
      "step_1005 = adopt(v107, v106)";
      "step_1006 = adopt(v110, v108)";
      "step_1007 = adopt(v110, v109)";
      "assoc(v111, A)";
      "assoc(v111, B)";
      "assoc(v111, C)";
      "assoc(v111, D)";
      "assoc(v111, E)";
      "assoc(v111, F)";
      "assoc(v111, G)";
      "assoc(v111, H)";
      "assoc(v103, A)";
      "assoc(v103, B)";
      "assoc(v103, C)";
      "assoc(v100, A)";
      "assoc(v101, B)";
      "assoc(v102, C)";
      "assoc(v107, D)";
      "assoc(v107, E)";
      "assoc(v107, F)";
      "assoc(v104, D)";
      "assoc(v105, E)";
      "assoc(v106, F)";
      "assoc(v110, G)";
      "assoc(v110, H)";
      "assoc(v108, G)";
      "assoc(v109, H)";
      "map(v111, A, step_1008)";
      "map(v111, B, step_1008)";
      "map(v111, C, step_1008)";
      "map(v111, D, step_1009)";
      "map(v111, E, step_1009)";
      "map(v111, F, step_1009)";
      "map(v111, G, step_1010)";
      "map(v111, H, step_1010)";
      "map(v103, A, step_1000)";
      "map(v103, B, step_1001)";
      "map(v103, C, step_1002)";
      "map(v107, D, step_1003)";
      "map(v107, E, step_1004)";
      "map(v107, F, step_1005)";
      "map(v110, G, step_1006)";
      "map(v110, H, step_1007)";
      "change_pol(v111, WFQ, 3)";
      "change_pol(v103, SP, 3)";
      "change_pol(v107, RR, 3)";
      "change_pol(v110, UNION, 2)";
      "change_weight(v111, step_1008, 1.)";
      "change_weight(v111, step_1009, 2.)";
      "change_weight(v111, step_1010, 3.)";
      "change_weight(v103, step_1000, 1.)";
      "change_weight(v103, step_1001, 2.)";
      "change_weight(v103, step_1002, 3.)";
    ]

let test_complex_tree_printer =
  "complex_tree pretty-prints" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_complex_tree
    (string_of_program complex_tree_program)

let suite =
  "ir printer tests" >::: [ test_rr_ab_printer; test_complex_tree_printer ]

let () = run_test_tt_main suite

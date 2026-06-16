open Rio_core
open Frontend
open OUnit2

let prog_dir = "../progs/work_conserving/"

(* Parse a [.sched] file and resolve it to a normalized [Pol.t]. *)
let normalized file =
  prog_dir ^ file ^ ".sched" |> Parser.parse_file |> Pol.of_program

let make_test name file expected =
  name >:: fun _ ->
  assert_equal expected (normalized file) ~printer:Pol.to_string

let fifo c = Pol.FIFO c

let normalize_tests =
  [
    (* A class declared but absent from the policy is silently dropped. *)
    make_test "unused class is dropped" "drop_class" (fifo "A");
    (* fifo[A] is already in normal form. *)
    make_test "fifo[A] is stable" "fifo_A" (fifo "A");
    (* strict[(B, 1), (A, 2)]: SP normalizes by ascending rank. *)
    make_test "strict sorts arms by rank" "strict_BA"
      (Pol.SP ([ (fifo "B", 1.0); (fifo "A", 2.0) ], false));
    (* rr[B, A, C]: RR children are sorted. *)
    make_test "rr sorts its arms" "rr_BAC"
      (Pol.RR [ fifo "A"; fifo "B"; fifo "C" ]);
    (* wfq[(B, 1), (A, 2)]: slots sort by arm, weights ride along. *)
    make_test "wfq sorts slots, weights follow" "wfq_BA"
      (Pol.WFQ [ (fifo "A", 2.0); (fifo "B", 1.0) ]);
    (* A nested tree: RR children sort, SP children sort by rank, and the
       WFQ slots sort by arm. *)
    make_test "complex tree" "complex_tree"
      (Pol.WFQ
         [
           ( Pol.SP
               ([ (fifo "A", 1.0); (fifo "B", 2.0); (fifo "C", 3.0) ], false),
             1.0 );
           (Pol.RR [ fifo "D"; fifo "E"; fifo "F" ], 2.0);
           (Pol.RR [ fifo "G"; fifo "H" ], 3.0);
         ]);
  ]

let suite = "normalization tests" >::: normalize_tests
let () = run_test_tt_main suite

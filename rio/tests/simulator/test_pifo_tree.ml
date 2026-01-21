open OUnit2
open Simulator
open Util

let push_seq int_to_path data tree =
  List.fold_left (fun tree i -> Pifotree.push tree i (int_to_path i)) tree data

let flush tree =
  let rec flush_aux tree acc =
    match (Pifotree.pop tree, Pifotree.size tree) with
    | None, 0 -> List.rev acc
    | None, _ -> failwith "ERROR: `pop` returns None on non-empty tree"
    | Some _, 0 -> failwith "ERROR: `pop` returns Some _ on size 0 tree"
    | Some (v, tree), _ -> flush_aux tree (v :: acc)
  in
  flush_aux tree []

(* How this test works:
   1. push all of `data` (using `int_to_path` to make paths) into PIFO tree `tree`.
   2. repeatedly pop `tree` until empty.
   3. check `data` is permuted correctly, i.e. matches `permutation`. *)
let make_push_pop_test name data tree int_to_path permutation =
  name >:: fun _ ->
  assert_equal permutation
    (tree |> push_seq int_to_path data |> flush)
    ~printer:data_to_string

(* Basic Invariant Tests *)
let basic_tests =
  let topo = Topo.Node [ Star; Star ] in
  let tree = Pifotree.create topo in

  [
    ( "`pop` on newly created tree is `None`" >:: fun _ ->
      assert_equal None (Pifotree.pop tree) );
    ( "newly created tree has size zero" >:: fun _ ->
      assert_equal 0 (Pifotree.size tree) );
    ( "newly created tree is the correct shape" >:: fun _ ->
      assert_equal topo (Pifotree.to_topo tree) );
  ]

(* Leaf Tests *)
let leaf_tests =
  let tree = Pifotree.create Topo.Star in
  let int_to_path i = Pifotree.Foot (float_of_int i) in
  let zero_to_nine = consecutive 0 9 in

  [
    make_push_pop_test "push-pop test on leaf using data {0, 1, 2, ..., 9}"
      zero_to_nine tree int_to_path zero_to_nine;
    make_push_pop_test "push-pop test on leaf using small random data"
      [ 2; 3; 1; 4; 6; 5; 0 ] tree int_to_path [ 0; 1; 2; 3; 4; 5; 6 ];
    make_push_pop_test "push-pop test on leaf using large random data"
      [ 2; 3; 1; 4; 6; 5; 0; 10; 15; 14; 11; 12; 13; 7; 8; 9; 16; 19; 18; 17 ]
      tree int_to_path
      [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19 ];
  ]

(* One Level Topology Tests
          - - - O - - -
          | |  / \  | |
          O O O   O O O
          A B C   D E F
*)
let one_level_tests =
  let topo = Topo.Node [ Star; Star; Star; Star; Star; Star ] in
  let tree = Pifotree.create topo in
  let int_to_path i =
    let ptr =
      match infer_flow i with
      | A -> 0
      | B -> 1
      | C -> 2
      | D -> 3
      | E -> 4
      | F -> 5
    in
    let r = float_of_int i in
    Pifotree.Path (ptr, r, Foot r)
  in
  let zero_to_thirty_five, small_random, large_random =
    ( consecutive 0 35,
      [ 5; 1; 3; 6; 2; 0; 4 ],
      [ 4; 12; 1; 17; 9; 5; 15; 0; 13; 6; 3; 2; 10; 7; 8; 16; 14; 11; 18 ] )
  in

  [
    make_push_pop_test "one level push-pop test using data {0, 1, ..., 35}"
      zero_to_thirty_five tree int_to_path
      (List.stable_sort compare zero_to_thirty_five);
    make_push_pop_test "one level push-pop test using small random data"
      small_random tree int_to_path
      (List.stable_sort compare small_random);
    make_push_pop_test "one level push-pop test using large random data"
      large_random tree int_to_path
      (List.stable_sort compare large_random);
  ]

(* Two Level Topology Tests
          - - - - O - - - -
          |     /   \     |
          O    O     O    O
        /   \  C     D  /   \
       O     O         O     O
       A     B         E     F
*)
let two_level_tests =
  let topo =
    Topo.Node [ Topo.Node [ Star; Star ]; Star; Star; Topo.Node [ Star; Star ] ]
  in
  let tree = Pifotree.create topo in
  let int_to_path i =
    let open Pifotree in
    match infer_flow i with
    | A -> Path (0, 0.0, Path (0, 1.0, Foot 0.0))
    | B -> Path (0, 0.0, Path (1, 0.0, Foot 0.0))
    | C -> Path (1, 2.0, Foot 0.0)
    | D -> Path (2, 2.0, Foot 0.0)
    | E -> Path (3, 1.0, Path (0, 0.0, Foot 0.0))
    | F -> Path (3, 1.0, Path (1, 1.0, Foot 0.0))
  in
  let compare i j =
    let order = [ B; A; E; F; C; D ] in
    match (infer_flow i, infer_flow j) with
    | C, D | D, C -> 0
    | a, b ->
        let idex, jdex =
          ( order |> List.find_index (fun x -> x = a) |> Option.get,
            order |> List.find_index (fun x -> x = b) |> Option.get )
        in
        idex - jdex
  in
  let zero_to_twenty_five, small_random, large_random =
    ( consecutive 0 25,
      [ 7; 0; 4; 9; 2; 6; 10; 3; 5; 8; 1 ],
      [ 6; 12; 3; 10; 0; 9; 14; 5; 1; 8; 2; 7; 15; 11; 13; 4 ] )
  in

  [
    make_push_pop_test "two level push-pop test using data {0, 1, ..., 25}"
      zero_to_twenty_five tree int_to_path
      (List.stable_sort compare zero_to_twenty_five);
    make_push_pop_test "two level push-pop test using small random data"
      small_random tree int_to_path
      (List.stable_sort compare small_random);
    make_push_pop_test "two level push-pop test using large random data"
      large_random tree int_to_path
      (List.stable_sort compare large_random);
  ]

let suite =
  "PIFO Tree tests"
  >::: basic_tests @ leaf_tests @ one_level_tests @ two_level_tests

let () = run_test_tt_main suite

open RioSemantics
open OUnit2
open Random

(** A module for creating tests *)
module TestGenerator = struct
  exception QueryFormatException

  let trunc f = Float.trunc (f *. 100.) /. 100.

  (* Create a testing file *)
  let gen_tests queue_count filename num_cmds =
    let rec create_string num_cmds =
      if num_cmds = 0 then ""
      else
        let cmd = if Random.int 100 > 66 then 1 else 0 in
        let rank, weight, time =
          if cmd = 1 then (0., 0., 0.)
          else
            ( trunc (Random.float 50.),
              trunc (Random.float 50.),
              trunc (Random.float 50.) )
        in
        let idx = if cmd = 1 then 0 else Random.int queue_count in
        string_of_int cmd ^ " " ^ string_of_float rank ^ " "
        ^ string_of_float time ^ " " ^ string_of_float weight ^ " "
        ^ string_of_int idx ^ "\n"
        ^ create_string (num_cmds - 1)
    in

    let oc = open_out filename in
    Printf.fprintf oc "%s\n" (create_string num_cmds);
    close_out oc

  (* Parse query string to format *)
  let parse_to_query = function
    | [ cmd; p1; p2; p3; idx ] ->
        ( int_of_string cmd,
          float_of_string p1,
          float_of_string p2,
          float_of_string p3,
          int_of_string idx )
    | _ -> raise QueryFormatException

  (* Load data from file into query format *)
  let load_queries filename =
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    read_lines []
    |> List.map (String.split_on_char ' ')
    |> List.map parse_to_query
end

(** A functor for testing semantics with packet and queue modules *)
module SemanticsTester
    (Pkt : Packet.Packet
             with type t = float * float * float
              and type ord = float)
    (Q : Queue.Queue with type elt = Pkt.t) =
struct
  include Program.Program
  module S = Semantics.Semantics (Pkt) (Q)

  exception QueryFormatException

  let rec queuegen n = if n = 0 then [] else Q.empty :: queuegen (n - 1)

  (* Write packets to file *)
  let write_results filename results =
    let oc = open_out filename in
    Printf.fprintf oc "%s\n"
      (List.fold_right
         (fun pkt acc ->
           match pkt with
           | None -> "No packet\n" ^ acc
           | Some p ->
               string_of_float (Pkt.rank p)
               ^ " "
               ^ string_of_float (Pkt.time p)
               ^ " "
               ^ string_of_float (Pkt.weight p)
               ^ "\n")
         results "");
    close_out oc

  (* Generate a list of packets by pushing and popping a given state tuple *)
  let simulate (p, qs) lst =
    let rec aux (p, qs) = function
      | [] -> []
      | (cmd, p1, p2, p3, i) :: t ->
          if cmd = 1 then
            (* Command = 1 => push with packet and qs[i] *)
            aux (S.push ((p1, p2, p3), List.nth qs i, (p, qs))) t
          else if cmd = 0 then
            (* Command = 0 => pop *)
            let popped, new_state = S.pop (p, qs) in
            popped :: aux new_state t
          else (* No other commands are valid *)
            raise QueryFormatException
    in
    aux (p, qs) lst

  (* union clss generates a union of classes such that each class i has name clss[i] *)
  let union classes = Union (List.map (fun c -> Class c) classes)

  (* 3 Underlying Classes *)
  let three_classes =
    [
      (Fifo (union [ "A"; "B"; "C" ]), queuegen 3);
      (EarliestDeadline (union [ "X"; "Y"; "Z" ]), queuegen 3);
      (ShortestJobNext (union [ "D"; "E"; "F" ]), queuegen 3);
    ]

  (* 4 Underlying Classes *)
  let four_classes =
    [
      ( RoundRobin
          [ Fifo (union [ "A"; "B" ]); EarliestDeadline (union [ "C"; "D" ]) ],
        queuegen 4 );
      ( Strict
          [
            ShortestJobNext (union [ "X"; "Y" ]);
            Fifo (Class "Z");
            EarliestDeadline (Class "W");
          ],
        queuegen 4 );
      ( WeightedFair
          ( [ Fifo (Class "A"); EarliestDeadline (union [ "B"; "C"; "D" ]) ],
            [ 1; 2 ] ),
        queuegen 4 );
    ]

  (* 5 Underlying Classes *)
  let five_classes =
    [
      ( WeightedFair
          ( [
              Strict [ Fifo (Class "A"); ShortestJobNext (Class "B") ];
              RoundRobin [ Fifo (Class "C"); EarliestDeadline (Class "D") ];
            ],
            [ 3; 2 ] ),
        queuegen 5 );
      ( Strict
          [
            RoundRobin [ Fifo (Class "E"); EarliestDeadline (Class "F") ];
            Fifo (union [ "G"; "H"; "I" ]);
          ],
        queuegen 5 );
      (EarliestDeadline (union [ "J"; "K"; "L"; "M"; "N" ]), queuegen 5);
    ]

  (* 6 Underlying Classes *)
  let six_classes =
    [
      ( RoundRobin
          [
            Fifo (union [ "A"; "B" ]);
            WeightedFair ([ ShortestJobNext (Class "C") ], [ 1 ]);
            Fifo (union [ "D"; "E"; "F" ]);
          ],
        queuegen 6 );
      ( Strict
          [
            EarliestDeadline (union [ "X"; "Y"; "Z" ]);
            RoundRobin [ Fifo (Class "P") ];
            ShortestJobNext (union [ "Q"; "R"; "S" ]);
          ],
        queuegen 6 );
      ( WeightedFair
          ( [
              Fifo (union [ "A"; "B"; "C" ]);
              EarliestDeadline (union [ "D"; "E"; "F" ]);
            ],
            [ 2; 3 ] ),
        queuegen 6 );
    ]

  (* 7 Underlying Classes *)
  let seven_classes =
    [
      ( RoundRobin
          [
            Fifo (union [ "A"; "B"; "C" ]);
            ShortestJobNext (union [ "D"; "E" ]);
            EarliestDeadline (union [ "F"; "G" ]);
          ],
        queuegen 7 );
      ( WeightedFair
          ( [
              Strict [ Fifo (Class "H"); EarliestDeadline (Class "I") ];
              ShortestJobNext (union [ "J"; "K"; "L"; "M" ]);
            ],
            [ 3; 4 ] ),
        queuegen 7 );
      ( Strict
          [
            RoundRobin
              [ Fifo (union [ "N"; "O" ]); EarliestDeadline (Class "P") ];
            Fifo (Class "Q");
          ],
        queuegen 7 );
    ]

  (* 8 Underlying Classes *)
  let eight_classes =
    [
      ( WeightedFair
          ( [
              RoundRobin
                [ Fifo (union [ "A"; "B"; "C" ]); ShortestJobNext (Class "D") ];
              Strict
                [ Fifo (Class "E"); EarliestDeadline (union [ "F"; "G"; "H" ]) ];
            ],
            [ 1; 1 ] ),
        queuegen 8 );
      ( RoundRobin
          [
            EarliestDeadline (union [ "I"; "J"; "K"; "L" ]);
            ShortestJobNext (union [ "M"; "N"; "O"; "P" ]);
          ],
        queuegen 8 );
      ( Strict
          [
            WeightedFair ([ Fifo (Class "Q") ], [ 2 ]);
            ShortestJobNext (union [ "R"; "S"; "T"; "U" ]);
          ],
        queuegen 8 );
    ]

  (* 9 Underlying Classes *)
  let nine_classes =
    [
      ( WeightedFair
          ( [
              RoundRobin
                [ Fifo (union [ "A"; "B" ]); EarliestDeadline (Class "C") ];
              Strict
                [
                  Fifo (union [ "D"; "E" ]);
                  ShortestJobNext (union [ "F"; "G"; "H" ]);
                ];
            ],
            [ 1; 3 ] ),
        queuegen 9 );
      ( Strict
          [
            WeightedFair
              ([ Fifo (Class "I"); EarliestDeadline (Class "J") ], [ 2 ]);
            Fifo (union [ "K"; "L"; "M" ]);
          ],
        queuegen 9 );
      ( RoundRobin
          [
            EarliestDeadline (union [ "N"; "O"; "P" ]);
            ShortestJobNext (union [ "Q"; "R"; "S" ]);
          ],
        queuegen 9 );
    ]

  (* 10 Underlying Classes *)
  let ten_classes =
    [
      ( WeightedFair
          ( [
              Strict
                [
                  RoundRobin
                    [
                      Fifo (union [ "A"; "B"; "C" ]);
                      EarliestDeadline (Class "D");
                    ];
                  ShortestJobNext (union [ "E"; "F"; "G"; "H" ]);
                ];
              Fifo (union [ "I"; "J" ]);
            ],
            [ 2; 2 ] ),
        queuegen 10 );
      ( RoundRobin
          [
            Strict
              [
                Fifo (union [ "K"; "L"; "M" ]);
                EarliestDeadline (union [ "N"; "O" ]);
              ];
            ShortestJobNext (union [ "P"; "Q"; "R"; "S" ]);
          ],
        queuegen 10 );
      ( Strict
          [
            WeightedFair
              ( [ Fifo (union [ "T"; "U"; "V" ]); ShortestJobNext (Class "W") ],
                [ 3; 4 ] );
            EarliestDeadline (union [ "X"; "Y"; "Z" ]);
          ],
        queuegen 10 );
    ]
end

module Tester =
  SemanticsTester (Packet.PacketImpl) (Queue.QueueImpl (Packet.PacketImpl))

let () =
  TestGenerator.gen_tests 3 "data/test_3_classes.data" 100;
  TestGenerator.gen_tests 4 "data/test_4_classes.data" 100;
  TestGenerator.gen_tests 5 "data/test_5_classes.data" 100;
  TestGenerator.gen_tests 6 "data/test_6_classes.data" 100;
  TestGenerator.gen_tests 7 "data/test_7_classes.data" 100;
  TestGenerator.gen_tests 8 "data/test_8_classes.data" 100;
  TestGenerator.gen_tests 9 "data/test_9_classes.data" 100;
  TestGenerator.gen_tests 10 "data/test_10_classes.data" 100

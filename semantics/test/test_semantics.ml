open RioSemantics
open OUnit2

module SemanticsTester
    (Pkt : RioSemantics.Packet.Packet with type t = float * float * float)
    (Q : RioSemantics.Queue.Queue with type elt = Pkt.t) =
struct
  include RioSemantics.Program.Program
  module S = RioSemantics.Semantics.Semantics (Pkt) (Q)

  exception QueryFormatException

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

  (* Generate a list of packets by pushing and popping a given state tuple *)
  let simulate (p, qs) input_file =
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
    aux (p, qs) (load_queries input_file)

  (* union clss generates a union of classes such that each class i has name clss[i] *)
  let union classes = Union (List.map (fun c -> Class c) classes)

  (* 3 Underlying Classes *)
  let three_classes =
    [
      Fifo (union [ "A"; "B"; "C" ]);
      EarliestDeadline (union [ "X"; "Y"; "Z" ]);
      ShortestJobNext (union [ "D"; "E"; "F" ]);
    ]

  (* 4 Underlying Classes *)
  let four_classes =
    [
      RoundRobin
        [ Fifo (union [ "A"; "B" ]); EarliestDeadline (union [ "C"; "D" ]) ];
      Strict
        [
          ShortestJobNext (union [ "X"; "Y" ]);
          Fifo (Class "Z");
          EarliestDeadline (Class "W");
        ];
      WeightedFair
        ( [ Fifo (Class "A"); EarliestDeadline (union [ "B"; "C"; "D" ]) ],
          [ 1; 2 ] );
    ]

  (* 5 Underlying Classes *)
  let five_classes =
    [
      WeightedFair
        ( [
            Strict [ Fifo (Class "A"); ShortestJobNext (Class "B") ];
            RoundRobin [ Fifo (Class "C"); EarliestDeadline (Class "D") ];
          ],
          [ 3; 2 ] );
      Strict
        [
          RoundRobin [ Fifo (Class "E"); EarliestDeadline (Class "F") ];
          Fifo (union [ "G"; "H"; "I" ]);
        ];
      EarliestDeadline (union [ "J"; "K"; "L"; "M"; "N" ]);
    ]

  (* 6 Underlying Classes *)
  let six_classes =
    [
      RoundRobin
        [
          Fifo (union [ "A"; "B" ]);
          WeightedFair ([ ShortestJobNext (Class "C") ], [ 1 ]);
          Fifo (union [ "D"; "E"; "F" ]);
        ];
      Strict
        [
          EarliestDeadline (union [ "X"; "Y"; "Z" ]);
          RoundRobin [ Fifo (Class "P") ];
          ShortestJobNext (union [ "Q"; "R"; "S" ]);
        ];
      WeightedFair
        ( [
            Fifo (union [ "A"; "B"; "C" ]);
            EarliestDeadline (union [ "D"; "E"; "F" ]);
          ],
          [ 2; 3 ] );
    ]

  (* 7 Underlying Classes *)
  let seven_classes =
    [
      RoundRobin
        [
          Fifo (union [ "A"; "B"; "C" ]);
          ShortestJobNext (union [ "D"; "E" ]);
          EarliestDeadline (union [ "F"; "G" ]);
        ];
      WeightedFair
        ( [
            Strict [ Fifo (Class "H"); EarliestDeadline (Class "I") ];
            ShortestJobNext (union [ "J"; "K"; "L"; "M" ]);
          ],
          [ 3; 4 ] );
      Strict
        [
          RoundRobin [ Fifo (union [ "N"; "O" ]); EarliestDeadline (Class "P") ];
          Fifo (Class "Q");
        ];
    ]

  (* 8 Underlying Classes *)
  let eight_classes =
    [
      WeightedFair
        ( [
            RoundRobin
              [ Fifo (union [ "A"; "B"; "C" ]); ShortestJobNext (Class "D") ];
            Strict
              [ Fifo (Class "E"); EarliestDeadline (union [ "F"; "G"; "H" ]) ];
          ],
          [ 1; 1 ] );
      RoundRobin
        [
          EarliestDeadline (union [ "I"; "J"; "K"; "L" ]);
          ShortestJobNext (union [ "M"; "N"; "O"; "P" ]);
        ];
      Strict
        [
          WeightedFair ([ Fifo (Class "Q") ], [ 2 ]);
          ShortestJobNext (union [ "R"; "S"; "T"; "U" ]);
        ];
    ]

  (* 9 Underlying Classes *)
  let nine_classes =
    [
      WeightedFair
        ( [
            RoundRobin
              [ Fifo (union [ "A"; "B" ]); EarliestDeadline (Class "C") ];
            Strict
              [
                Fifo (union [ "D"; "E" ]);
                ShortestJobNext (union [ "F"; "G"; "H" ]);
              ];
          ],
          [ 1; 3 ] );
      Strict
        [
          WeightedFair
            ([ Fifo (Class "I"); EarliestDeadline (Class "J") ], [ 2 ]);
          Fifo (union [ "K"; "L"; "M" ]);
        ];
      RoundRobin
        [
          EarliestDeadline (union [ "N"; "O"; "P" ]);
          ShortestJobNext (union [ "Q"; "R"; "S" ]);
        ];
    ]

  (* 10 Underlying Classes *)
  let ten_classes =
    [
      WeightedFair
        ( [
            Strict
              [
                RoundRobin
                  [
                    Fifo (union [ "A"; "B"; "C" ]); EarliestDeadline (Class "D");
                  ];
                ShortestJobNext (union [ "E"; "F"; "G"; "H" ]);
              ];
            Fifo (union [ "I"; "J" ]);
          ],
          [ 2; 2 ] );
      RoundRobin
        [
          Strict
            [
              Fifo (union [ "K"; "L"; "M" ]);
              EarliestDeadline (union [ "N"; "O" ]);
            ];
          ShortestJobNext (union [ "P"; "Q"; "R"; "S" ]);
        ];
      Strict
        [
          WeightedFair
            ( [ Fifo (union [ "T"; "U"; "V" ]); ShortestJobNext (Class "W") ],
              [ 3; 4 ] );
          EarliestDeadline (union [ "X"; "Y"; "Z" ]);
        ];
    ]
end

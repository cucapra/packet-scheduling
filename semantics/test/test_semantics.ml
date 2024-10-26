open RioSemantics
open OUnit2

module SemanticsTester
    (Pkt : RioSemantics.Packet.Packet)
    (Q : RioSemantics.Queue.Queue with type elt = Pkt.t) =
struct
  include RioSemantics.Program.Program
  module S = RioSemantics.Semantics.Semantics (Pkt) (Q)

  (* Generate a list of packets by pushing and popping a given state tuple *)
  let rec simulate state = function
    | [] -> []
    | (cmd, pkt, q) :: t ->
        (* Command = true => push with packet and queue *)
        if cmd then simulate (S.push (pkt, q, state)) t
        else
          (* Command = false => pop *)
          let popped, new_state = S.pop state in
          popped :: simulate new_state t

  let programs =
    [
      (* 1. Simple FIFO with a Single Class *)
      Fifo (Class "A");
      (* 2. Union of Multiple Classes *)
      Fifo (Union [ Class "A"; Class "B"; Class "C" ]);
      (* 3. Earliest Deadline on a Union *)
      EarliestDeadline (Union [ Class "X"; Class "Y" ]);
      (* 4. Shortest Job Next with a Single Class *)
      ShortestJobNext (Class "Z");
      (* 5. Strict Combination of Two Streams *)
      Strict
        [ Fifo (Class "A"); ShortestJobNext (Union [ Class "B"; Class "C" ]) ];
      (* 6. RoundRobin with Three Streams *)
      RoundRobin
        [
          Fifo (Class "A");
          EarliestDeadline (Class "B");
          ShortestJobNext (Union [ Class "C"; Class "D" ]);
        ];
      (* 7. WeightedFair with Two Streams and Weights *)
      WeightedFair ([ Fifo (Class "A"); EarliestDeadline (Class "B") ], [ 3; 5 ]);
      (* 8. Complex Strict with a Nested RoundRobin *)
      Strict
        [
          RoundRobin
            [
              Fifo (Class "A"); ShortestJobNext (Union [ Class "B"; Class "C" ]);
            ];
          EarliestDeadline (Class "D");
        ];
      (* 9. WeightedFair with Nested Streams *)
      WeightedFair
        ( [
            Strict [ Fifo (Class "A"); EarliestDeadline (Class "B") ];
            ShortestJobNext (Union [ Class "C"; Class "D" ]);
          ],
          [ 1; 2 ] );
      (* 10. Deeply Nested Stream with Multiple Layers *)
      RoundRobin
        [
          Strict
            [
              EarliestDeadline (Union [ Class "X"; Class "Y" ]);
              RoundRobin
                [
                  ShortestJobNext (Class "Z");
                  WeightedFair
                    ( [
                        Fifo (Class "A");
                        Strict
                          [ EarliestDeadline (Class "B"); Fifo (Class "C") ];
                      ],
                      [ 4; 2 ] );
                ];
            ];
          Fifo (Union [ Class "D"; Class "E" ]);
        ];
    ]
end

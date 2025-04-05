type t = 
   | Foot of Rank.t
   | Path of int * Rank.t * t
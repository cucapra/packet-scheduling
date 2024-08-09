type t = (int * Rank.t) list
(* The _foot_ of this list has should have `foot` (i.e. `-1`) in the int slot.
   We only care about the rank of the foot. *)

let foot = -1

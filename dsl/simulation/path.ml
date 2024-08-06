type t = (int * Rank.t) list
(* The _foot_ of this list has should have `foot` (i.e. `-1`) in the int slot.
   We only care about the rank of the foot.

   Another way of writing this type is:
    `type t = (int * Rank.t) list * Rank.t`
   where the final rank is the singeton foot of the list.
   However, the existing version is a little easier to work with.
*)

let foot = -1

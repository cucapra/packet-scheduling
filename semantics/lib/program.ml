(** A module representation for Rio program forms. *)
module Program = struct
  type clss = string
  type set = Class of clss | Union of set list

  type stream =
    (* Set To Stream *)
    | Fifo of set
    | EarliestDeadline of set
    | ShortestJobNext of set
    (* Stream To Stream *)
    | RoundRobin of stream list
    | Strict of stream list
    | WeightedFair of stream list * int list

  type prog = stream (* Exportable type *)
end

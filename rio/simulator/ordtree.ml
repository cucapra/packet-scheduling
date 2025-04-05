type t =
  | Foot
  | Order of (t * Rank.t) list (* ranks must be unique *)

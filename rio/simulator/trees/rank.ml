type t = float * Time.t

let cmp (r1, t1) (r2, t2) =
  if r1 == r2 then Time.cmp t1 t2 else if r1 -. r2 < 0. then -1 else 1

let create f = (f, Time.epoch)
let create_for_pkt f pkt = (f, Packet.time pkt)
let to_string t = string_of_float (fst t)

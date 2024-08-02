type t = float * Time.t

let cmp (r1, t1) (r2, t2) =
  (* If the float portions are identical, try to break ties using time.
     Using this cmp function in out PIFOs will give us FIFO order in case
     of rank ties, which is what we want.
  *)
  if r1 == r2 then Time.cmp t1 t2 else if r1 -. r2 < 0. then -1 else 1

let create f t = (f, t)
let time (_, t) = t

let to_string showtimes (f, t) =
  if showtimes then Printf.sprintf "%.1f/%.1f" f (Time.to_float t)
  else Printf.sprintf "%.1f" f

type t = float (* in seconds *)

let cmp t1 t2 =
  let diff = t1 -. t2 in
  if diff < 0.0 then -1 else if diff = 0.0 then 0 else 1

let to_float t = t
let of_float t = t
let add_float t f = to_float t +. f

let of_ints sec usec =
  of_float (Int32.to_float sec +. float_of_string ("0." ^ Int32.to_string usec))

let epoch = -1.0
let terminus = Float.infinity

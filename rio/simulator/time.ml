type t = float (* in seconds *)

let of_ints sec usec =
  Int32.to_float sec +. float_of_string ("0." ^ Int32.to_string usec)

let epoch = -1.0
let terminus = Float.infinity

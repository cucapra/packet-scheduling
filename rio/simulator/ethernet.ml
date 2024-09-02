(* Ethernet Cstruct for preprocessor to work on. *)

[%%cstruct
type ethernet = {
  dst : uint8_t; [@len 6]
  src : uint8_t; [@len 6]
  ethertype : uint16_t;
}
[@@big_endian]]

open Pcap

type t = {
  len : int;
  time : Time.t;
  flow : string;
  pushed : Time.t option;
  popped : Time.t option;
}

exception UnknownMacAddress of string
exception PCAPNotFound of string
exception MalformedPCAPHeader of string

let time t = t.time
let flow t = t.flow
let len t = float_of_int t.len
let punch_in t time = { t with pushed = Some time }
let punch_out t time = { t with popped = Some time }

let mac_addr_to_flow = function
  | "10:10:10:10:10:10" -> "A"
  | "20:20:20:20:20:20" -> "B"
  | "30:30:30:30:30:30" -> "C"
  | "40:40:40:40:40:40" -> "D"
  | "50:50:50:50:50:50" -> "E"
  | "60:60:60:60:60:60" -> "F"
  | "70:70:70:70:70:70" -> "G"
  | n -> raise (UnknownMacAddress n)

let find_flow x =
  let hex = Printf.sprintf "%x" x in
  let n = String.length hex in
  let buf = Buffer.create ((3 * (n / 2)) - 1) in
  let f i c =
    Buffer.add_char buf c;
    if i mod 2 = 1 && i < n - 1 then Buffer.add_char buf ':' else ()
  in
  String.iteri f hex;
  Buffer.contents buf |> mac_addr_to_flow

let create_pkt h (ph, pb) =
  (* ph is the packet header; pb is the packet body. *)
  let module H = (val h : HDR) in
  let hex_to_int = function
    | `Hex s -> int_of_string ("0x" ^ s)
  in
  let time, size_incl =
    ( Time.of_ints (H.get_pcap_packet_ts_sec ph) (H.get_pcap_packet_ts_usec ph),
      H.get_pcap_packet_incl_len ph )
  in
  let src = hex_to_int (Hex.of_string (Ethernet.copy_ethernet_src pb)) in
  {
    time;
    len = Int32.to_int size_incl;
    flow = src |> find_flow;
    pushed = None;
    popped = None;
  }

let create_pcap_packets h body : t list =
  List.rev (Cstruct.fold (fun l p -> create_pkt h p :: l) (packets h body) [])

let pkts_from_file filename =
  let open_file filename =
    let fd =
      try Unix.(openfile filename [ O_RDONLY ] 0)
      with Unix.Unix_error _ -> raise (PCAPNotFound filename)
    in
    let ba =
      Bigarray.(
        array1_of_genarray
          (Mmap.V1.map_file fd Bigarray.char c_layout false [| -1 |]))
    in
    Unix.close fd;
    Cstruct.of_bigarray ba
  in
  let read_header filename =
    let buf = open_file filename in
    match Pcap.detect buf with
    | Some h -> (h, buf)
    | None -> raise (MalformedPCAPHeader filename)
  in
  let h, buf = read_header filename in
  let _, body = Cstruct.split buf sizeof_pcap_header in
  create_pcap_packets h body

let write_to_csv ts filename =
  let format_to_csv metas =
    let headers = "\"flow\", \"arrived\", \"length\", \"pushed\", \"popped\"" in
    let format_one_to_csv meta =
      let pushed, popped =
        match (meta.pushed, meta.popped) with
        | Some pushed', Some popped' -> (pushed', popped')
        | _, _ -> (0.0, 0.0)
      in
      Printf.sprintf "\"%s\",\"%f\",\"%d\",\"%f\",\"%f\"" meta.flow meta.time
        meta.len pushed popped
    in
    Printf.sprintf "%s\n%s" headers
      (String.concat "\n" (List.map format_one_to_csv metas))
  in
  let payload = format_to_csv ts in
  let ecsv = Csv.input_all (Csv.of_string payload) in
  Csv.save filename ecsv

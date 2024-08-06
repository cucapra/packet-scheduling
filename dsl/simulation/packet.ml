open Pcap
open Ethernet

type packet_header_t = { time : Time.t; size_incl : int32 }
type ethernet_t = { dst : int; src : int }

type packet_complete = {
  (* This is the type for all the information we get'll get
     about an individual packet from the PCAP. *)
  header : packet_header_t;
  payload : ethernet_t;
}

type t = {
  (* We will mostly pass around this lighter-weight metadata. *)
  len : int;
  dst : int;
  time : Time.t;
  flow : string;
  pushed : Time.t option;
  popped : Time.t option;
}

(* It is occasionally useful to read/write fields in the metadata. *)
let time t = t.time
let flow t = t.flow

let len t =
  (* The only user of this immediately converts it into a float, so we just do it here. *)
  float_of_int t.len

(* It will become useful to modify these timing fields as the packet enters and leaves
   the scheduler. This is just for the purposes of our visualization. *)
let punch_in t time = { t with pushed = Some time }
let punch_out t time = { t with popped = Some time }

let mac_addr_to_flow s : Frontend.Ast.clss =
  match s with
  | "10:10:10:10:10:10" -> "A"
  | "20:20:20:20:20:20" -> "B"
  | "30:30:30:30:30:30" -> "C"
  | "40:40:40:40:40:40" -> "D"
  | "50:50:50:50:50:50" -> "E"
  | "60:60:60:60:60:60" -> "F"
  | "70:70:70:70:70:70" -> "G"
  | n -> failwith Printf.(sprintf "Unknown MAC address: %s." n)

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

let complete_to_meta (p : packet_complete) =
  (* This is how we'll discard unnecessary packet information and keep just the metadata. *)
  {
    time = p.header.time;
    len = Int32.to_int p.header.size_incl;
    flow = p.payload.src |> find_flow;
    dst = p.payload.dst;
    pushed = None;
    popped = None;
  }

let create_pkt h (ph, pb) =
  (* ph is the packet header; pb is the packet body. *)
  let module H = (val h : HDR) in
  let hex_to_int = function `Hex s -> int_of_string ("0x" ^ s) in
  let header =
    {
      time =
        Time.of_ints
          (H.get_pcap_packet_ts_sec ph)
          (H.get_pcap_packet_ts_usec ph);
      size_incl = H.get_pcap_packet_incl_len ph;
    }
  in
  let payload =
    {
      src = hex_to_int (Hex.of_string (copy_ethernet_src pb));
      dst = hex_to_int (Hex.of_string (copy_ethernet_dst pb));
    }
  in
  (* For debugging etc, it is useful to compute the "complete" version and
     then discard some information to get the metadata we are interested in.
  *)
  complete_to_meta { header; payload }

let create_pcap_packets h body : t list =
  List.rev (Cstruct.fold (fun l p -> create_pkt h p :: l) (packets h body) [])

let pkts_from_file filename =
  let open_file filename =
    let fd = Unix.(openfile filename [ O_RDONLY ] 0) in
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
    | None ->
        failwith (Printf.sprintf "can't parse pcap header from %s" filename)
  in
  let h, buf = read_header filename in
  let _, body = Cstruct.split buf sizeof_pcap_header in
  create_pcap_packets h body

let write_to_csv ts filename =
  (* This is the CSV format that out plot.py method expects downstream. *)
  let format_to_csv metas =
    let headers =
      "\"flow\", \"dst\", \"arrived\", \"length\", \"pushed\", \"popped\""
    in
    let format_one_to_csv meta =
      let pushed, popped =
        match (meta.pushed, meta.popped) with
        | Some pushed', Some popped' ->
            (Time.to_float pushed', Time.to_float popped')
        | _, _ -> (0.0, 0.0)
      in
      Printf.sprintf "\"%s\",\"%d\",\"%f\",\"%d\",\"%f\",\"%f\"" meta.flow
        meta.dst (Time.to_float meta.time) meta.len pushed popped
    in
    Printf.sprintf "%s\n%s" headers
      (String.concat "\n" (List.map format_one_to_csv metas))
  in
  let payload = format_to_csv ts in
  let ecsv = Csv.input_all (Csv.of_string payload) in
  Csv.save filename ecsv

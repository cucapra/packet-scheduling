type flow =
  | A
  | B
  | C
  | D
  | E
  | F

let ( mod ) a b = (a mod b) + if a < 0 then b else 0

let infer_flow i =
  match i mod 6 with
  | 0 -> A
  | 1 -> B
  | 2 -> C
  | 3 -> D
  | 4 -> E
  | 5 -> F
  | _ -> failwith "Ruh Roh Raggy"

let flow_to_string = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"

let consecutive bot top =
  let rec consecutive_aux acc i =
    match i <= top with
    | true -> consecutive_aux (i :: acc) (i + 1)
    | false -> List.rev acc
  in
  consecutive_aux [] bot

let data_to_string data =
  let to_string i =
    Printf.sprintf "(%s, %d)" (i |> infer_flow |> flow_to_string) i
  in
  Printf.sprintf "[%s]" (String.concat "; " (List.map to_string data))

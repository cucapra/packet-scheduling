type 'a t = {
  heap : 'a Fheap.t;
  cmp : 'a -> 'a -> int;
}

let wrap cmp heap = { heap; cmp }
let create cmp = Fheap.create ~compare:cmp |> wrap cmp
let of_list l cmp = l |> Fheap.of_list ~compare:cmp |> wrap cmp
let size t f = Fheap.count t.heap ~f
let push t v = Fheap.add t.heap v |> wrap t.cmp

let pop t f =
  let rec pop_aux l acc v =
    match List.sort t.cmp l with
    | [] -> (v, acc)
    | h :: t -> (
        match v with
        | Some _ -> pop_aux t (h :: acc) v
        | None -> if f h then pop_aux t acc (Some h) else pop_aux t (h :: acc) v
        )
  in
  match pop_aux (Fheap.to_list t.heap) [] None with
  | None, _ -> None
  | Some v, l -> Some (v, of_list l t.cmp)

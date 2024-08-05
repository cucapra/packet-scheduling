type 'a t = { heap : 'a Fheap.t; cmp : 'a -> 'a -> int }

let ( let* ) = Option.bind
let wrap cmp heap = { heap; cmp }
let tuple_wrap cmp (a, heap) = (a, wrap cmp heap)

let tuple_wrap_opt cmp = function
  | Some tup -> Some (tuple_wrap cmp tup)
  | None -> None

let create cmp = Fheap.create ~compare:cmp |> wrap cmp

let pop t f =
  let* v = Fheap.to_list t.heap |> List.sort t.cmp |> List.find_opt f in
  let init = Fheap.create ~compare:t.cmp in
  let f acc x = if x = v then acc else Fheap.add acc x in
  Some (v, Fheap.fold t.heap ~init ~f |> wrap t.cmp)

let push t v = Fheap.add t.heap v |> wrap t.cmp
let top_exn t = Fheap.top_exn t.heap
let pop_if t f = Fheap.pop_if t.heap f |> tuple_wrap_opt t.cmp
let pop_exn t = Fheap.pop_exn t.heap |> tuple_wrap t.cmp
let length t = Fheap.length t.heap
let of_list l cmp = wrap cmp (Fheap.of_list l ~compare:cmp)
let count t f = Fheap.count t.heap ~f

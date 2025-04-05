type 'a t = ('a * Rank.t) Fheap.t

let create () = Fheap.create ~compare:(fun (_, a) (_, b) -> Rank.cmp a b)
let size = Fheap.length
let push t v r = Fheap.add t (v, r)

let pop t =
  match Fheap.pop t with
  | Some ((v, _), t') -> Some (v, t')
  | None -> None

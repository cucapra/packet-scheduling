type 'a t = {
  heap : ('a * Rank.t * int) Fheap.t;
  counter : int; (* for breaking rank ties in FIFO order *)
}

let create () =
  let cmp (_, a, i) (_, b, j) = if a = b then i - j else a - b in
  { heap = Fheap.create ~compare:cmp; counter = 0 }

let size t = Fheap.length t.heap

let push t v r =
  { heap = Fheap.add t.heap (v, r, t.counter); counter = t.counter + 1 }

let pop t =
  match Fheap.pop t.heap with
  | Some ((v, _, _), heap') -> Some (v, { t with heap = heap' })
  | None -> None

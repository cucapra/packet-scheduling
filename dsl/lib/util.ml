(* `replace_nth l n nth'` is `l` with the `n`th element replaced with `nth'` *)
let replace_nth l n nth' = List.mapi (fun i x -> if i = n then nth' else x) l

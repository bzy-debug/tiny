let index l x =
  let rec index_aux l x acc =
    match l with
    | [] -> failwith "index: cannot find"
    | h::t ->
        if h = x then acc
        else index_aux t x (acc+1) in
  index_aux l x 0

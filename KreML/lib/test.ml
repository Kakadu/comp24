let rec foldl folder acc l =
  match l, l with
  | [], [] -> acc
  | x::xs, [] -> foldl folder (folder acc xs) l 

let cons x xs =
  let fst = x and snd = x in
  let tail = xs in
  fst::tail


let a = 5

let (x, y) = a, 6

let t f =
  let some_fun (x, y, z)  = y in
  some_fun (y, y, y)
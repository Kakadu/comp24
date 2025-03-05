
let id x = x
let rec fold_right f acc xs =
  match xs with
  | [] -> acc
  | h:: tl -> f h (fold_right f acc tl)
let foldl f a bs = fold_right (fun b g x -> g (f x b)) id bs a

let main = print_int (foldl (fun x y -> x * y) 1 [1;2;3])

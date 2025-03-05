let rec map f xs k  =
  match xs with
  | [] -> k []
  | h::tl -> map f tl (fun tl -> k (f h :: tl))
let rec iter f xs =
  match xs with
  | [] -> ()
  | h::tl -> let w = f h in  iter f tl

let main = iter print_int (map (fun x -> x+1) [1;2;3] (fun x -> x))

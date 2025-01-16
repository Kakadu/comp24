let f x y =
  let a = x + 6 in
  let b = y + 322 in
  fun x -> x + a - b

let f1 = f 3
let f2 = f 0

let f11 = f1 3
let f22 = f2 0

let r1 = f11 3

let r2 = f22 0

let main =
  let () = print_int r1 in
  let () = print_int r2 in
  0
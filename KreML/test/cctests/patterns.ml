let f (a, b, c) (x::xs) =
  x + a - b * 2 - c

let x = 1, 1, 1
let f1 = f x
let res = f1 [5; 6]
let () = print_int res

let f2 = f (1, 1, 1)

let l = [5; 6]
let res = f2 l
let () = print_int res

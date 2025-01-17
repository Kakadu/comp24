let global = 10
let f x =
  let a = 5 in
  fun y -> x + a + y + global

let f1 = f 0
let f2 = f1 1

let () = print_int f2
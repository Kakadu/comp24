let f =
  let a = 5 in
  fun x -> x + a

let () =
  let res = f 5 in
  print_int res 


let f_inner x env = x + env
let f =
  let a = 5 in
  f_inner

let () =
  let res = f 5 5 in
  print_int res 
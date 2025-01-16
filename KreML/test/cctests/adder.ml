let add x y = x + y

let inc = add 1
let dec = add (-1)


let main =
  let r1 = inc 5 in
  let r2 = dec 5 in
  let () = print_int r1 in
  let () = print_int r2 in
  0
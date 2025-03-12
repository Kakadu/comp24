let x = 1

let f y = x + y

let x = 2

let g y = x + y

let main = let () = print_int (f 1) in 
  let () = print_int (g 1) in 0
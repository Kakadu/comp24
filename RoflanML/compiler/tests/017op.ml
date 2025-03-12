let ( + ) x y z = x + y - z

let ( - ) x y = x * y

let ( * ) x y = x + y

let main = let () = print_int ((+) 1 2 3) in 
let () = print_int (1 - 2) in 
let () = print_int ((2 * 3) 4) in 0
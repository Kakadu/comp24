let wrap f = if 1 = 1 then f else f

let test3 a b c =
  let a = print_int a in
  let b = print_int b in
  let c = print_int c in
  0

let apply_args a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let rez =
      (wrap apply_args 1 10 100 1000 10000 100000 1000000 10000000 100000000
         1000000000)
  in
  let () = print_int rez in
  0


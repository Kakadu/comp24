let rec fac n = if n <= 1 then 1 else n * another_fac (n - 1)
and another_fac n = if n <= 1 then 1 else n * fac (n - 1)

let main =
  let () = print_int (fac 4) in
  let () = print_int (another_fac 4) in
  0


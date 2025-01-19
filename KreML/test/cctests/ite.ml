let f b =
  if b then fun x -> x
  else fun x -> -x

let () =
  let a = f true 6 in
  let b = f false (-6) in
  let () = print_int a in
  print_int b
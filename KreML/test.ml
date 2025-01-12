let rec f a =
  if a > 0 then
    let x = 5 in
    let add x y = x + y in
    add (x + 6)
  else
    (fun x -> x)
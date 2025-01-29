let rec fac_cps x k = if x <= 1 then k 1 else fac_cps (x - 1) (fun res -> k (res * x))

let main =
  let id x = x in
  let () = print_int (fac_cps 10 id) in
  0

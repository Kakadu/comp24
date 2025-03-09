let fac n = 
  let rec fack n f = 
  if (n <= 1) then (f 1)
  else (fack (n-1) (fun x -> x * (f n)))
  in
  (fack n (fun x -> x));;
let main = (print_int (fac 3));;

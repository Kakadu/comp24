  $ ./run_closure_conversion.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> 
  if ((<= n) 1)
  then 1
  else ((* n) (fac ((- n) 1))))
  
  let main = let #t = (print_int (fac 4)) in
  0

  $ ./run_closure_conversion.exe < manytests/typed/006partial.ml
  let foo = (fun b -> 
  if b
  then (fun foo -> ((+ foo) 2))
  else (fun foo -> ((* foo) 10)))
  
  let foo = (fun x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))))
  
  let main = let #t = (print_int (foo 11)) in
  0

  $ ./run_closure_conversion.exe < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let #t = (print_int a) in
  let #t = (print_int b) in
  let #t = (print_int c) in
  ((+ a) ((* b) c)))
  
  let main = let foo = (foo 1) in
  let foo = (foo 2) in
  let foo = (foo 3) in
  let #t = (print_int foo) in
  0

  $ ./run_closure_conversion.exe < manytests/typed/006partial3.ml
  let foo = (fun a -> let #t = (print_int a) in
  (fun b -> let #t = (print_int b) in
  (fun c -> (print_int c))))
  
  let main = let #t = (((foo 4) 8) 9) in
  0
  $ ./run_closure_conversion.exe < manytests/typed/002fac.ml
  let rec fac_cps = (fun n k -> 
  if ((= n) 1)
  then (k 1)
  else ((fac_cps ((- n) 1)) (((fun k n p -> (k ((* p) n))) k) n)))
  
  let main = let #t = (print_int ((fac_cps 4) (fun print_int -> print_int))) in
  0

  $ ./run_closure_conversion.exe << EOF
  > let rec y x =
  >   let a z = y x + z in
  >   let b x = a 5 + x in
  >   b 5


  $ ./run_closure_conversion.exe << EOF
  > let f a b =
  >   let a x = b + x in
  >   let b x = a 1 + x in
  >   a 1 + b 2;;


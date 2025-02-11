DO NOT MODIFY AST
  $ ./run_closure_conversion.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> 
  if ((<= n) 1)
  then 1
  else ((* n) (fac ((- n) 1))))
  
  let main = 0

  $ ./run_closure_conversion.exe < manytests/typed/006partial.ml
  let foo = (fun b -> 
  if b
  then (fun foo -> ((+ foo) 2))
  else (fun foo -> ((* foo) 10)))
  
  let foo = (fun x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))))
  
  let main = 0

  $ ./run_closure_conversion.exe < manytests/typed/006partial2.ml
  let foo = (fun a b c -> ((+ a) ((* b) c)))
  
  let main = let foo = (foo 1) in
  let foo = (foo 2) in
  let foo = (foo 3) in
  0

  $ ./run_closure_conversion.exe < manytests/typed/006partial3.ml
  let foo = (fun a -> (fun b -> (fun c -> (print_int c))))
  
  let main = 0

MODIFY AST 
  $ ./run_closure_conversion.exe < manytests/typed/002fac.ml
  let rec fac_cps = (fun n k -> 
  if ((= n) 1)
  then (k 1)
  else ((fac_cps ((- n) 1)) (((fun k n p -> (k ((* p) n))) k) n)))
  
  let main = 0

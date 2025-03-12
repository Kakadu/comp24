  $ dune exec ./demoAC.exe << EOF
  > let rec fac_cps n k =
  >   if n=1 then k 1 else
  >   fac_cps (n-1) (fun p -> k (p*n))
  > let main =
  >   let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
  >   0
  > EOF
  
  let rec fac_cps n k = if ( = ) n 1 then k 1 else fac_cps (( - ) n 1) (fun p -> k (( * ) p n))
  
  let main = 
  let () = print_int (fac_cps 4 (fun alpha_0 -> alpha_0)) in 0

  $ dune exec ./demoAC.exe << EOF
  > let f x = let x = 1 in let x = 2 in let x = 3 in x + 1
  > EOF
  
  let f x = 
  let alpha_0 = 1 in 
  let alpha_1 = 2 in 
  let alpha_2 = 3 in ( + ) alpha_2 1

  $ dune exec ./demoAC.exe << EOF
  > let x = 1
  > let x = 2
  > let x = 3
  > let f x y = x + y
  > let () = x + (f x 1)
  > EOF
  
  let x = 1
  
  let alpha_0 = 2
  
  let alpha_1 = 3
  
  let f alpha_2 y = ( + ) alpha_2 y
  
  let () = ( + ) alpha_1 (f alpha_1 1)

  $ dune exec ./demoAC.exe << EOF
  > let f x = let a = 1 in let a = a in x + a
  > EOF
  
  let f x = 
  let a = 1 in 
  let alpha_0 = a in ( + ) x alpha_0

  $ dune exec ./demoAC.exe << EOF
  > let wrap f = if 1 = 1 then f else f
  > let test3 a b c =
  >  let a = print_int a in
  >  let b = print_int b in
  >  let c = print_int c in
  >  a
  > let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j
  > let main =
  > let rez =
  >     (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
  >        1000000000)
  > in
  > let () = print_int rez in
  > let temp2 = wrap test3 1 10 100 in
  > 0
  
  let wrap f = if ( = ) 1 1 then f else f
  
  let test3 a b c = 
  let alpha_0 = print_int a in 
  let alpha_1 = print_int b in 
  let alpha_2 = print_int c in alpha_0
  
  let test10 a b c d e f g h i j = ( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) a b) c) d) e) f) g) h) i) j
  
  let main = 
  let rez = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in 
  let () = print_int rez in 
  let temp2 = wrap test3 1 10 100 in 0
  $ dune exec ./demoAC.exe << EOF
  > let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)
  > 
  > let foo x = foo true (foo false (foo true (foo false x)))
  > let main =
  > let () = print_int (foo 11) in
  > 0
  
  let foo b = if b then (fun foo -> ( + ) foo 2) else (fun foo -> ( * ) foo 10)
  
  let alpha_0 x = foo true (foo false (foo true (foo false x)))
  
  let main = 
  let () = print_int (alpha_0 11) in 0

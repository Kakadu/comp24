  $ dune exec ./demoANF.exe << EOF
  > let q = 1 + 2 + 3 * 4 + 5
  > EOF
  
  let q = 
  let anf_0 = ( + ) 1 2 in 
  let anf_1 = ( * ) 3 4 in 
  let anf_2 = ( + ) anf_0 anf_1 in 
  let anf_3 = ( + ) anf_2 5 in anf_3

  $ dune exec ./demoANF.exe << EOF
  > let my_true x = true
  > let q = let f x y z = x + y + z in (f 1 2 3, my_true true, my_true (fun x -> x))
  > EOF
  
  let my_true x = true
  
  let ll_0 x y z = 
  let anf_4 = ( + ) x y in 
  let anf_5 = ( + ) anf_4 z in anf_5
  
  let ll_1 x = x
  
  let q = 
  let anf_0 = ll_0 1 2 3 in 
  let anf_1 = my_true true in 
  let anf_2 = my_true ll_1 in 
  let anf_3 = anf_0, anf_1, anf_2 in anf_3

  $ dune exec ./demoANF.exe << EOF
  > let q = let f x y z = x + y + z in let g x = f 1 2 in g 3
  > EOF
  
  let ll_0 x y z = 
  let anf_2 = ( + ) x y in 
  let anf_3 = ( + ) anf_2 z in anf_3
  
  let ll_1 f x = 
  let anf_1 = ll_0 1 2 in anf_1
  
  let q = 
  let anf_0 = ll_1 ll_0 3 in anf_0

  $ dune exec ./demoANF.exe << EOF
  > let q = if true && false then let f x = x + 1 in f 1 else let g x = x - 1 in g 1
  > EOF
  
  let ll_0 x = 
  let anf_5 = ( + ) x 1 in anf_5
  
  let ll_1 x = 
  let anf_4 = ( - ) x 1 in anf_4
  
  let q = 
  let anf_0 = ( && ) true false in 
  let anf_1 = if anf_0 then 
  let anf_2 = ll_0 1 in anf_2 else 
  let anf_3 = ll_1 1 in anf_3 in anf_1

  $ dune exec ./demoANF.exe << EOF
  > let q = let x = 1 in let y = 2 in x / y
  > EOF
  
  let q = 
  let x = 1 in 
  let y = 2 in 
  let anf_0 = ( / ) x y in anf_0

  $ dune exec ./demoANF.exe << EOF
  > let q = [(fun x -> x * x) 1; (fun x -> x / x) 2]
  > EOF
  
  let ll_0 x = 
  let anf_4 = ( * ) x x in anf_4
  
  let ll_1 x = 
  let anf_3 = ( / ) x x in anf_3
  
  let q = 
  let anf_0 = ll_0 1 in 
  let anf_1 = ll_1 2 in 
  let anf_2 = [ anf_0; anf_1 ] in anf_2


  $ dune exec ./demoANF.exe << EOF
  > let rec fact x k = 
  > if x = 2 then k 1 else fact (x - 1) (fun n -> k ( x * n)) 
  > EOF
  
  let ll_0 k x n = 
  let anf_6 = ( * ) x n in 
  let anf_7 = k anf_6 in anf_7
  
  let rec fact x k = 
  let anf_0 = ( = ) x 2 in 
  let anf_1 = if anf_0 then 
  let anf_2 = k 1 in anf_2 else 
  let anf_3 = ( - ) x 1 in 
  let anf_4 = ll_0 k x in 
  let anf_5 = fact anf_3 anf_4 in anf_5 in anf_1

  $ dune exec ./demoANF.exe << EOF
  > let anf_1 x = x + 1
  > let q = let anf_1 = anf_1 (anf_1 (1 + 2)) in anf_1
  > EOF
  
  let anf_1 x = 
  let anf_4 = ( + ) x 1 in anf_4
  
  let q = 
  let anf_0 = ( + ) 1 2 in 
  let anf_2 = anf_1 anf_0 in 
  let anf_3 = anf_1 anf_2 in 
  let anf_1 = anf_3 in anf_1

  $ dune exec ./demoANF.exe << EOF
  > let rec even x = 
  > if x = 0 then true else odd (x - 1) 
  > and 
  > odd x = if x = 0 then false else even (x - 1)
  > EOF
  
  let rec even = (fun x -> 
  let anf_4 = ( = ) x 0 in 
  let anf_5 = if anf_4 then true else 
  let anf_6 = ( - ) x 1 in 
  let anf_7 = odd anf_6 in anf_7 in anf_5)
  and
  odd = (fun x -> 
  let anf_0 = ( = ) x 0 in 
  let anf_1 = if anf_0 then false else 
  let anf_2 = ( - ) x 1 in 
  let anf_3 = even anf_2 in anf_3 in anf_1)

  $ dune exec ./demoANF.exe << EOF
  > let rec fac_cps n k =
  > if n=1 then k 1 else
  > fac_cps (n-1) (fun p -> k (p*n))
  > 
  > let main =
  > let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
  > 0
  > EOF
  
  let ll_0 k n p = 
  let anf_8 = ( * ) p n in 
  let anf_9 = k anf_8 in anf_9
  
  let rec fac_cps n k = 
  let anf_2 = ( = ) n 1 in 
  let anf_3 = if anf_2 then 
  let anf_4 = k 1 in anf_4 else 
  let anf_5 = ( - ) n 1 in 
  let anf_6 = ll_0 k n in 
  let anf_7 = fac_cps anf_5 anf_6 in anf_7 in anf_3
  
  let ll_1 print_int = print_int
  
  let main = 
  let anf_0 = fac_cps 4 ll_1 in 
  let anf_1 = print_int anf_0 in 
  let () = anf_1 in 0

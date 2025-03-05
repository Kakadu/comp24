  $ dune exec ./demoANF.exe << EOF
  > let q = 1 + 2 + 3 * 4 + 5
  > EOF
  
  let q = 
  let ANF_0 = ( + ) 1 2 in 
  let ANF_1 = ( * ) 3 4 in 
  let ANF_2 = ( + ) ANF_0 ANF_1 in 
  let ANF_3 = ( + ) ANF_2 5 in ANF_3

  $ dune exec ./demoANF.exe << EOF
  > let q = let my_true x = true in let f x y z = x + y + z in (f 1 2 3, my_true true, my_true (fun x -> x))
  > EOF
  
  let LL_0 x = true
  
  let LL_1 x y z = 
  let ANF_4 = ( + ) x y in 
  let ANF_5 = ( + ) ANF_4 z in ANF_5
  
  let LL_2 x = x
  
  let q = 
  let ANF_0 = LL_1 1 2 3 in 
  let ANF_1 = LL_0 true in 
  let ANF_2 = LL_0 LL_2 in 
  let ANF_3 = ANF_0, ANF_1, ANF_2 in ANF_3

  $ dune exec ./demoANF.exe << EOF
  > let q = let f x y z = x + y + z in let g x = f 1 2 in g 3
  > EOF
  
  let LL_0 x y z = 
  let ANF_2 = ( + ) x y in 
  let ANF_3 = ( + ) ANF_2 z in ANF_3
  
  let LL_1 f x = 
  let ANF_1 = LL_0 1 2 in ANF_1
  
  let q = 
  let ANF_0 = LL_1 LL_0 3 in ANF_0

  $ dune exec ./demoANF.exe << EOF
  > let q = if true && false then let f x = x + 1 in f 1 else let g x = x - 1 in g 1
  > EOF
  
  let LL_0 x = 
  let ANF_5 = ( + ) x 1 in ANF_5
  
  let LL_1 x = 
  let ANF_4 = ( - ) x 1 in ANF_4
  
  let q = 
  let ANF_0 = ( && ) true false in 
  let ANF_1 = if ANF_0 then 
  let ANF_2 = LL_0 1 in ANF_2 else 
  let ANF_3 = LL_1 1 in ANF_3 in ANF_1

  $ dune exec ./demoANF.exe << EOF
  > let q = let x = 1 in let y = 2 in x / y
  > EOF
  
  let q = 
  let x = 1 in 
  let y = 2 in 
  let ANF_0 = ( / ) x y in ANF_0

  $ dune exec ./demoANF.exe << EOF
  > let q = [(fun x -> x * x) 1; (fun x -> x / x) 2]
  > EOF
  
  let LL_0 x = 
  let ANF_4 = ( * ) x x in ANF_4
  
  let LL_1 x = 
  let ANF_3 = ( / ) x x in ANF_3
  
  let q = 
  let ANF_0 = LL_0 1 in 
  let ANF_1 = LL_1 2 in 
  let ANF_2 = [ ANF_0; ANF_1 ] in ANF_2





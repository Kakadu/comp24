  $ dune exec ./demoANF.exe << EOF
  > let q = 1 + 2 + 3 * 4 + 5
  > EOF
  
  let q = 
  let anf_0 = ( + ) 1 2 in 
  let anf_1 = ( * ) 3 4 in 
  let anf_2 = ( + ) anf_0 anf_1 in 
  let anf_3 = ( + ) anf_2 5 in anf_3

  $ dune exec ./demoANF.exe << EOF
  > let q = let my_true x = true in let f x y z = x + y + z in (f 1 2 3, my_true true, my_true (fun x -> x))
  > EOF
  
  let ll_0 x = true
  
  let ll_1 x y z = 
  let anf_4 = ( + ) x y in 
  let anf_5 = ( + ) anf_4 z in anf_5
  
  let ll_2 x = x
  
  let q = 
  let anf_0 = ll_1 1 2 3 in 
  let anf_1 = ll_0 true in 
  let anf_2 = ll_0 ll_2 in 
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





  $ dune exec ./demoLL.exe << EOF
  > let q = let f x = x in f 10
  > EOF
  
  let ll_0 x = x
  
  let q = ll_0 10

  $ dune exec ./demoLL.exe << EOF
  > let q = let f x = x + 1 in let g x = f x in g 10
  > EOF
  
  let ll_0 x = ( + ) x 1
  
  let ll_1 f x = ll_0 x
  
  let q = ll_1 ll_0 10

  $ dune exec ./demoLL.exe << EOF
  > let q = let f x = x + 1 in let g x = f x in let k x = g x in let q x = k x in q 1
  > EOF
  
  let ll_0 x = ( + ) x 1
  
  let ll_1 f x = ll_0 x
  
  let ll_2 f g x = ll_1 ll_0 x
  
  let ll_3 f g k x = ll_2 ll_0 ll_1 x
  
  let q = ll_3 ll_0 ll_1 ll_2 1

  $ dune exec ./demoLL.exe << EOF
  > let q = let x = 1 in let y = 2 in fun z -> x + y + z
  > EOF
  
  let ll_0 x y z = ( + ) (( + ) x y) z
  
  let q = 
  let x = 1 in 
  let y = 2 in ll_0 x y

  $ dune exec ./demoLL.exe << EOF
  > let q = let x = 1 in (fun f y -> (f x) + x + 1) (fun z -> x + z)
  > EOF
  
  let ll_0 x f y = ( + ) (( + ) (f x) x) 1
  
  let ll_1 x z = ( + ) x z
  
  let q = 
  let x = 1 in ll_0 x (ll_1 x)

  $ dune exec ./demoLL.exe << EOF
  > let f x = print_int x
  > EOF
  
  let f x = print_int x

  $ dune exec ./demoLL.exe << EOF
  > let fs = (let y = 1 in fun x -> x + y), (let y = true in fun x -> x && y)
  > EOF
  
  let ll_0 y x = ( + ) x y
  
  let ll_1 y x = ( && ) x y
  
  let fs = 
  let y = 1 in ll_0 y, 
  let y = true in ll_1 y

  $ dune exec ./demoLL.exe << EOF
  > let rec fact x k = 
  > if x = 2 then k 1 else fact (x - 1) (fun n -> k ( x * n)) 
  > EOF
  
  let ll_0 k x n = k (( * ) x n)
  
  let rec fact x k = if ( = ) x 2 then k 1 else fact (( - ) x 1) (ll_0 k x)

  $ dune exec ./demoLL.exe << EOF
  > let ll_0 x = x
  > let ll_1 x = x + 1
  > let ll_2 x = x + 2
  > let f x = let g x = x + x in g (x / 2)
  > EOF
  
  let ll_0 x = x
  
  let ll_1 x = ( + ) x 1
  
  let ll_2 x = ( + ) x 2
  
  let ll_3 x = ( + ) x x
  
  let f x = ll_3 (( / ) x 2)

  $ dune exec ./demoLL.exe << EOF
  > let rec f x = let g x = x + 1 in g (x + 2) 
  > and g x = let f x = x + 2 in f (x + 1)
  > EOF
  
  let ll_0 x = ( + ) x 2
  
  let ll_1 x = ( + ) x 1
  
  let rec f = (fun x -> ll_1 (( + ) x 2))
  and
  g = (fun x -> ll_0 (( + ) x 1))

  $ dune exec ./demoLL.exe << EOF
  > let rec even x = 
  > if x = 0 then true else odd (x - 1) 
  > and 
  > odd x = if x = 0 then false else even (x - 1)
  > EOF
  
  let rec even = (fun x -> if ( = ) x 0 then true else odd (( - ) x 1))
  and
  odd = (fun x -> if ( = ) x 0 then false else even (( - ) x 1))

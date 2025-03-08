  $ dune exec ./demoLL.exe << EOF
  > let q = let f x = x in f 10
  > EOF
  
  let LL_0 x = x
  
  let q = LL_0 10

  $ dune exec ./demoLL.exe << EOF
  > let q = let f x = x + 1 in let g x = f x in g 10
  > EOF
  
  let LL_0 x = ( + ) x 1
  
  let LL_1 f x = LL_0 x
  
  let q = LL_1 LL_0 10

  $ dune exec ./demoLL.exe << EOF
  > let q = let f x = x + 1 in let g x = f x in let k x = g x in let q x = k x in q 1
  > EOF
  
  let LL_0 x = ( + ) x 1
  
  let LL_1 f x = LL_0 x
  
  let LL_2 f g x = LL_1 LL_0 x
  
  let LL_3 f g k x = LL_2 LL_0 LL_1 x
  
  let q = LL_3 LL_0 LL_1 LL_2 1

  $ dune exec ./demoLL.exe << EOF
  > let q = let x = 1 in let y = 2 in fun z -> x + y + z
  > EOF
  
  let LL_0 x y z = ( + ) (( + ) x y) z
  
  let q = 
  let x = 1 in 
  let y = 2 in LL_0 x y

  $ dune exec ./demoLL.exe << EOF
  > let q = let x = 1 in (fun f y -> (f x) + x + 1) (fun z -> x + z)
  > EOF
  
  let LL_0 x f y = ( + ) (( + ) (f x) x) 1
  
  let LL_1 x z = ( + ) x z
  
  let q = 
  let x = 1 in LL_0 x (LL_1 x)

  $ dune exec ./demoLL.exe << EOF
  > let f x = print_int x
  > EOF
  
  let f x = print_int x

  $ dune exec ./demoLL.exe << EOF
  > let rec fact x k = 
  > match x with 
  > | 1 -> k 1
  > | x -> fact (x - 1) (fun n -> k (x * n))
  > EOF
  
  let LL_0 k x n = k (( * ) x n)
  
  let rec fact x k = match x with 
  | 1 -> k 1
  | x -> fact (( - ) x 1) (LL_0 k x)

  $ dune exec ./demoLL.exe << EOF
  > let fs = (let y = 1 in fun x -> x + y), (let y = true in fun x -> x && y)
  > EOF
  
  let LL_0 y x = ( + ) x y
  
  let LL_1 y x = ( && ) x y
  
  let fs = 
  let y = 1 in LL_0 y, 
  let y = true in LL_1 y

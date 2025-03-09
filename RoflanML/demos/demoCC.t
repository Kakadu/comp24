  $ dune exec ./demoCC.exe << EOF
  > let q = let f x = x in f 10
  > EOF
  
  let q = 
  let f x = x in f 10

  $ dune exec ./demoCC.exe << EOF
  > let q = let f x = x + 1 in let g x = f x in g 10
  > EOF
  
  let q = 
  let f x = ( + ) x 1 in 
  let g f x = f x in g f 10

  $ dune exec ./demoCC.exe << EOF
  > let q = let f x = x + 1 in let g x = f x in let k x = g x in let q x = k x in q 1
  > EOF
  
  let q = 
  let f x = ( + ) x 1 in 
  let g f x = f x in 
  let k f g x = g f x in 
  let q f g k x = k f g x in q f g k 1

  $ dune exec ./demoCC.exe << EOF
  > let q = let x = 1 in let y = 2 in fun z -> x + y + z
  > EOF
  
  let q = 
  let x = 1 in 
  let y = 2 in (fun x y z -> ( + ) (( + ) x y) z) x y

  $ dune exec ./demoCC.exe << EOF
  > let q = let x = 1 in (fun f y -> (f x) + x + 1) (fun z -> x + z)
  > EOF
  
  let q = 
  let x = 1 in (fun x f y -> ( + ) (( + ) (f x) x) 1) x ((fun x z -> ( + ) x z) x)

  $ dune exec ./demoCC.exe << EOF
  > let f x = print_int x
  > EOF
  
  let f x = print_int x

  $ dune exec ./demoCC.exe << EOF
  > let fs = (let y = 1 in fun x -> x + y), (let y = true in fun x -> x && y)
  > EOF
  
  let fs = 
  let y = 1 in (fun y x -> ( + ) x y) y, 
  let y = true in (fun y x -> ( && ) x y) y

  $ dune exec ./demoCC.exe << EOF
  > let rec fact x k = 
  > if x = 2 then k 1 else fact (x - 1) (fun n -> k ( x * n)) 
  > EOF
  
  let rec fact x k = if ( = ) x 2 then k 1 else fact (( - ) x 1) ((fun k x n -> k (( * ) x n)) k x)

  $ dune exec ./demoCC.exe << EOF
  > let rec even x = 
  > if x = 0 then true else odd (x - 1) 
  > and 
  > odd x = if x = 0 then false else even (x - 1)
  > EOF
  
  let rec even = (fun x -> if ( = ) x 0 then true else odd (( - ) x 1))
  and
  odd = (fun x -> if ( = ) x 0 then false else even (( - ) x 1))

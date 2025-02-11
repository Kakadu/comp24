DO NOTHING
  $ ./run_remove_patterns.exe << EOF
  > let a = 1
  > 
  > let sum a b = a + b
  > 
  > let rec fac n = if n<=1 then 1 else n * fac (n-1)
  > 
  > let f x = 
  >   let a b = b + x in 
  >   let temp = a 1 in 
  >   temp - x
  > 
  > 
  > let rec is_even n =
  > if n = 0 then true
  > else is_odd (n - 1)
  > 
  > and is_odd n =
  > if n = 0 then false
  > else is_even (n - 1)
  let a = 1
  
  let sum = (fun a b -> ((+ a) b))
  
  let rec fac = (fun n -> 
  if ((<= n) 1)
  then 1
  else ((* n) (fac ((- n) 1))))
  
  let f = (fun x -> let a = (fun b -> ((+ b) x)) in
  let temp = (a 1) in
  ((- temp) x))
  
  let rec is_even = (fun n -> 
  if ((= n) 0)
  then true
  else (is_odd ((- n) 1)))
  and is_odd = (fun n -> 
  if ((= n) 0)
  then false
  else (is_even ((- n) 1)))

MODIFY
  $ ./run_remove_patterns.exe << EOF
  > let (a, b) = (1,2)
  > let (c :: d) = [1; 2; 3]
  > let f (x, y) = x + y
  let a = match (1, 2) with 
  | (a, b) -> a 
  
  let b = match (1, 2) with 
  | (a, b) -> b 
  
  let c = match (1::(2::(3::[]))) with 
  | (c::d) -> c 
  
  let d = match (1::(2::(3::[]))) with 
  | (c::d) -> d 
  
  let f = (fun #0 -> match (#0) with 
  | ((x, y)) -> ((+ x) y) )

  $ ./run_remove_patterns.exe << EOF
  > let f x = 
  >  let g (a, b) (c :: d) e = x + 1
  >  in g 
  let f = (fun x -> let g = (fun #0 #1 e -> match (#0, #1) with 
  | ((a, b), (c::d)) -> ((+ x) 1) ) in
  g)

  $ ./run_remove_patterns.exe << EOF
  > let a =
  >   let f (x, y) = x + y in 
  >   let rec g a = h a and h a = g a in 
  >   1
  let a = let f = (fun #0 -> match (#0) with 
  | ((x, y)) -> ((+ x) y) ) in
  let rec g = (fun a -> (h a)) and h = (fun a -> (g a)) in
  1

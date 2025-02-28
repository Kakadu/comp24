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
  
  let rec fac = (fun n -> if ((<= n) 1)
  then 1
  else ((* n) (fac ((- n) 1))))
  
  let f = (fun x -> let a = (fun b -> ((+ b) x)) in
  let temp = (a 1) in
  ((- temp) x))
  
  let rec is_even = (fun n -> if ((= n) 0)
  then true
  else (is_odd ((- n) 1)))
  and is_odd = (fun n -> if ((= n) 0)
  then false
  else (is_even ((- n) 1)))

MODIFY
  $ ./run_remove_patterns.exe << EOF
  > let abc = 
  >   let (a, (b, c)) = (1, (2, 3)) in 
  >   let (d::e) = [1; 2; 3] in
  >   let f (x, y) = x + y in 
  >   0
  let abc = let #2 = (1, (2, 3)) in
  let a = ((#unpack_tuple #2) 0) in
  let b = ((#unpack_tuple ((#unpack_tuple #2) 1)) 0) in
  let c = ((#unpack_tuple ((#unpack_tuple #2) 1)) 1) in
  let #1 = (1::(2::(3::[]))) in
  if ((> (#list_length #1)) 0)
  then let d = (#list_hd #1) in
  let e = (#list_tl #1) in
  let f = (fun #0 -> let x = ((#unpack_tuple #0) 0) in
  let y = ((#unpack_tuple #0) 1) in
  ((+ x) y)) in
  0
  else #match_failure

  $ ./run_remove_patterns.exe << EOF
  > let f x = 
  >  let g (a, b) (c :: d) e = x + 1
  >  in g 
  let f = (fun x -> let g = (fun #0 #1 e -> let #2 = (#0, #1) in
  if ((> (#list_length ((#unpack_tuple #2) 1))) 0)
  then let a = ((#unpack_tuple ((#unpack_tuple #2) 0)) 0) in
  let b = ((#unpack_tuple ((#unpack_tuple #2) 0)) 1) in
  let c = (#list_hd ((#unpack_tuple #2) 1)) in
  let d = (#list_tl ((#unpack_tuple #2) 1)) in
  ((+ x) 1)
  else #match_failure) in
  g)

  $ ./run_remove_patterns.exe << EOF
  > let a =
  >   let f (x, y) = x + y in 
  >   let rec g a = h a and h a = g a in 
  >   1
  let a = let f = (fun #0 -> let x = ((#unpack_tuple #0) 0) in
  let y = ((#unpack_tuple #0) 1) in
  ((+ x) y)) in
  let rec g = (fun a -> (h a)) and h = (fun a -> (g a)) in
  1

  $ ./run_remove_patterns.exe << EOF
  > let x = 1
  > let t = match x with 
  >   | 2 -> 3
  >   | 3 -> 4
  >   | _ -> 1
  >   | 5 -> 6
  >   | 7 -> 8
  >   | 10 -> 9
  let x = 1
  
  let t = if ((= x) 2)
  then 3
  else if ((= x) 3)
  then 4
  else 1

  $ ./run_remove_patterns.exe << EOF
  > let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)
  let rec append = (fun xs ys -> if ((= xs) [])
  then ys
  else if ((> (#list_length xs)) 0)
  then let x = (#list_hd xs) in
  let xs = (#list_tl xs) in
  (x::((append xs) ys))
  else #match_failure)

  $ ./run_remove_patterns.exe << EOF
  > let (1, 2) = (2, 3)
  let #0 = (2, 3)
  
  let () = if ((&& ((= ((#unpack_tuple #0) 0)) 1)) ((= ((#unpack_tuple #0) 1)) 2))
  then ()
  else #match_failure

  $ ./run_remove_patterns.exe << EOF
  > let rec map f xs =
  > match xs with
  > | [] -> []
  > | a::[] -> [f a]
  > | a::b::[] -> [f a; f b]
  > | a::b::c::[] -> [f a; f b; f c]
  > | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl
  let rec map = (fun f xs -> if ((= xs) [])
  then []
  else if ((&& ((> (#list_length xs)) 0)) ((= (#list_tl xs)) []))
  then let a = (#list_hd xs) in
  ((f a)::[])
  else if ((&& ((> (#list_length xs)) 1)) ((= (#list_tl (#list_tl xs))) []))
  then let a = (#list_hd xs) in
  let b = (#list_hd (#list_tl xs)) in
  ((f a)::((f b)::[]))
  else if ((&& ((> (#list_length xs)) 2)) ((= (#list_tl (#list_tl (#list_tl xs)))) []))
  then let a = (#list_hd xs) in
  let b = (#list_hd (#list_tl xs)) in
  let c = (#list_hd (#list_tl (#list_tl xs))) in
  ((f a)::((f b)::((f c)::[])))
  else if ((> (#list_length xs)) 3)
  then let a = (#list_hd xs) in
  let b = (#list_hd (#list_tl xs)) in
  let c = (#list_hd (#list_tl (#list_tl xs))) in
  let d = (#list_hd (#list_tl (#list_tl (#list_tl xs)))) in
  let tl = (#list_tl (#list_tl (#list_tl (#list_tl xs)))) in
  ((f a)::((f b)::((f c)::((f d)::((map f) tl)))))
  else #match_failure)


  $ ./run_remove_patterns.exe << EOF
  > let a = match 1 + 2 with 
  > | x -> x
  let a = let #0 = ((+ 1) 2) in
  let x = #0 in
  x

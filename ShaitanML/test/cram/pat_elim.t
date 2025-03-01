  $ pat_elim << EOF
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
  
  let sum = (fun a b -> ((( + ) a) b))
  
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let f = (fun x -> let a = (fun b -> ((( + ) b) x)) in
  let temp = (a 1) in
  ((( - ) temp) x))
  
  let rec is_even = (fun n -> if ((( = ) n) 0)
  then true
  else (is_odd ((( - ) n) 1)))
  and is_odd = (fun n -> if ((( = ) n) 0)
  then false
  else (is_even ((( - ) n) 1)))

  $ pat_elim << EOF
  > let abc =
  >   let (a, (b, c)) = (1, (2, 3)) in
  >   let (d::e) = [1; 2; 3] in
  >   let f (x, y) = x + y in
  >   0
  let abc = let a2 = (1, (2, 3)) in
  let a = ((tuple_element a2) 0) in
  let b = ((tuple_element ((tuple_element a2) 1)) 0) in
  let c = ((tuple_element ((tuple_element a2) 1)) 1) in
  let a1 = (1::(2::(3::[]))) in
  if ((> (list_len a1)) 0)
  then let d = (list_head a1) in
  let e = (list_tail a1) in
  let f = (fun a0 -> let x = ((tuple_element a0) 0) in
  let y = ((tuple_element a0) 1) in
  ((( + ) x) y)) in
  0
  else fail_match

  $ pat_elim << EOF
  > let rec map f xs =
  > match xs with
  > | [] -> []
  > | a::[] -> [f a]
  > | a::b::[] -> [f a; f b]
  > | a::b::c::[] -> [f a; f b; f c]
  > | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl
  Infer error: Occurs check failed

  $ pat_elim << EOF
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

  $ pat_elim << EOF
  > let a = match 1 + 2 with
  > | x -> x
  let a = let a0 = ((( + ) 1) 2) in
  let x = a0 in
  x

  $ pat_elim << EOF
  > let a =
  >   let f (x, y) = x + y in
  >   let rec g a = h a and h a = g a in
  >   1
  Infer error: Unexpected structure item, expected let or let rec

  $ alpha << EOF
  > let a = 1
  > EOF
  let a = 1

  $ alpha << EOF
  > let a, b = 5, 6
  let a0 = (5, 6)
  
  let a = ((tuple_element a0) 0)
  
  let b = ((tuple_element a0) 1)

  $ alpha << EOF
  > let rec fac x = if x < 1 then 1 else x * fac (x - 1)
  > EOF
  let rec fac = (fun x -> if ((( < ) x) 1)
  then 1
  else ((( * ) x) (fac ((( - ) x) 1))))

  $ alpha << EOF
  > let rec fac x =
  >  let rec helper acc x =
  >    if x < 1 then acc
  >    else helper (x * acc) (x - 1)
  >  in helper 1 x
  let rec fac = (fun x -> let rec helper = (fun acc a0 -> if ((( < ) a0) 1)
  then acc
  else ((helper ((( * ) a0) acc)) ((( - ) a0) 1))) in
  ((helper 1) x))

  $ alpha << EOF
  > let f x =
  >  (fun x -> x + 1) x, (fun x -> x + 2) x
  let f = (fun x -> (((fun a0 -> ((( + ) a0) 1)) x), ((fun a1 -> ((( + ) a1) 2)) x)))

  $ alpha << EOF
  > let f (a, b) = a + 1, b + 1
  let f = (fun a0 -> let a = ((tuple_element a0) 0) in
  let b = ((tuple_element a0) 1) in
  (((( + ) a) 1), ((( + ) b) 1)))

  $ alpha << EOF
  > let f (a, b) (x::y) ((z,w)::zws) =
  >  a + b + x + z + w
  let f = (fun a0 a1 a2 -> let a3 = (a0, a1, a2) in
  if ((&& ((> (list_len ((tuple_element a3) 1))) 0)) ((> (list_len ((tuple_element a3) 2))) 0))
  then let a = ((tuple_element ((tuple_element a3) 0)) 0) in
  let b = ((tuple_element ((tuple_element a3) 0)) 1) in
  let w = (list_tail ((tuple_element a3) 1)) in
  let x = (list_head ((tuple_element a3) 1)) in
  let y = (list_tail ((tuple_element a3) 1)) in
  let z = (list_tail ((tuple_element a3) 1)) in
  let zws = (list_tail ((tuple_element a3) 1)) in
  ((( + ) ((( + ) ((( + ) ((( + ) a) b)) x)) z)) w)
  else fail_match)

  $ alpha << EOF
  > let rec map f list =
  >   match list with
  >   | [] -> []
  >   | x::xs -> x::(map f xs)
  let rec map = (fun f list -> if ((= list) [])
  then []
  else if ((> (list_len list)) 0)
  then let x = (list_head list) in
  let xs = (list_tail list) in
  (x::((map f) xs))
  else fail_match)

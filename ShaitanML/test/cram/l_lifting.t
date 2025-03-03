  $ l_lifting << EOF
  > let f = fun x -> x;;
  > EOF
  let f = (fun x -> x)

  $ l_lifting << EOF
  > let f x =
  >  let plus_one x = x + 1 in
  >  plus_one x
  > EOF
  let a1 = (fun a0 -> ((( + ) a0) 1))
  
  let f = (fun x -> (a1 x))

  $ l_lifting << EOF
  > let f x =
  >  let a = 1 in
  >  let sum a b = a + b in
  >  sum x a
  > EOF
  let a1 = (fun a0 b -> ((( + ) a0) b))
  
  let f = (fun x -> let a = 1 in
  ((a1 x) a))


  $ l_lifting << EOF
  > let (a, b) = (1, 2)
  > EOF
  let a0 = (1, 2)
  
  let a = ((tuple_element a0) 0)
  
  let b = ((tuple_element a0) 1)


  $ l_lifting << EOF
  > let test a =
  >     let (f1, f2, f3), s, t = a in
  >     f1 + f2 + f3 + s + t
  > EOF
  let test = (fun a -> let f1 = ((tuple_element ((tuple_element a) 0)) 0) in
  let f2 = ((tuple_element ((tuple_element a) 0)) 1) in
  let f3 = ((tuple_element ((tuple_element a) 0)) 2) in
  let s = ((tuple_element a) 1) in
  let t = ((tuple_element a) 2) in
  ((( + ) ((( + ) ((( + ) ((( + ) f1) f2)) f3)) s)) t))

  $ l_lifting << EOF
  > let rec fac_cps n k =
  > if n=1 then k 1 else
  > fac_cps (n-1) (fun p -> k (p*n))
  > EOF
  let a0 = (fun k n p -> (k ((( * ) p) n)))
  
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) ((a0 k) n)))

  $ l_lifting << EOF
  > let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)
  > EOF
  let a2 = (fun a0 -> ((( + ) a0) 2))
  
  let a3 = (fun a1 -> ((( * ) a1) 10))
  
  let foo = (fun b -> if b
  then a2
  else a3)

  $ closure << EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n-1) ((fun k n m -> k (m * n)) k n)
  >   in
  >   fack n (fun x -> x)
  let fac = (fun n -> let rec fack = (fun a0 k -> if ((( <= ) a0) 1)
  then (k 1)
  else ((fack ((( - ) a0) 1)) (((fun a1 a2 m -> (a1 ((( * ) m) a2))) k) a0))) in
  ((fack n) (fun x -> x)))

  $ closure << EOF
  > let sum x =
  >   let new_sum y = x + y in
  >   new_sum 5
  > EOF
  let sum = (fun x -> let new_sum = (fun x y -> ((( + ) x) y)) in
  ((new_sum x) 5))

  $ closure << EOF
  >   let x y =
  >   let z a = a (y + 1) in
  >   z (fun x -> x)
  let x = (fun y -> let z = (fun y a -> (a ((( + ) y) 1))) in
  ((z y) (fun a0 -> a0)))

  $ closure << EOF
  >     let fibo n =
  >       let rec fibo_cps n acc =
  >       if n < 3
  >       then acc 1
  >       else fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
  >       in
  >       fibo_cps n (fun x -> x)
  let fibo = (fun n -> let rec fibo_cps = (fun a0 acc -> if ((( < ) a0) 3)
  then (acc 1)
  else ((fibo_cps ((( - ) a0) 1)) ((((fun a0 acc fibo_cps x -> ((fibo_cps ((( - ) a0) 2)) (((fun acc x y -> (acc ((( + ) x) y))) acc) x))) a0) acc) fibo_cps))) in
  ((fibo_cps n) (fun x -> x)))

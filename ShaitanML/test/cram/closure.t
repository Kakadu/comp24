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


  $ closure < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ closure < manytests/typed/002fac.ml
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) (((fun k n p -> (k ((( * ) p) n))) k) n)))
  
  let main = let () = (print_int ((fac_cps 4) (fun a0 -> a0))) in
  0

  $ closure < manytests/typed/003fib.ml
  let rec fib_acc = (fun a b n -> if ((( = ) n) 1)
  then b
  else let n1 = ((( - ) n) 1) in
  let ab = ((( + ) a) b) in
  (((fib_acc b) ab) n1))
  
  let rec fib = (fun n -> if ((( < ) n) 2)
  then n
  else ((( + ) (fib ((( - ) n) 1))) (fib ((( - ) n) 2))))
  
  let main = let () = (print_int (((fib_acc 0) 1) 4)) in
  let () = (print_int (fib 4)) in
  0

  $ closure < manytests/typed/004manyargs.ml
  let wrap = (fun f -> if ((( = ) 1) 1)
  then f
  else f)
  
  let test3 = (fun a0 a1 a2 a b c -> let a0 = (print_int a0) in
  let a1 = (print_int a1) in
  let a2 = (print_int a2) in
  0)
  
  let test10 = (fun a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ closure < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ closure < manytests/typed/006partial.ml
  let foo = (fun b -> if b
  then (fun a0 -> ((( + ) a0) 2))
  else (fun a1 -> ((( * ) a1) 10)))
  
  let a2 = (fun a2 x -> ((a2 true) ((a2 false) ((a2 true) ((a2 false) x)))))
  
  let main = let () = (print_int (a2 11)) in
  0

  $ closure < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let a0 = (a0 1) in
  let a1 = (a1 2) in
  let a2 = (a2 3) in
  let () = (print_int a2) in
  0

  $ closure < manytests/typed/006partial3.ml
  let foo = (fun a -> let () = (print_int a) in
  (fun b -> let () = (print_int b) in
  (fun c -> (print_int c))))
  
  let main = let () = (((foo 4) 8) 9) in
  0

  $ closure < manytests/typed/007order.ml
  let _start = (fun () () a () b _c () d __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = print_int

  $ closure < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let main = let () = (print_int (((addi (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))) (fun _start -> ((( = ) ((( / ) _start) 2)) 0))) 4)) in
  0

  $ closure < manytests/typed/009let_poly.ml
  let temp = let f = (fun x -> x) in
  ((f 1), (f true))


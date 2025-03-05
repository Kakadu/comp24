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

  $ l_lifting < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ l_lifting < manytests/typed/002fac.ml
  let a1 = (fun k n p -> (k ((( * ) p) n)))
  
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) ((a1 k) n)))
  
  let a2 = (fun a0 -> a0)
  
  let main = let () = (print_int ((fac_cps 4) a2)) in
  0

  $ l_lifting < manytests/typed/003fib.ml
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

  $ l_lifting < manytests/typed/004manyargs.ml
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

  $ l_lifting < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ l_lifting < manytests/typed/006partial.ml
  let a3 = (fun a0 -> ((( + ) a0) 2))
  
  let a4 = (fun a1 -> ((( * ) a1) 10))
  
  let foo = (fun b -> if b
  then a3
  else a4)
  
  let a2 = (fun a2 x -> ((a2 true) ((a2 false) ((a2 true) ((a2 false) x)))))
  
  let main = let () = (print_int (a2 11)) in
  0

  $ l_lifting < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let a0 = (a0 1) in
  let a1 = (a1 2) in
  let a2 = (a2 3) in
  let () = (print_int a2) in
  0

  $ l_lifting < manytests/typed/006partial3.ml
  let a1 = (fun c -> (print_int c))
  
  let a0 = (fun b -> let () = (print_int b) in
  a1)
  
  let foo = (fun a -> let () = (print_int a) in
  a0)
  
  let main = let () = (((foo 4) 8) 9) in
  0

  $ l_lifting < manytests/typed/007order.ml
  let _start = (fun () () a () b _c () d __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = print_int

  $ l_lifting < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let a0 = (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))
  
  let a1 = (fun _start -> ((( = ) ((( / ) _start) 2)) 0))
  
  let main = let () = (print_int (((addi a0) a1) 4)) in
  0

  $ l_lifting < manytests/typed/009let_poly.ml
  let a0 = (fun x -> x)
  
  let temp = ((a0 1), (a0 true))

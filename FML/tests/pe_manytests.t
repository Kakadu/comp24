  $ ./pe_runner.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let a0 = (print_int (fac 4)) in
  0

  $ ./pe_runner.exe < manytests/typed/002fac.ml
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) (fun p -> (k ((( * ) p) n)))))
  
  let main = let a0 = (print_int ((fac_cps 4) (fun print_int -> print_int))) in
  0

  $ ./pe_runner.exe < manytests/typed/003fib.ml
  let rec fib_acc = (fun a b n -> if ((( = ) n) 1)
  then b
  else let n1 = ((( - ) n) 1) in
  let ab = ((( + ) a) b) in
  (((fib_acc b) ab) n1))
  
  let rec fib = (fun n -> if ((( < ) n) 2)
  then n
  else ((( + ) (fib ((( - ) n) 1))) (fib ((( - ) n) 2))))
  
  let main = let a1 = (print_int (((fib_acc 0) 1) 4)) in
  let a0 = (print_int (fib 4)) in
  0

  $ ./pe_runner.exe < manytests/typed/004manyargs.ml
  let wrap = (fun f -> if ((( = ) 1) 1)
  then f
  else f)
  
  let test3 = (fun a b c -> let a = (print_int a) in
  let b = (print_int b) in
  let c = (print_int c) in
  0)
  
  let test10 = (fun a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let a0 = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ ./pe_runner.exe < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let a0 = (print_int ((fix fac) 6)) in
  0

  $ ./pe_runner.exe < manytests/typed/006partial.ml
  let foo = (fun b -> if b
  then (fun foo -> ((( + ) foo) 2))
  else (fun foo -> ((( * ) foo) 10)))
  
  let foo = (fun x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))))
  
  let main = let a0 = (print_int (foo 11)) in
  0

  $ ./pe_runner.exe < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let a2 = (print_int a) in
  let a1 = (print_int b) in
  let a0 = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let foo = (foo 1) in
  let foo = (foo 2) in
  let foo = (foo 3) in
  let a3 = (print_int foo) in
  0
  $ ./pe_runner.exe < manytests/typed/006partial3.ml
  let foo = (fun a -> let a1 = (print_int a) in
  (fun b -> let a0 = (print_int b) in
  (fun c -> (print_int c))))
  
  let main = let a2 = (((foo 4) 8) 9) in
  0
  $ ./pe_runner.exe < manytests/typed/007order.ml
  let _start = (fun a0 a1 a a2 b _c a3 d __ -> let a6 = (a0, a1, a2, a3) in
  let a5 = (print_int ((( + ) a) b)) in
  let a4 = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (( ~- ) 1))) 10000) (( ~- ) 555555)))
  $ ./pe_runner.exe < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let main = let a0 = (print_int (((addi (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))) (fun _start -> ((( = ) ((( / ) _start) 2)) 0))) 4)) in
  0

  $ ./pe_runner.exe < manytests/typed/009let_poly.ml
  let temp = let f = (fun x -> x) in
  ((f 1), (f true))

  $ ./pe_runner.exe < manytests/typed/015tuples.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let map = (fun f p -> let a = ((tuple_element p) 0) in
  let b = ((tuple_element p) 1) in
  ((f a), (f b)))
  
  let fixpoly = (fun l -> ((fix (fun self l -> ((map (fun li x -> ((li (self l)) x))) l))) l))
  
  let feven = (fun p n -> let e = ((tuple_element p) 0) in
  let o = ((tuple_element p) 1) in
  if ((( == ) n) 0)
  then 1
  else (o ((( - ) n) 1)))
  
  let fodd = (fun p n -> let e = ((tuple_element p) 0) in
  let o = ((tuple_element p) 1) in
  if ((( == ) n) 0)
  then 0
  else (e ((( - ) n) 1)))
  
  let tie = (fixpoly (feven, fodd))
  
  let rec meven = (fun n -> if ((( = ) n) 0)
  then 1
  else (modd ((( - ) n) 1)))
  and modd = (fun n -> if ((( = ) n) 0)
  then 1
  else (meven ((( - ) n) 1)))
  
  let main = let a3 = (print_int (modd 1)) in
  let a2 = (print_int (meven 2)) in
  let even = ((tuple_element tie) 0) in
  let odd = ((tuple_element tie) 1) in
  let a1 = (print_int (odd 3)) in
  let a0 = (print_int (even 4)) in
  0

  $ ./pe_runner.exe < manytests/typed/016lists.ml
  let rec length = (fun xs -> 0)
  
  let length_tail = let rec helper = (fun acc xs -> acc) in
  (helper 0)
  
  let rec map = (fun f xs -> [])
  
  let rec append = (fun xs ys -> ys)
  
  let concat = let rec helper = (fun xs -> []) in
  helper
  
  let rec iter = (fun f xs -> ())
  
  let rec cartesian = (fun xs ys -> [])
  
  let main = let a1 = ((iter print_int) (1::(2::(3::[])))) in
  let a0 = (print_int (length ((cartesian (1::(2::[]))) (1::(2::(3::(4::[]))))))) in
  0
 
  $ ./pe_runner.exe < manytests/do_not_type/001.ml
  Infer error:
  $ ./pe_runner.exe < manytests/do_not_type/002if.ml
  Infer error:
  $ ./pe_runner.exe < manytests/do_not_type/003occurs.ml
  Infer error:

  $ ./pe_runner.exe < manytests/do_not_type/004let_poly.ml
  Infer error:

  $ ./pe_runner.exe < manytests/do_not_type/015tuples.ml
  Infer error:

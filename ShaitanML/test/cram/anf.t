  $ anf << EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n-1) ((fun k n m -> k (m * n)) k n)
  >   in
  >   fack n (fun x -> x)
  let a4 a1 a2 m = let a6 = ((( * ) m) a2) in
  (a1 a6)
  
  let rec a3 a0 k = let a7 = ((( <= ) a0) 1) in
  if a7
  then (k 1)
  else let a9 = ((a4 k) a0) in
  let a8 = ((( - ) a0) 1) in
  ((a3 a8) a9)
  
  let a5 x = x
  
  let fac n = ((a3 n) a5)

  $ anf << EOF
  > let sum x =
  >   let new_sum y = x + y in
  >   new_sum 5
  > EOF
  let a0 x y = ((( + ) x) y)
  
  let sum x = ((a0 x) 5)

  $ anf << EOF
  >   let x y =
  >   let z a = a (y + 1) in
  >   z (fun x -> x)
  let a0 y a = let a2 = ((( + ) y) 1) in
  (a a2)
  
  let a1 x = x
  
  let x y = ((a0 y) a1)

  $ anf << EOF
  >     let fibo n =
  >       let rec fibo_cps n acc =
  >       if n < 3
  >       then acc 1
  >       else fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
  >       in
  >       fibo_cps n (fun x -> x)
  let a3 acc x y = let a5 = ((( + ) x) y) in
  (acc a5)
  
  let a2 a0 acc fibo_cps x = let a7 = ((a3 acc) x) in
  let a6 = ((( - ) a0) 2) in
  ((fibo_cps a6) a7)
  
  let rec a1 a0 acc = let a8 = ((( < ) a0) 3) in
  if a8
  then (acc 1)
  else let a10 = (((a2 a0) acc) a1) in
  let a9 = ((( - ) a0) 1) in
  ((a1 a9) a10)
  
  let a4 x = x
  
  let fibo n = ((a1 n) a4)

  $ anf << EOF
  > let test_cons =
  >   let (h::t) = [1; 2; 3] in
  >   h + 1
  let test_cons = let a2 = (3::[]) in
  let a1 = (2::a2) in
  let a0 = (1::a1) in
  let a4 = (list_len a0) in
  let a3 = ((( > ) a4) 0) in
  if a3
  then let h = (list_head a0) in
  let t = (list_tail a0) in
  ((( + ) h) 1)
  else fail_match


  $ anf << EOF
  > let test_tuple =
  >   let (x, (y, z)) = (5, (10, 15)) in
  >   x + y + z
  let test_tuple = let a1 = (10, 15) in
  let a0 = (5, a1) in
  let x = ((tuple_element a0) 0) in
  let a2 = ((tuple_element a0) 1) in
  let y = ((tuple_element a2) 0) in
  let a3 = ((tuple_element a0) 1) in
  let z = ((tuple_element a3) 1) in
  let a4 = ((( + ) x) y) in
  ((( + ) a4) z)


  $ anf << EOF
  > let test_list =
  >   let lst = [1; 2; 3; 4; 5] in
  >   let rec sum lst = match lst with
  >   | [] -> 0
  >   | h::t -> h + (sum t)
  > in
  > sum lst
  let rec a1 a0 = let a2 = ((( = ) a0) []) in
  if a2
  then 0
  else let a4 = (list_len a0) in
  let a3 = ((( > ) a4) 0) in
  if a3
  then let h = (list_head a0) in
  let t = (list_tail a0) in
  let a5 = (a1 t) in
  ((( + ) h) a5)
  else fail_match
  
  let test_list = let a9 = (5::[]) in
  let a8 = (4::a9) in
  let a7 = (3::a8) in
  let a6 = (2::a7) in
  let lst = (1::a6) in
  (a1 lst)

  $ anf < manytests/typed/001fac.ml
  let rec fac n = let a1 = ((( <= ) n) 1) in
  if a1
  then 1
  else let a3 = ((( - ) n) 1) in
  let a2 = (fac a3) in
  ((( * ) n) a2)
  
  let main = let a4 = (fac 4) in
  let a0 = (print_int a4) in
  0

  $ anf < manytests/typed/002fac.ml
  let a2 k n p = let a4 = ((( * ) p) n) in
  (k a4)
  
  let rec fac_cps n k = let a5 = ((( = ) n) 1) in
  if a5
  then (k 1)
  else let a7 = ((a2 k) n) in
  let a6 = ((( - ) n) 1) in
  ((fac_cps a6) a7)
  
  let a3 a1 = a1
  
  let main = let a8 = ((fac_cps 4) a3) in
  let a0 = (print_int a8) in
  0

  $ anf < manytests/typed/003fib.ml
  let rec fib_acc a b n = let a2 = ((( = ) n) 1) in
  if a2
  then b
  else let n1 = ((( - ) n) 1) in
  let ab = ((( + ) a) b) in
  (((fib_acc b) ab) n1)
  
  let rec fib n = let a3 = ((( < ) n) 2) in
  if a3
  then n
  else let a7 = ((( - ) n) 2) in
  let a6 = (fib a7) in
  let a5 = ((( - ) n) 1) in
  let a4 = (fib a5) in
  ((( + ) a4) a6)
  
  let main = let a8 = (((fib_acc 0) 1) 4) in
  let a1 = (print_int a8) in
  let a9 = (fib 4) in
  let a0 = (print_int a9) in
  0

  $ anf < manytests/typed/004manyargs.ml
  let wrap f = let a4 = ((( = ) 1) 1) in
  if a4
  then f
  else f
  
  let test3 a b c = let a1 = (print_int a) in
  let a2 = (print_int b) in
  let a3 = (print_int c) in
  0
  
  let test10 a b c d e f g h i j = let a12 = ((( + ) a) b) in
  let a11 = ((( + ) a12) c) in
  let a10 = ((( + ) a11) d) in
  let a9 = ((( + ) a10) e) in
  let a8 = ((( + ) a9) f) in
  let a7 = ((( + ) a8) g) in
  let a6 = ((( + ) a7) h) in
  let a5 = ((( + ) a6) i) in
  ((( + ) a5) j)
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let a0 = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ anf < manytests/typed/005fix.ml
  let rec fix f x = let a1 = (fix f) in
  ((f a1) x)
  
  let fac self n = let a2 = ((( <= ) n) 1) in
  if a2
  then 1
  else let a4 = ((( - ) n) 1) in
  let a3 = (self a4) in
  ((( * ) n) a3)
  
  let main = let a5 = ((fix fac) 6) in
  let a0 = (print_int a5) in
  0

  $ anf < manytests/typed/006partial.ml
  let a2 foo = ((( + ) foo) 2)
  
  let a3 foo = ((( * ) foo) 10)
  
  let foo b = if b
  then a2
  else a3
  
  let a1 x = let a6 = ((foo false) x) in
  let a5 = ((foo true) a6) in
  let a4 = ((foo false) a5) in
  ((foo true) a4)
  
  let main = let a7 = (a1 11) in
  let a0 = (print_int a7) in
  0

  $ anf < manytests/typed/006partial2.ml
  let foo a b c = let a2 = (print_int a) in
  let a1 = (print_int b) in
  let a0 = (print_int c) in
  let a7 = ((( * ) b) c) in
  ((( + ) a) a7)
  
  let main = let a4 = (foo 1) in
  let a5 = (a4 2) in
  let a6 = (a5 3) in
  let a3 = (print_int a6) in
  0

  $ anf < manytests/typed/006partial3.ml
  let a4 c = (print_int c)
  
  let a3 b = let a0 = (print_int b) in
  a4
  
  let foo a = let a1 = (print_int a) in
  a3
  
  let main = let a2 = (((foo 4) 8) 9) in
  0

  $ anf < manytests/typed/007order.ml
  let _start a0 a1 a a2 b _c a3 d __ = let a6 = (a0, a1, a2, a3) in
  let a7 = ((( + ) a) b) in
  let a5 = (print_int a7) in
  let a4 = (print_int __) in
  let a9 = ((( * ) a) b) in
  let a8 = ((( / ) a9) _c) in
  ((( + ) a8) d)
  
  let main = print_int

  $ anf < manytests/typed/008ascription.ml
  let addi f g x = let a3 = (g x) in
  ((f x) a3)
  
  let a1 x b = if b
  then ((( + ) x) 1)
  else ((( * ) x) 2)
  
  let a2 _start = let a4 = ((( / ) _start) 2) in
  ((( = ) a4) 0)
  
  let main = let a5 = (((addi a1) a2) 4) in
  let a0 = (print_int a5) in
  0

  $ anf < manytests/typed/009let_poly.ml
  let a0 x = x
  
  let temp = let a1 = (a0 1) in
  let a2 = (a0 true) in
  (a1, a2)

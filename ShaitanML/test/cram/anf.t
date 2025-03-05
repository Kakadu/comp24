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
  let a1 y a = let a3 = ((( + ) y) 1) in
  (a a3)
  
  let a2 a0 = a0
  
  let x y = ((a1 y) a2)

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


  $ anf << EOF
  > let test_multiple_cases x =
  >   match x with
  >   | 0 -> "zero"
  >   | 1 -> "one"
  >   | _ -> "other"
  Fatal error: exception File "lib/anf_ast.ml", line 17, characters 20-25: Pattern matching failed
  Raised at Shaitanml_lib__Anf_ast.const_to_aexp in file "lib/anf_ast.ml", line 17, characters 20-124
  Called from Shaitanml_lib__Anf.to_cexp in file "lib/anf.ml", line 18, characters 43-58
  Called from Shaitanml_lib__Anf.to_exp in file "lib/anf.ml", line 63, characters 21-30
  Called from Shaitanml_lib__Anf.to_cexp in file "lib/anf.ml", line 27, characters 14-23
  Called from Shaitanml_lib__Common.MonadCounter.bind in file "lib/common.ml", line 122, characters 24-27
  Called from Shaitanml_lib__Common.MonadCounter.bind in file "lib/common.ml", line 122, characters 24-27
  Called from Shaitanml_lib__Common.MonadCounter.bind in file "lib/common.ml", line 122, characters 24-27
  Called from Dune__exe__Anf in file "test/cram/anf.ml", line 19, characters 20-50
  [2]

  $ anf < manytests/typed/001fac.ml
  let rec fac n = let a0 = ((( <= ) n) 1) in
  if a0
  then 1
  else let a2 = ((( - ) n) 1) in
  let a1 = (fac a2) in
  ((( * ) n) a1)
  
  let main = let a3 = (fac 4) in
  let () = (print_int a3) in
  0

  $ anf < manytests/typed/002fac.ml
  let a1 k n p = let a3 = ((( * ) p) n) in
  (k a3)
  
  let rec fac_cps n k = let a4 = ((( = ) n) 1) in
  if a4
  then (k 1)
  else let a6 = ((a1 k) n) in
  let a5 = ((( - ) n) 1) in
  ((fac_cps a5) a6)
  
  let a2 a0 = a0
  
  let main = let a7 = ((fac_cps 4) a2) in
  let () = (print_int a7) in
  0

  $ anf < manytests/typed/003fib.ml
  let rec fib_acc a b n = let a0 = ((( = ) n) 1) in
  if a0
  then b
  else let n1 = ((( - ) n) 1) in
  let ab = ((( + ) a) b) in
  (((fib_acc b) ab) n1)
  
  let rec fib n = let a1 = ((( < ) n) 2) in
  if a1
  then n
  else let a5 = ((( - ) n) 2) in
  let a4 = (fib a5) in
  let a3 = ((( - ) n) 1) in
  let a2 = (fib a3) in
  ((( + ) a2) a4)
  
  let main = let a6 = (((fib_acc 0) 1) 4) in
  let () = (print_int a6) in
  let a7 = (fib 4) in
  let () = (print_int a7) in
  0

  $ anf < manytests/typed/004manyargs.ml
  let wrap f = let a3 = ((( = ) 1) 1) in
  if a3
  then f
  else f
  
  let test3 a0 a1 a2 a b c = let a0 = (print_int a0) in
  let a1 = (print_int a1) in
  let a2 = (print_int a2) in
  0
  
  let test10 a b c d e f g h i j = let a11 = ((( + ) a) b) in
  let a10 = ((( + ) a11) c) in
  let a9 = ((( + ) a10) d) in
  let a8 = ((( + ) a9) e) in
  let a7 = ((( + ) a8) f) in
  let a6 = ((( + ) a7) g) in
  let a5 = ((( + ) a6) h) in
  let a4 = ((( + ) a5) i) in
  ((( + ) a4) j)
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ anf < manytests/typed/005fix.ml
  let rec fix f x = let a0 = (fix f) in
  ((f a0) x)
  
  let fac self n = let a1 = ((( <= ) n) 1) in
  if a1
  then 1
  else let a3 = ((( - ) n) 1) in
  let a2 = (self a3) in
  ((( * ) n) a2)
  
  let main = let a4 = ((fix fac) 6) in
  let () = (print_int a4) in
  0

  $ anf < manytests/typed/006partial.ml
  let a3 a0 = ((( + ) a0) 2)
  
  let a4 a1 = ((( * ) a1) 10)
  
  let foo b = if b
  then a3
  else a4
  
  let a2 a2 x = let a7 = ((a2 false) x) in
  let a6 = ((a2 true) a7) in
  let a5 = ((a2 false) a6) in
  ((a2 true) a5)
  
  let main = let a8 = (a2 11) in
  let () = (print_int a8) in
  0

  $ anf < manytests/typed/006partial2.ml
  let foo a b c = let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  let a3 = ((( * ) b) c) in
  ((( + ) a) a3)
  
  let main = let a0 = (a0 1) in
  let a1 = (a1 2) in
  let a2 = (a2 3) in
  let () = (print_int a2) in
  0

  $ anf < manytests/typed/006partial3.ml
  let a1 c = (print_int c)
  
  let a0 b = let () = (print_int b) in
  a1
  
  let foo a = let () = (print_int a) in
  a0
  
  let main = let () = (((foo 4) 8) 9) in
  0

  $ anf < manytests/typed/007order.ml
  let _start () () a () b _c () d __ = let a0 = ((( + ) a) b) in
  let () = (print_int a0) in
  let () = (print_int __) in
  let a2 = ((( * ) a) b) in
  let a1 = ((( / ) a2) _c) in
  ((( + ) a1) d)
  
  let main = print_int

  $ anf < manytests/typed/008ascription.ml
  let addi f g x = let a2 = (g x) in
  ((f x) a2)
  
  let a0 x b = if b
  then ((( + ) x) 1)
  else ((( * ) x) 2)
  
  let a1 _start = let a3 = ((( / ) _start) 2) in
  ((( = ) a3) 0)
  
  let main = let a4 = (((addi a0) a1) 4) in
  let () = (print_int a4) in
  0

  $ anf < manytests/typed/009let_poly.ml
  let a0 x = x
  
  let temp = let a1 = (a0 1) in
  let a2 = (a0 true) in
  (a1, a2)

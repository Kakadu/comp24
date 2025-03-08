  $ ./lambda_lifting_runner.exe << EOF
  > let fac n =
  > let rec fack n k =
  > if n<=1 then k 1
  > else fack (n - 1) ((fun k n m -> k (m * n)) k n)
  > in
  > fack n (fun x -> x)
  > EOF
  let a4 = (fun a1 a2 m -> (a1 ((( * ) m) a2)))
  
  let rec a3 = (fun a0 k -> if ((( <= ) a0) 1)
  then (k 1)
  else ((a3 ((( - ) a0) 1)) ((a4 k) a0)))
  
  let a5 = (fun x -> x)
  
  let fac = (fun n -> a3)
  $ ./lambda_lifting_runner.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ ./lambda_lifting_runner.exe < manytests/typed/002fac.ml
  let a1 = (fun k n p -> (k ((( * ) p) n)))
  
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) ((a1 k) n)))
  
  let a2 = (fun a0 -> a0)
  
  let main = let () = (print_int ((fac_cps 4) a2)) in
  0

  $ ./lambda_lifting_runner.exe < manytests/typed/003fib.ml
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

  $ ./lambda_lifting_runner.exe < manytests/typed/004manyargs.ml
  let wrap = (fun f -> if ((( = ) 1) 1)
  then f
  else f)
  
  let test3 = (fun a b c -> let a0 = (print_int a) in
  let a1 = (print_int b) in
  let a2 = (print_int c) in
  0)
  
  let test10 = (fun a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ ./lambda_lifting_runner.exe < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ ./lambda_lifting_runner.exe < manytests/typed/006partial.ml
  let a3 = (fun a0 -> ((( + ) a0) 2))
  
  let a4 = (fun a1 -> ((( * ) a1) 10))
  
  let foo = (fun b -> if b
  then a3
  else a4)
  
  let a2 = (fun a2 x -> ((a2 true) ((a2 false) ((a2 true) ((a2 false) x)))))
  
  let main = let () = (print_int (a2 11)) in
  0

  $ ./lambda_lifting_runner.exe < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let a0 = (foo 1) in
  let a1 = (a0 2) in
  let a2 = (a1 3) in
  let () = (print_int a2) in
  0
  $ ./lambda_lifting_runner.exe < manytests/typed/006partial3.ml
  let a0 = (fun b -> let () = (print_int b) in
  a1)
  
  let foo = (fun a -> let () = (print_int a) in
  a0)
  
  let main = let () = (((foo 4) 8) 9) in
  0
  $ ./lambda_lifting_runner.exe < manytests/typed/007order.ml
  let _start = (fun a0 a1 a a2 b _c a3 d __ -> let a4 = (a0, a1, a2, a3) in
  let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (( ~- ) 1))) 10000) (( ~- ) 555555)))
  $ ./lambda_lifting_runner.exe < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let a0 = (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))
  
  let a1 = (fun _start -> ((( = ) ((( / ) _start) 2)) 0))
  
  let main = let () = (print_int (((addi a0) a1) 4)) in
  0

  $ ./lambda_lifting_runner.exe < manytests/typed/009let_poly.ml
  let temp = let f = (fun x -> x) in
  ((a0 1), (a0 true))

  $ ./lambda_lifting_runner.exe < manytests/typed/011mapcps.ml
  let a1 = (fun f h k a0 -> (k ((f h)::a0)))
  
  let rec map = (fun f xs k -> if (is_empty xs)
  then (k [])
  else if (not (is_empty xs))
  then let h = (list_head xs) in
  let tl = (list_tail xs) in
  (((map f) tl) (((a1 f) h) k))
  else fail_match)
  
  let rec iter = (fun f xs -> if (is_empty xs)
  then ()
  else if (not (is_empty xs))
  then let h = (list_head xs) in
  let tl = (list_tail xs) in
  let w = (f h) in
  ((iter f) tl)
  else fail_match)
  
  let a2 = (fun x -> ((( + ) x) 1))
  
  let a3 = (fun x -> x)
  
  let main = ((iter print_int) (((map a2) (1::(2::(3::[])))) a3))
  $ ./lambda_lifting_runner.exe < manytests/typed/012fibcps.ml
  let a0 = (fun fib k n a -> ((fib ((( - ) n) 2)) ((a1 a) k)))
  
  let rec fib = (fun n k -> if ((( < ) n) 2)
  then (k n)
  else ((fib ((( - ) n) 1)) (((a0 fib) k) n)))
  
  let a2 = (fun x -> x)
  
  let main = (print_int ((fib 6) a2))
  $ ./lambda_lifting_runner.exe < manytests/typed/013foldfoldr.ml
  let id = (fun x -> x)
  
  let rec fold_right = (fun f acc xs -> if (is_empty xs)
  then acc
  else if (not (is_empty xs))
  then let h = (list_head xs) in
  let tl = (list_tail xs) in
  ((f h) (((fold_right f) acc) tl))
  else fail_match)
  
  let a0 = (fun f b g x -> (g ((f x) b)))
  
  let foldl = (fun f a bs -> ((((fold_right (a0 f)) id) bs) a))
  
  let a1 = (fun x y -> ((( * ) x) y))
  
  let main = (print_int (((foldl a1) 1) (1::(2::(3::[])))))

  $ ./lambda_lifting_runner.exe < manytests/typed/015tuples.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let map = (fun f p -> let a = ((tuple_element p) 0) in
  let b = ((tuple_element p) 1) in
  ((f a), (f b)))
  
  let a1 = (fun self a0 -> ((map ((a2 a0) self)) a0))
  
  let fixpoly = (fun l -> ((fix a1) l))
  
  let feven = (fun p n -> let e = ((tuple_element p) 0) in
  let o = ((tuple_element p) 1) in
  if ((( = ) n) 0)
  then 1
  else (o ((( - ) n) 1)))
  
  let fodd = (fun p n -> let e = ((tuple_element p) 0) in
  let o = ((tuple_element p) 1) in
  if ((( = ) n) 0)
  then 0
  else (e ((( - ) n) 1)))
  
  let tie = (fixpoly (feven, fodd))
  
  let rec meven = (fun n -> if ((( = ) n) 0)
  then 1
  else (modd ((( - ) n) 1)))
  and modd = (fun n -> if ((( = ) n) 0)
  then 1
  else (meven ((( - ) n) 1)))
  
  let main = let () = (print_int (modd 1)) in
  let () = (print_int (meven 2)) in
  let even = ((tuple_element tie) 0) in
  let odd = ((tuple_element tie) 1) in
  let () = (print_int (odd 3)) in
  let () = (print_int (even 4)) in
  0

  $ ./lambda_lifting_runner.exe < manytests/typed/016lists.ml
  let rec length = (fun xs -> if (is_empty xs)
  then 0
  else if (not (is_empty xs))
  then let h = (list_head xs) in
  let tl = (list_tail xs) in
  ((( + ) 1) (length tl))
  else fail_match)
  
  let rec a1 = (fun acc xs -> if (is_empty xs)
  then acc
  else if (not (is_empty xs))
  then let h = (list_head xs) in
  let tl = (list_tail xs) in
  ((a1 ((( + ) acc) 1)) tl)
  else fail_match)
  
  let length_tail = a1
  
  let rec map = (fun f xs -> if (is_empty xs)
  then []
  else if ((( && ) (not (is_empty xs))) (is_empty (list_tail xs)))
  then let a = (list_head xs) in
  ((f a)::[])
  else if ((( && ) (not (is_empty xs))) (is_empty (list_tail (list_tail xs))))
  then let a = (list_head xs) in
  let b = (list_head (list_tail xs)) in
  ((f a)::((f b)::[]))
  else if ((( && ) (not (is_empty xs))) (is_empty (list_tail (list_tail (list_tail xs)))))
  then let a = (list_head xs) in
  let b = (list_head (list_tail xs)) in
  let c = (list_head (list_tail (list_tail xs))) in
  ((f a)::((f b)::((f c)::[])))
  else if (not (is_empty xs))
  then let a = (list_head xs) in
  let b = (list_head (list_tail xs)) in
  let c = (list_head (list_tail (list_tail xs))) in
  let d = (list_head (list_tail (list_tail (list_tail xs)))) in
  let tl = (list_tail (list_tail (list_tail (list_tail xs)))) in
  ((f a)::((f b)::((f c)::((f d)::((map f) tl)))))
  else fail_match)
  
  let rec append = (fun xs ys -> if (is_empty xs)
  then ys
  else if (not (is_empty xs))
  then let x = (list_head xs) in
  let a0 = (list_tail xs) in
  (x::((append a0) ys))
  else fail_match)
  
  let rec a2 = (fun xs -> if (is_empty xs)
  then []
  else if (not (is_empty xs))
  then let h = (list_head xs) in
  let tl = (list_tail xs) in
  ((append h) (a2 tl))
  else fail_match)
  
  let concat = a2
  
  let rec iter = (fun f xs -> if (is_empty xs)
  then ()
  else if (not (is_empty xs))
  then let h = (list_head xs) in
  let tl = (list_tail xs) in
  let () = (f h) in
  ((iter f) tl)
  else fail_match)
  
  let a3 = (fun h a -> (h, a))
  
  let rec cartesian = (fun xs ys -> if (is_empty xs)
  then []
  else if (not (is_empty xs))
  then let h = (list_head xs) in
  let tl = (list_tail xs) in
  ((append ((map (a3 h)) ys)) ((cartesian tl) ys))
  else fail_match)
  
  let main = let () = ((iter print_int) (1::(2::(3::[])))) in
  let () = (print_int (length ((cartesian (1::(2::[]))) (1::(2::(3::(4::[]))))))) in
  0
 
  $ ./lambda_lifting_runner.exe < manytests/do_not_type/001.ml
  Infer error:
  $ ./lambda_lifting_runner.exe < manytests/do_not_type/002if.ml
  Infer error:
  $ ./lambda_lifting_runner.exe < manytests/do_not_type/003occurs.ml
  Infer error:

  $ ./lambda_lifting_runner.exe < manytests/do_not_type/004let_poly.ml
  Infer error:

  $ ./lambda_lifting_runner.exe < manytests/do_not_type/015tuples.ml
  Infer error:


  $ ./pe_runner.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ ./pe_runner.exe < manytests/typed/002fac.ml
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) (fun p -> (k ((( * ) p) n)))))
  
  let main = let () = (print_int ((fac_cps 4) (fun print_int -> print_int))) in
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
  
  let main = let () = (print_int (((fib_acc 0) 1) 4)) in
  let () = (print_int (fib 4)) in
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
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ ./pe_runner.exe < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ ./pe_runner.exe < manytests/typed/006partial.ml
  let foo = (fun b -> if b
  then (fun foo -> ((( + ) foo) 2))
  else (fun foo -> ((( * ) foo) 10)))
  
  let foo = (fun x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))))
  
  let main = let () = (print_int (foo 11)) in
  0

  $ ./pe_runner.exe < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let foo = (foo 1) in
  let foo = (foo 2) in
  let foo = (foo 3) in
  let () = (print_int foo) in
  0
  $ ./pe_runner.exe < manytests/typed/006partial3.ml
  let foo = (fun a -> let () = (print_int a) in
  (fun b -> let () = (print_int b) in
  (fun c -> (print_int c))))
  
  let main = let () = (((foo 4) 8) 9) in
  0
  $ ./pe_runner.exe < manytests/typed/007order.ml
  let _start = (fun a0 a1 a a2 b _c a3 d __ -> let a4 = (a0, a1, a2, a3) in
  let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (( ~- ) 1))) 10000) (( ~- ) 555555)))
  $ ./pe_runner.exe < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let main = let () = (print_int (((addi (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))) (fun _start -> ((( = ) ((( / ) _start) 2)) 0))) 4)) in
  0

  $ ./pe_runner.exe < manytests/typed/009let_poly.ml
  let temp = let f = (fun x -> x) in
  ((f 1), (f true))

  $ ./pe_runner.exe < manytests/typed/011mapcps.ml
  let rec map = (fun f xs k -> if (#is_empty xs)
  then (k [])
  else if (not (#is_empty xs))
  then let h = (#list_head xs) in
  let tl = (#list_tail xs) in
  (((map f) tl) (fun tl -> (k ((f h)::tl))))
  else #fail_match)
  
  let rec iter = (fun f xs -> if (#is_empty xs)
  then ()
  else if (not (#is_empty xs))
  then let h = (#list_head xs) in
  let tl = (#list_tail xs) in
  let w = (f h) in
  ((iter f) tl)
  else #fail_match)
  
  let main = ((iter print_int) (((map (fun x -> ((( + ) x) 1))) (1::(2::(3::[])))) (fun x -> x)))
  $ ./pe_runner.exe < manytests/typed/012fibcps.ml
  let rec fib = (fun n k -> if ((( < ) n) 2)
  then (k n)
  else ((fib ((( - ) n) 1)) (fun a -> ((fib ((( - ) n) 2)) (fun b -> (k ((( + ) a) b)))))))
  
  let main = (print_int ((fib 6) (fun x -> x)))
  $ ./pe_runner.exe < manytests/typed/013foldfoldr.ml
  let id = (fun x -> x)
  
  let rec fold_right = (fun f acc xs -> if (#is_empty xs)
  then acc
  else if (not (#is_empty xs))
  then let h = (#list_head xs) in
  let tl = (#list_tail xs) in
  ((f h) (((fold_right f) acc) tl))
  else #fail_match)
  
  let foldl = (fun f a bs -> ((((fold_right (fun b g x -> (g ((f x) b)))) id) bs) a))
  
  let main = (print_int (((foldl (fun x y -> ((( * ) x) y))) 1) (1::(2::(3::[])))))

  $ ./pe_runner.exe < manytests/typed/015tuples.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let map = (fun f p -> let a = ((#tuple_element p) 0) in
  let b = ((#tuple_element p) 1) in
  ((f a), (f b)))
  
  let fixpoly = (fun l -> ((fix (fun self l -> ((map (fun li x -> ((li (self l)) x))) l))) l))
  
  let feven = (fun p n -> let e = ((#tuple_element p) 0) in
  let o = ((#tuple_element p) 1) in
  if ((( = ) n) 0)
  then 1
  else (o ((( - ) n) 1)))
  
  let fodd = (fun p n -> let e = ((#tuple_element p) 0) in
  let o = ((#tuple_element p) 1) in
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
  let even = ((#tuple_element tie) 0) in
  let odd = ((#tuple_element tie) 1) in
  let () = (print_int (odd 3)) in
  let () = (print_int (even 4)) in
  0

  $ ./pe_runner.exe < manytests/typed/016lists.ml
  let rec length = (fun xs -> if (#is_empty xs)
  then 0
  else if (not (#is_empty xs))
  then let h = (#list_head xs) in
  let tl = (#list_tail xs) in
  ((( + ) 1) (length tl))
  else #fail_match)
  
  let length_tail = let rec helper = (fun acc xs -> if (#is_empty xs)
  then acc
  else if (not (#is_empty xs))
  then let h = (#list_head xs) in
  let tl = (#list_tail xs) in
  ((helper ((( + ) acc) 1)) tl)
  else #fail_match) in
  (helper 0)
  
  let rec map = (fun f xs -> if (#is_empty xs)
  then []
  else if ((( && ) (not (#is_empty xs))) (#is_empty (#list_tail xs)))
  then let a = (#list_head xs) in
  ((f a)::[])
  else if ((( && ) (not (#is_empty xs))) (#is_empty (#list_tail (#list_tail xs))))
  then let a = (#list_head xs) in
  let b = (#list_head (#list_tail xs)) in
  ((f a)::((f b)::[]))
  else if ((( && ) (not (#is_empty xs))) (#is_empty (#list_tail (#list_tail (#list_tail xs)))))
  then let a = (#list_head xs) in
  let b = (#list_head (#list_tail xs)) in
  let c = (#list_head (#list_tail (#list_tail xs))) in
  ((f a)::((f b)::((f c)::[])))
  else if (not (#is_empty xs))
  then let a = (#list_head xs) in
  let b = (#list_head (#list_tail xs)) in
  let c = (#list_head (#list_tail (#list_tail xs))) in
  let d = (#list_head (#list_tail (#list_tail (#list_tail xs)))) in
  let tl = (#list_tail (#list_tail (#list_tail (#list_tail xs)))) in
  ((f a)::((f b)::((f c)::((f d)::((map f) tl)))))
  else #fail_match)
  
  let rec append = (fun xs ys -> if (#is_empty xs)
  then ys
  else if (not (#is_empty xs))
  then let x = (#list_head xs) in
  let xs = (#list_tail xs) in
  (x::((append xs) ys))
  else #fail_match)
  
  let concat = let rec helper = (fun xs -> if (#is_empty xs)
  then []
  else if (not (#is_empty xs))
  then let h = (#list_head xs) in
  let tl = (#list_tail xs) in
  ((append h) (helper tl))
  else #fail_match) in
  helper
  
  let rec iter = (fun f xs -> if (#is_empty xs)
  then ()
  else if (not (#is_empty xs))
  then let h = (#list_head xs) in
  let tl = (#list_tail xs) in
  let () = (f h) in
  ((iter f) tl)
  else #fail_match)
  
  let rec cartesian = (fun xs ys -> if (#is_empty xs)
  then []
  else if (not (#is_empty xs))
  then let h = (#list_head xs) in
  let tl = (#list_tail xs) in
  ((append ((map (fun a -> (h, a))) ys)) ((cartesian tl) ys))
  else #fail_match)
  
  let main = let () = ((iter print_int) (1::(2::(3::[])))) in
  let () = (print_int (length ((cartesian (1::(2::[]))) (1::(2::(3::(4::[]))))))) in
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


  $ ./match_elimination_runner.exe << EOF
  > let f (x,y) = x+y;;
  > let main = let () = print_int ( f (1,2) ) in 0;;
  > EOF
  let f = (fun me_me0 -> let x = ((tuple_get me_me0) 0) in
  let y = ((tuple_get me_me0) 1) in
  ((( + ) x) y))
  
  let main = let () = (print_int (f (1, 2))) in
  0

  $ ./match_elimination_runner.exe << EOF
  > let f = let y x = x + 1 in y 3;;
  > EOF
  let f = let y = (fun x -> ((( + ) x) 1)) in
  (y 3)

  $ ./match_elimination_runner.exe << EOF
  > let length xs = match xs with
  > | a::b::[] -> 2
  > | a::[] -> 1
  > | [] -> 0
  > EOF
  let length = (fun xs -> if if (is_cons xs)
  then if (is_cons (tl_list_get xs))
  then (is_empty (tl_list_get (tl_list_get xs)))
  else false
  else false
  then let a = (hd_list_get xs) in
  let b = (hd_list_get (tl_list_get xs)) in
  2
  else if if (is_cons xs)
  then (is_empty (tl_list_get xs))
  else false
  then let a = (hd_list_get xs) in
  1
  else if (is_empty xs)
  then 0
  else fail)

  $ ./match_elimination_runner.exe << EOF
  > let is_empty x = x+1
  > let rec length xs = match xs with
  > | [] -> 0
  > | _::tl -> 1 + length xs
  > EOF
  let is_empty_ac0 = (fun x -> ((( + ) x) 1))
  
  let rec length = (fun xs -> if (is_empty xs)
  then 0
  else if (is_cons xs)
  then let tl = (tl_list_get xs) in
  ((( + ) 1) (length xs))
  else fail)

  $ ./match_elimination_runner.exe << EOF
  > let (a, b) = (5,6)
  > EOF
  let tmp_me0 = (5, 6)
  let a = ((tuple_get tmp_me0) 0)
  let b = ((tuple_get tmp_me0) 1)

  $ ./match_elimination_runner.exe << EOF
  > let fac n =
  > let rec fack n k =
  > if n<=1 then k 1
  > else fack (n - 1) ((fun k n m -> k (m * n)) k n)
  > in
  > fack n (fun x -> x)
  > EOF
  let fac = (fun n -> let rec fack = (fun n_ac0 k -> if ((( <= ) n_ac0) 1)
  then (k 1)
  else ((fack ((( - ) n_ac0) 1)) (((fun k_ac1 n_ac2 m -> (k_ac1 ((( * ) m) n_ac2))) k) n_ac0))) in
  ((fack n) (fun x -> x)))

  $ ./match_elimination_runner.exe << EOF
  > let f x = match x with
  > | 1 -> 12
  > | 12 -> 12
  > | _ -> 325
  > EOF
  let f = (fun x -> if (((=) x) 1)
  then 12
  else if (((=) x) 12)
  then 12
  else if true
  then 325
  else fail)

  $ ./match_elimination_runner.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/002fac.ml
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) (fun p -> (k ((( * ) p) n)))))
  
  let main = let () = (print_int ((fac_cps 4) (fun print_int_ac0 -> print_int_ac0))) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/003fib.ml
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

  $ ./match_elimination_runner.exe < manytests/typed/004manyargs.ml
  let wrap = (fun f -> if ((( = ) 1) 1)
  then f
  else f)
  
  let test3 = (fun a b c -> let a_ac0 = (print_int a) in
  let b_ac1 = (print_int b) in
  let c_ac2 = (print_int c) in
  0)
  
  let test10 = (fun a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/006partial.ml
  let foo = (fun b -> if b
  then (fun foo -> ((( + ) foo) 2))
  else (fun foo -> ((( * ) foo) 10)))
  
  let foo_ac0 = (fun x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))))
  
  let main = let () = (print_int (foo_ac0 11)) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let foo_ac0 = (foo 1) in
  let foo_ac1 = (foo_ac0 2) in
  let foo_ac2 = (foo_ac1 3) in
  let () = (print_int foo_ac2) in
  0
  $ ./match_elimination_runner.exe < manytests/typed/006partial3.ml
  let foo = (fun a -> let () = (print_int a) in
  (fun b -> let () = (print_int b) in
  (fun c -> (print_int c))))
  
  let main = let () = (((foo 4) 8) 9) in
  0
  $ ./match_elimination_runner.exe < manytests/typed/007order.ml
  let _start = (fun () () a () b _c () d __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (( ~- ) 1))) 10000) (( ~- ) 555555)))
  $ ./match_elimination_runner.exe < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let main = let () = (print_int (((addi (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))) (fun _start -> ((( = ) ((( / ) _start) 2)) 0))) 4)) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/009let_poly.ml
  let temp = let f = (fun x -> x) in
  ((f 1), (f true))

  $ ./match_elimination_runner.exe < manytests/typed/011mapcps.ml
  let rec map = (fun f xs k -> if (is_empty xs)
  then (k [])
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  (((map f) tl) (fun tl_ac0 -> (k ((f h)::tl_ac0))))
  else fail)
  
  let rec iter = (fun f xs -> if (is_empty xs)
  then ()
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  let w = (f h) in
  ((iter f) tl)
  else fail)
  
  let main = ((iter print_int) (((map (fun x -> ((( + ) x) 1))) (1::(2::(3::[])))) (fun x -> x)))
  $ ./match_elimination_runner.exe < manytests/typed/012fibcps.ml
  let rec fib = (fun n k -> if ((( < ) n) 2)
  then (k n)
  else ((fib ((( - ) n) 1)) (fun a -> ((fib ((( - ) n) 2)) (fun b -> (k ((( + ) a) b)))))))
  
  let main = (print_int ((fib 6) (fun x -> x)))
  $ ./match_elimination_runner.exe < manytests/typed/013foldfoldr.ml
  let id = (fun x -> x)
  
  let rec fold_right = (fun f acc xs -> if (is_empty xs)
  then acc
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((f h) (((fold_right f) acc) tl))
  else fail)
  
  let foldl = (fun f a bs -> ((((fold_right (fun b g x -> (g ((f x) b)))) id) bs) a))
  
  let main = (print_int (((foldl (fun x y -> ((( * ) x) y))) 1) (1::(2::(3::[])))))

  $ ./match_elimination_runner.exe < manytests/typed/015tuples.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let map = (fun f p -> let a = ((tuple_get p) 0) in
  let b = ((tuple_get p) 1) in
  ((f a), (f b)))
  
  let fixpoly = (fun l -> ((fix (fun self l_ac0 -> ((map (fun li x -> ((li (self l_ac0)) x))) l_ac0))) l))
  
  let feven = (fun p n -> let e = ((tuple_get p) 0) in
  let o = ((tuple_get p) 1) in
  if ((( = ) n) 0)
  then 1
  else (o ((( - ) n) 1)))
  
  let fodd = (fun p n -> let e = ((tuple_get p) 0) in
  let o = ((tuple_get p) 1) in
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
  let even = ((tuple_get tie) 0) in
  let odd = ((tuple_get tie) 1) in
  let () = (print_int (odd 3)) in
  let () = (print_int (even 4)) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/016lists.ml
  let rec length = (fun xs -> if (is_empty xs)
  then 0
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((( + ) 1) (length tl))
  else fail)
  
  let length_tail = let rec helper = (fun acc xs -> if (is_empty xs)
  then acc
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((helper ((( + ) acc) 1)) tl)
  else fail) in
  (helper 0)
  
  let rec map = (fun f xs -> if (is_empty xs)
  then []
  else if if (is_cons xs)
  then (is_empty (tl_list_get xs))
  else false
  then let a = (hd_list_get xs) in
  ((f a)::[])
  else if if (is_cons xs)
  then if (is_cons (tl_list_get xs))
  then (is_empty (tl_list_get (tl_list_get xs)))
  else false
  else false
  then let a = (hd_list_get xs) in
  let b = (hd_list_get (tl_list_get xs)) in
  ((f a)::((f b)::[]))
  else if if (is_cons xs)
  then if (is_cons (tl_list_get xs))
  then if (is_cons (tl_list_get (tl_list_get xs)))
  then (is_empty (tl_list_get (tl_list_get (tl_list_get xs))))
  else false
  else false
  else false
  then let a = (hd_list_get xs) in
  let b = (hd_list_get (tl_list_get xs)) in
  let c = (hd_list_get (tl_list_get (tl_list_get xs))) in
  ((f a)::((f b)::((f c)::[])))
  else if if (is_cons xs)
  then if (is_cons (tl_list_get xs))
  then if (is_cons (tl_list_get (tl_list_get xs)))
  then (is_cons (tl_list_get (tl_list_get (tl_list_get xs))))
  else false
  else false
  else false
  then let a = (hd_list_get xs) in
  let b = (hd_list_get (tl_list_get xs)) in
  let c = (hd_list_get (tl_list_get (tl_list_get xs))) in
  let d = (hd_list_get (tl_list_get (tl_list_get (tl_list_get xs)))) in
  let tl = (tl_list_get (tl_list_get (tl_list_get (tl_list_get xs)))) in
  ((f a)::((f b)::((f c)::((f d)::((map f) tl)))))
  else fail)
  
  let rec append = (fun xs ys -> if (is_empty xs)
  then ys
  else if (is_cons xs)
  then let x = (hd_list_get xs) in
  let xs_ac0 = (tl_list_get xs) in
  (x::((append xs_ac0) ys))
  else fail)
  
  let concat = let rec helper = (fun xs -> if (is_empty xs)
  then []
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((append h) (helper tl))
  else fail) in
  helper
  
  let rec iter = (fun f xs -> if (is_empty xs)
  then ()
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  let () = (f h) in
  ((iter f) tl)
  else fail)
  
  let rec cartesian = (fun xs ys -> if (is_empty xs)
  then []
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((append ((map (fun a -> (h, a))) ys)) ((cartesian tl) ys))
  else fail)
  
  let main = let () = ((iter print_int) (1::(2::(3::[])))) in
  let () = (print_int (length ((cartesian (1::(2::[]))) (1::(2::(3::(4::[]))))))) in
  0
 
  $ ./match_elimination_runner.exe < manytests/do_not_type/001.ml
  Infer error:
  $ ./match_elimination_runner.exe < manytests/do_not_type/002if.ml
  Infer error:
  $ ./match_elimination_runner.exe < manytests/do_not_type/003occurs.ml
  Infer error:

  $ ./match_elimination_runner.exe < manytests/do_not_type/004let_poly.ml
  Infer error:

  $ ./match_elimination_runner.exe < manytests/do_not_type/015tuples.ml
  Infer error:


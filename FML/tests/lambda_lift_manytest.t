  $ ./lambda_lift_runner.exe << EOF
  > let main = 
  > let rec fib n k =
  > if n < 2
  > then k n
  > else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b))) in print_int(fib 6 (fun x -> x))
  > EOF
  let lam_ll2 = (fun a k b -> (k ((( + ) a) b)))
  
  let lam_ll1 = (fun fib_ll0 k n a -> ((fib_ll0 ((( - ) n) 2)) ((lam_ll2 a) k)))
  
  let rec fib_ll0 = (fun n k -> if ((( < ) n) 2)
  then (k n)
  else ((fib_ll0 ((( - ) n) 1)) (((lam_ll1 fib_ll0) k) n)))
  
  let lam_ll3 = (fun x -> x)
  
  let main = (print_int ((fib_ll0 6) lam_ll3))

  $ ./lambda_lift_runner.exe << EOF
  > let f x = let g y = x + y in g 5;;
  > EOF
  let g_ll0 = (fun x y -> ((( + ) x) y))
  
  let f = (fun x -> ((g_ll0 x) 5))

  $ ./lambda_lift_runner.exe << EOF
  > let length xs = match xs with
  > | a::b::[] -> 2
  > | a::[] -> 1
  > | [] -> 0
  > EOF
  let length = (fun xs -> if ((( && ) ((( && ) (is_cons xs)) (is_empty (tl_list_get (tl_list_get xs))))) (is_cons (tl_list_get xs)))
  then let a = (hd_list_get xs) in
  let b = (hd_list_get (tl_list_get xs)) in
  2
  else if ((( && ) (is_cons xs)) (is_empty (tl_list_get xs)))
  then let a = (hd_list_get xs) in
  1
  else if (is_empty xs)
  then 0
  else fail)

  $ ./lambda_lift_runner.exe << EOF
  > let f = let y x = x + 1 in y 3;;
  > EOF
  let y_ll0 = (fun x -> ((( + ) x) 1))
  
  let f = (y_ll0 3)

  $ ./lambda_lift_runner.exe << EOF
  > let length xs = match xs with
  > | a::b::[] -> 2
  > | a::[] -> 1
  > | [] -> 0
  > EOF
  let length = (fun xs -> if ((( && ) ((( && ) (is_cons xs)) (is_empty (tl_list_get (tl_list_get xs))))) (is_cons (tl_list_get xs)))
  then let a = (hd_list_get xs) in
  let b = (hd_list_get (tl_list_get xs)) in
  2
  else if ((( && ) (is_cons xs)) (is_empty (tl_list_get xs)))
  then let a = (hd_list_get xs) in
  1
  else if (is_empty xs)
  then 0
  else fail)

  $ ./lambda_lift_runner.exe << EOF
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

  $ ./lambda_lift_runner.exe << EOF
  > let (a, b) = (5,6)
  > EOF
  let tmp_me0 = (5, 6)
  let a = ((tuple_get tmp_me0) 0)
  let b = ((tuple_get tmp_me0) 1)

  $ ./lambda_lift_runner.exe << EOF
  > let fac n =
  > let rec fack n k =
  > if n<=1 then k 1
  > else fack (n - 1) ((fun k n m -> k (m * n)) k n)
  > in
  > fack n (fun x -> x)
  > EOF
  let lam_ll1 = (fun k_ac1 n_ac2 m -> (k_ac1 ((( * ) m) n_ac2)))
  
  let rec fack_ll0 = (fun n_ac0 k -> if ((( <= ) n_ac0) 1)
  then (k 1)
  else ((fack_ll0 ((( - ) n_ac0) 1)) ((lam_ll1 k) n_ac0)))
  
  let lam_ll2 = (fun x -> x)
  
  let fac = (fun n -> ((fack_ll0 n) lam_ll2))

  $ ./lambda_lift_runner.exe << EOF
  > let f x = match x with
  > | 1 -> 12
  > | 12 -> 12
  > | _ -> 325
  > EOF
  let f = (fun x -> if ((( = ) x) 1)
  then 12
  else if ((( = ) x) 12)
  then 12
  else if true
  then 325
  else fail)

  $ ./lambda_lift_runner.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/002fac.ml
  let lam_ll0 = (fun k n p -> (k ((( * ) p) n)))
  
  let rec fac_cps = (fun n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) ((lam_ll0 k) n)))
  
  let lam_ll1 = (fun print_int_ac0 -> print_int_ac0)
  
  let main = let () = (print_int ((fac_cps 4) lam_ll1)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/003fib.ml
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

  $ ./lambda_lift_runner.exe < manytests/typed/004manyargs.ml
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

  $ ./lambda_lift_runner.exe < manytests/typed/005fix.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let fac = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/006partial.ml
  let lam_ll0 = (fun foo -> ((( + ) foo) 2))
  
  let lam_ll1 = (fun foo -> ((( * ) foo) 10))
  
  let foo = (fun b -> if b
  then lam_ll0
  else lam_ll1)
  
  let foo_ac0 = (fun x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))))
  
  let main = let () = (print_int (foo_ac0 11)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let foo_ac0 = (foo 1) in
  let foo_ac1 = (foo_ac0 2) in
  let foo_ac2 = (foo_ac1 3) in
  let () = (print_int foo_ac2) in
  0
  $ ./lambda_lift_runner.exe < manytests/typed/006partial3.ml
  let lam_ll1 = (fun c -> (print_int c))
  
  let lam_ll0 = (fun b -> let () = (print_int b) in
  lam_ll1)
  
  let foo = (fun a -> let () = (print_int a) in
  lam_ll0)
  
  let main = let () = (((foo 4) 8) 9) in
  0
  $ ./lambda_lift_runner.exe < manytests/typed/007order.ml
  let _start = (fun () () a () b _c () d __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (( ~- ) 1))) 10000) (( ~- ) 555555)))
  $ ./lambda_lift_runner.exe < manytests/typed/008ascription.ml
  let addi = (fun f g x -> ((f x) (g x)))
  
  let lam_ll0 = (fun x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))
  
  let lam_ll1 = (fun _start -> ((( = ) ((( / ) _start) 2)) 0))
  
  let main = let () = (print_int (((addi lam_ll0) lam_ll1) 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/009let_poly.ml
  let f_ll0 = (fun x -> x)
  
  let temp = ((f_ll0 1), (f_ll0 true))

  $ ./lambda_lift_runner.exe < manytests/typed/011mapcps.ml
  let lam_ll0 = (fun f h k tl_ac0 -> (k ((f h)::tl_ac0)))
  
  let rec map = (fun f xs k -> if (is_empty xs)
  then (k [])
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  (((map f) tl) (((lam_ll0 f) h) k))
  else fail)
  
  let rec iter = (fun f xs -> if (is_empty xs)
  then ()
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  let w = (f h) in
  ((iter f) tl)
  else fail)
  
  let lam_ll1 = (fun x -> ((( + ) x) 1))
  
  let lam_ll2 = (fun x -> x)
  
  let main = ((iter print_int) (((map lam_ll1) (1::(2::(3::[])))) lam_ll2))
  $ ./lambda_lift_runner.exe < manytests/typed/012fibcps.ml
  let lam_ll1 = (fun a k b -> (k ((( + ) a) b)))
  
  let lam_ll0 = (fun fib k n a -> ((fib ((( - ) n) 2)) ((lam_ll1 a) k)))
  
  let rec fib = (fun n k -> if ((( < ) n) 2)
  then (k n)
  else ((fib ((( - ) n) 1)) (((lam_ll0 fib) k) n)))
  
  let lam_ll2 = (fun x -> x)
  
  let main = (print_int ((fib 6) lam_ll2))
  $ ./lambda_lift_runner.exe < manytests/typed/013foldfoldr.ml
  let id = (fun x -> x)
  
  let rec fold_right = (fun f acc xs -> if (is_empty xs)
  then acc
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((f h) (((fold_right f) acc) tl))
  else fail)
  
  let lam_ll1 = (fun f b g x -> (g ((f x) b)))
  
  let lam_ll0 = (fun fold_right f a bs -> ((((fold_right (lam_ll1 f)) id) bs) a))
  
  let foldl = (lam_ll0 fold_right)
  
  let lam_ll2 = (fun x y -> ((( * ) x) y))
  
  let main = (print_int (((foldl lam_ll2) 1) (1::(2::(3::[])))))

  $ ./lambda_lift_runner.exe < manytests/typed/015tuples.ml
  let rec fix = (fun f x -> ((f (fix f)) x))
  
  let map = (fun f p -> let a = ((tuple_get p) 0) in
  let b = ((tuple_get p) 1) in
  ((f a), (f b)))
  
  let lam_ll2 = (fun l_ac0 self li x -> ((li (self l_ac0)) x))
  
  let lam_ll1 = (fun self l_ac0 -> ((map ((lam_ll2 l_ac0) self)) l_ac0))
  
  let lam_ll0 = (fun fix l -> ((fix lam_ll1) l))
  
  let fixpoly = (lam_ll0 fix)
  
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

  $ ./lambda_lift_runner.exe < manytests/typed/016lists.ml
  let rec length = (fun xs -> if (is_empty xs)
  then 0
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((( + ) 1) (length tl))
  else fail)
  
  let rec helper_ll0 = (fun acc xs -> if (is_empty xs)
  then acc
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((helper_ll0 ((( + ) acc) 1)) tl)
  else fail)
  
  let length_tail = (helper_ll0 0)
  
  let rec map = (fun f xs -> if (is_empty xs)
  then []
  else if ((( && ) (is_cons xs)) (is_empty (tl_list_get xs)))
  then let a = (hd_list_get xs) in
  ((f a)::[])
  else if ((( && ) ((( && ) (is_cons xs)) (is_empty (tl_list_get (tl_list_get xs))))) (is_cons (tl_list_get xs)))
  then let a = (hd_list_get xs) in
  let b = (hd_list_get (tl_list_get xs)) in
  ((f a)::((f b)::[]))
  else if ((( && ) ((( && ) ((( && ) (is_cons xs)) (is_empty (tl_list_get (tl_list_get (tl_list_get xs)))))) (is_cons (tl_list_get (tl_list_get xs))))) (is_cons (tl_list_get xs)))
  then let a = (hd_list_get xs) in
  let b = (hd_list_get (tl_list_get xs)) in
  let c = (hd_list_get (tl_list_get (tl_list_get xs))) in
  ((f a)::((f b)::((f c)::[])))
  else if ((( && ) ((( && ) ((( && ) (is_cons xs)) (is_cons (tl_list_get (tl_list_get (tl_list_get xs)))))) (is_cons (tl_list_get (tl_list_get xs))))) (is_cons (tl_list_get xs)))
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
  
  let rec helper_ll1 = (fun append xs -> if (is_empty xs)
  then []
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((append h) ((helper_ll1 append) tl))
  else fail)
  
  let concat = (helper_ll1 append)
  
  let rec iter = (fun f xs -> if (is_empty xs)
  then ()
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  let () = (f h) in
  ((iter f) tl)
  else fail)
  
  let lam_ll2 = (fun h a -> (h, a))
  
  let rec cartesian = (fun xs ys -> if (is_empty xs)
  then []
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((append ((map (lam_ll2 h)) ys)) ((cartesian tl) ys))
  else fail)
  
  let main = let () = ((iter print_int) (1::(2::(3::[])))) in
  let () = (print_int (length ((cartesian (1::(2::[]))) (1::(2::(3::(4::[]))))))) in
  0
 

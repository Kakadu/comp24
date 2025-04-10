  $ ./lambda_lift_runner.exe << EOF
  > let length xs = match xs with
  > | a::b::[] -> 2
  > | a::[] -> 1
  > | [] -> 0
  > EOF
  let lam_ll0 = (fun xs -> if if (is_cons xs)
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
  let length = lam_ll0

  $ ./lambda_lift_runner.exe << EOF
  > let f = let y x = x + 1 in y 3;;
  > EOF
  let lam_ll0 = (fun x -> ((( + ) x) 1))
  let f = let y = lam_ll0 in
  (y 3)

  $ ./lambda_lift_runner.exe << EOF
  > let length xs = match xs with
  > | a::b::[] -> 2
  > | a::[] -> 1
  > | [] -> 0
  > EOF
  let lam_ll0 = (fun xs -> if if (is_cons xs)
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
  let length = lam_ll0

  $ ./lambda_lift_runner.exe << EOF
  > let is_empty x = x+1
  > let rec length xs = match xs with
  > | [] -> 0
  > | _::tl -> 1 + length xs
  > EOF
  let lam_ll0 = (fun x -> ((( + ) x) 1))
  let is_empty_ac0 = lam_ll0
  
  let rec lam_ll1 = (fun length xs -> if (is_empty xs)
  then 0
  else if (is_cons xs)
  then let _ = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((( + ) 1) (length xs))
  else fail)
  and length = (lam_ll1 length)

  $ ./lambda_lift_runner.exe << EOF
  > let (a, b) = (5,6)
  > EOF
  let tmp_me0 = (5, 6)
  let a = (tuple_get (tmp_me0, 0))
  let b = (tuple_get (tmp_me0, 1))

  $ ./lambda_lift_runner.exe << EOF
  > let fac n =
  > let rec fack n k =
  > if n<=1 then k 1
  > else fack (n - 1) ((fun k n m -> k (m * n)) k n)
  > in
  > fack n (fun x -> x)
  > EOF
  let lam_ll5 = (fun k_ac1 n_ac2 m -> (k_ac1 ((( * ) m) n_ac2)))
  let lam_ll4 = (fun k_ac1 n_ac2 -> ((lam_ll5 k_ac1) n_ac2))
  let lam_ll3 = (fun k_ac1 -> (lam_ll4 k_ac1))
  let lam_ll2 = (fun n_ac0 fack n_ac0 n_ac0 k -> if ((( <= ) n_ac0) 1)
  then (k 1)
  else ((fack ((( - ) n_ac0) 1)) ((lam_ll3 k) n_ac0)))
  let lam_ll1 = (fun fack n_ac0 -> ((((lam_ll2 n_ac0) fack) n_ac0) n_ac0))
  let lam_ll6 = (fun x -> x)
  let lam_ll0 = (fun fack n -> let rec fack = (lam_ll1 fack) in
  ((fack n) lam_ll6))
  let fac = (lam_ll0 fack)

  $ ./lambda_lift_runner.exe << EOF
  > let f x = match x with
  > | 1 -> 12
  > | 12 -> 12
  > | _ -> 325
  > EOF
  let lam_ll0 = (fun (=) (=) x -> if (((=) x) 1)
  then 12
  else if (((=) x) 12)
  then 12
  else if true
  then 325
  else fail)
  let f = ((lam_ll0 (=)) (=))

  $ ./lambda_lift_runner.exe < manytests/typed/001fac.ml
  let rec lam_ll0 = (fun fac n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  and fac = (lam_ll0 fac)
  
  let main = let () = (print_int (fac 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/002fac.ml
  let rec lam_ll2 = (fun k n p -> (k ((( * ) p) n)))
  and lam_ll1 = (fun n fac_cps n n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) ((lam_ll2 k) n)))
  and lam_ll0 = (fun fac_cps n -> ((((lam_ll1 n) fac_cps) n) n))
  and fac_cps = (lam_ll0 fac_cps)
  
  let lam_ll3 = (fun print_int_ac0 -> print_int_ac0)
  let main = let () = (print_int ((fac_cps 4) lam_ll3)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/003fib.ml
  let rec lam_ll2 = (fun b a b fib_acc b n -> if ((( = ) n) 1)
  then b
  else let n1 = ((( - ) n) 1) in
  let ab = ((( + ) a) b) in
  (((fib_acc b) ab) n1))
  and lam_ll1 = (fun a fib_acc b -> (((((lam_ll2 b) a) b) fib_acc) b))
  and lam_ll0 = (fun fib_acc a -> ((lam_ll1 a) fib_acc))
  and fib_acc = (lam_ll0 fib_acc)
  
  let rec lam_ll3 = (fun fib fib n -> if ((( < ) n) 2)
  then n
  else ((( + ) (fib ((( - ) n) 1))) (fib ((( - ) n) 2))))
  and fib = ((lam_ll3 fib) fib)
  
  let main = let () = (print_int (((fib_acc 0) 1) 4)) in
  let () = (print_int (fib 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/004manyargs.ml
  let lam_ll0 = (fun f -> if ((( = ) 1) 1)
  then f
  else f)
  let wrap = lam_ll0
  
  let lam_ll3 = (fun a b c -> let a_ac0 = (print_int a) in
  let b_ac1 = (print_int b) in
  let c_ac2 = (print_int c) in
  0)
  let lam_ll2 = (fun a b -> ((lam_ll3 a) b))
  let lam_ll1 = (fun a -> (lam_ll2 a))
  let test3 = lam_ll1
  
  let lam_ll13 = (fun a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  let lam_ll12 = (fun a b c d e f g h i -> (((((((((lam_ll13 a) b) c) d) e) f) g) h) i))
  let lam_ll11 = (fun a b c d e f g h -> ((((((((lam_ll12 a) b) c) d) e) f) g) h))
  let lam_ll10 = (fun a b c d e f g -> (((((((lam_ll11 a) b) c) d) e) f) g))
  let lam_ll9 = (fun a b c d e f -> ((((((lam_ll10 a) b) c) d) e) f))
  let lam_ll8 = (fun a b c d e -> (((((lam_ll9 a) b) c) d) e))
  let lam_ll7 = (fun a b c d -> ((((lam_ll8 a) b) c) d))
  let lam_ll6 = (fun a b c -> (((lam_ll7 a) b) c))
  let lam_ll5 = (fun a b -> ((lam_ll6 a) b))
  let lam_ll4 = (fun a -> (lam_ll5 a))
  let test10 = lam_ll4
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/005fix.ml
  let rec lam_ll1 = (fun f fix f x -> ((f (fix f)) x))
  and lam_ll0 = (fun fix f -> (((lam_ll1 f) fix) f))
  and fix = (lam_ll0 fix)
  
  let lam_ll3 = (fun self n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  let lam_ll2 = (fun self -> (lam_ll3 self))
  let fac = lam_ll2
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/006partial.ml
  let lam_ll1 = (fun foo -> ((( + ) foo) 2))
  let lam_ll2 = (fun foo -> ((( * ) foo) 10))
  let lam_ll0 = (fun b -> if b
  then lam_ll1
  else lam_ll2)
  let foo = lam_ll0
  
  let lam_ll3 = (fun foo foo foo foo x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))))
  let foo_ac0 = ((((lam_ll3 foo) foo) foo) foo)
  
  let main = let () = (print_int (foo_ac0 11)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/006partial2.ml
  let lam_ll2 = (fun a b a b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  let lam_ll1 = (fun a a b -> ((((lam_ll2 a) b) a) b))
  let lam_ll0 = (fun a -> ((lam_ll1 a) a))
  let foo = lam_ll0
  
  let main = let foo_ac0 = (foo 1) in
  let foo_ac1 = (foo_ac0 2) in
  let foo_ac2 = (foo_ac1 3) in
  let () = (print_int foo_ac2) in
  0
  $ ./lambda_lift_runner.exe < manytests/typed/006partial3.ml
  let lam_ll2 = (fun c -> (print_int c))
  let lam_ll1 = (fun b -> let () = (print_int b) in
  lam_ll2)
  let lam_ll0 = (fun a -> let () = (print_int a) in
  lam_ll1)
  let foo = lam_ll0
  
  let main = let () = (((foo 4) 8) 9) in
  0
  $ ./lambda_lift_runner.exe < manytests/typed/007order.ml
  let lam_ll8 = (fun a b a b _c d __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  let lam_ll7 = (fun a b a b _c d -> ((((((lam_ll8 a) b) a) b) _c) d))
  let lam_ll6 = (fun a b a b _c () -> (((((lam_ll7 a) b) a) b) _c))
  let lam_ll5 = (fun a b a b _c -> (((((lam_ll6 a) b) a) b) _c))
  let lam_ll4 = (fun a a b -> ((((lam_ll5 a) b) a) b))
  let lam_ll3 = (fun a a () -> ((lam_ll4 a) a))
  let lam_ll2 = (fun a -> ((lam_ll3 a) a))
  let lam_ll1 = (fun () -> lam_ll2)
  let lam_ll0 = (fun () -> lam_ll1)
  let _start = lam_ll0
  
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (( ~- ) 1))) 10000) (( ~- ) 555555)))
  $ ./lambda_lift_runner.exe < manytests/typed/008ascription.ml
  let lam_ll2 = (fun f g x -> ((f x) (g x)))
  let lam_ll1 = (fun f g -> ((lam_ll2 f) g))
  let lam_ll0 = (fun f -> (lam_ll1 f))
  let addi = lam_ll0
  
  let lam_ll4 = (fun x x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))
  let lam_ll3 = (fun x -> ((lam_ll4 x) x))
  let lam_ll5 = (fun _start -> ((( = ) ((( / ) _start) 2)) 0))
  let main = let () = (print_int (((addi lam_ll3) lam_ll5) 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/009let_poly.ml
  let lam_ll0 = (fun x -> x)
  let temp = let f = lam_ll0 in
  ((f 1), (f true))

  $ ./lambda_lift_runner.exe < manytests/typed/011mapcps.ml
  let rec lam_ll3 = (fun k f h tl_ac0 -> (k ((f h)::tl_ac0)))
  and lam_ll2 = (fun xs xs xs xs map f f k -> if (is_empty xs)
  then (k [])
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  (((map f) tl) (((lam_ll3 k) f) h))
  else fail)
  and lam_ll1 = (fun map f f xs -> (((((((lam_ll2 xs) xs) xs) xs) map) f) f))
  and lam_ll0 = (fun map f -> (((lam_ll1 map) f) f))
  and map = (lam_ll0 map)
  
  let rec lam_ll5 = (fun f iter f xs -> if (is_empty xs)
  then ()
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  let w = (f h) in
  ((iter f) tl)
  else fail)
  and lam_ll4 = (fun iter f -> (((lam_ll5 f) iter) f))
  and iter = (lam_ll4 iter)
  
  let lam_ll6 = (fun x -> ((( + ) x) 1))
  let lam_ll7 = (fun x -> x)
  let main = ((iter print_int) (((map lam_ll6) (1::(2::(3::[])))) lam_ll7))
  $ ./lambda_lift_runner.exe < manytests/typed/012fibcps.ml
  let rec lam_ll3 = (fun k a b -> (k ((( + ) a) b)))
  and lam_ll2 = (fun fib n k a -> ((fib ((( - ) n) 2)) ((lam_ll3 k) a)))
  and lam_ll1 = (fun n n fib n fib n k -> if ((( < ) n) 2)
  then (k n)
  else ((fib ((( - ) n) 1)) (((lam_ll2 fib) n) k)))
  and lam_ll0 = (fun fib fib n -> ((((((lam_ll1 n) n) fib) n) fib) n))
  and fib = ((lam_ll0 fib) fib)
  
  let lam_ll4 = (fun x -> x)
  let main = (print_int ((fib 6) lam_ll4))
  $ ./lambda_lift_runner.exe < manytests/typed/013foldfoldr.ml
  let lam_ll0 = (fun x -> x)
  let id = lam_ll0
  
  let rec lam_ll3 = (fun acc f fold_right f acc xs -> if (is_empty xs)
  then acc
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((f h) (((fold_right f) acc) tl))
  else fail)
  and lam_ll2 = (fun f fold_right f acc -> (((((lam_ll3 acc) f) fold_right) f) acc))
  and lam_ll1 = (fun fold_right f -> (((lam_ll2 f) fold_right) f))
  and fold_right = (lam_ll1 fold_right)
  
  let lam_ll9 = (fun g f b x -> (g ((f x) b)))
  let lam_ll8 = (fun f b g -> (((lam_ll9 g) f) b))
  let lam_ll7 = (fun f b -> ((lam_ll8 f) b))
  let lam_ll6 = (fun fold_right f id a bs -> ((((fold_right (lam_ll7 f)) id) bs) a))
  let lam_ll5 = (fun fold_right f id a -> ((((lam_ll6 fold_right) f) id) a))
  let lam_ll4 = (fun fold_right id f -> (((lam_ll5 fold_right) f) id))
  let foldl = ((lam_ll4 fold_right) id)
  
  let lam_ll11 = (fun x y -> ((( * ) x) y))
  let lam_ll10 = (fun x -> (lam_ll11 x))
  let main = (print_int (((foldl lam_ll10) 1) (1::(2::(3::[])))))

  $ ./lambda_lift_runner.exe < manytests/typed/015tuples.ml
  let rec lam_ll1 = (fun f fix f x -> ((f (fix f)) x))
  and lam_ll0 = (fun fix f -> (((lam_ll1 f) fix) f))
  and fix = (lam_ll0 fix)
  
  let lam_ll3 = (fun f f p -> let a = ((tuple_get p) 0) in
  let b = ((tuple_get p) 1) in
  ((f a), (f b)))
  let lam_ll2 = (fun f -> ((lam_ll3 f) f))
  let map = lam_ll2
  
  let lam_ll8 = (fun li self l_ac0 x -> ((li (self l_ac0)) x))
  let lam_ll7 = (fun self l_ac0 li -> (((lam_ll8 li) self) l_ac0))
  let lam_ll6 = (fun map self l_ac0 -> ((map ((lam_ll7 self) l_ac0)) l_ac0))
  let lam_ll5 = (fun map self -> ((lam_ll6 map) self))
  let lam_ll4 = (fun fix map l -> ((fix (lam_ll5 map)) l))
  let fixpoly = ((lam_ll4 fix) map)
  
  let lam_ll10 = (fun p p n -> let e = ((tuple_get p) 0) in
  let o = ((tuple_get p) 1) in
  if ((( = ) n) 0)
  then 1
  else (o ((( - ) n) 1)))
  let lam_ll9 = (fun p -> ((lam_ll10 p) p))
  let feven = lam_ll9
  
  let lam_ll12 = (fun p p n -> let e = ((tuple_get p) 0) in
  let o = ((tuple_get p) 1) in
  if ((( = ) n) 0)
  then 0
  else (e ((( - ) n) 1)))
  let lam_ll11 = (fun p -> ((lam_ll12 p) p))
  let fodd = lam_ll11
  
  let tie = (fixpoly (feven, fodd))
  
  let rec lam_ll13 = (fun modd n -> if ((( = ) n) 0)
  then 1
  else (modd ((( - ) n) 1)))
  and meven = (lam_ll13 modd)
  and lam_ll14 = (fun meven n -> if ((( = ) n) 0)
  then 1
  else (meven ((( - ) n) 1)))
  and modd = (lam_ll14 meven)
  
  let main = let () = (print_int (modd 1)) in
  let () = (print_int (meven 2)) in
  let even = ((tuple_get tie) 0) in
  let odd = ((tuple_get tie) 1) in
  let () = (print_int (odd 3)) in
  let () = (print_int (even 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/016lists.ml
  let rec lam_ll0 = (fun length xs -> if (is_empty xs)
  then 0
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((( + ) 1) (length tl))
  else fail)
  and length = (lam_ll0 length)
  
  let lam_ll2 = (fun acc helper acc xs -> if (is_empty xs)
  then acc
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((helper ((( + ) acc) 1)) tl)
  else fail)
  let lam_ll1 = (fun helper acc -> (((lam_ll2 acc) helper) acc))
  let length_tail = let rec helper = (lam_ll1 helper) in
  (helper 0)
  
  let rec lam_ll4 = (fun f f f f f f f f f f map f xs -> if (is_empty xs)
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
  and lam_ll3 = (fun map f -> ((((((((((((lam_ll4 f) f) f) f) f) f) f) f) f) f) map) f))
  and map = (lam_ll3 map)
  
  let rec lam_ll6 = (fun xs xs xs xs append ys -> if (is_empty xs)
  then ys
  else if (is_cons xs)
  then let x = (hd_list_get xs) in
  let xs_ac0 = (tl_list_get xs) in
  (x::((append xs_ac0) ys))
  else fail)
  and lam_ll5 = (fun append xs -> (((((lam_ll6 xs) xs) xs) xs) append))
  and append = (lam_ll5 append)
  
  let lam_ll7 = (fun append helper xs -> if (is_empty xs)
  then []
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((append h) (helper tl))
  else fail)
  let concat = let rec helper = ((lam_ll7 append) helper) in
  helper
  
  let rec lam_ll9 = (fun f iter f xs -> if (is_empty xs)
  then ()
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  let () = (f h) in
  ((iter f) tl)
  else fail)
  and lam_ll8 = (fun iter f -> (((lam_ll9 f) iter) f))
  and iter = (lam_ll8 iter)
  
  let rec lam_ll12 = (fun h a -> (h, a))
  and lam_ll11 = (fun xs xs xs xs append map cartesian ys -> if (is_empty xs)
  then []
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((append ((map (lam_ll12 h)) ys)) ((cartesian tl) ys))
  else fail)
  and lam_ll10 = (fun append map cartesian xs -> (((((((lam_ll11 xs) xs) xs) xs) append) map) cartesian))
  and cartesian = (((lam_ll10 append) map) cartesian)
  
  let main = let () = ((iter print_int) (1::(2::(3::[])))) in
  let () = (print_int (length ((cartesian (1::(2::[]))) (1::(2::(3::(4::[]))))))) in
  0
 
  $ ./lambda_lift_runner.exe < manytests/do_not_type/001.ml
  Infer error:
  $ ./lambda_lift_runner.exe < manytests/do_not_type/002if.ml
  Infer error:
  $ ./lambda_lift_runner.exe < manytests/do_not_type/003occurs.ml
  Infer error:

  $ ./lambda_lift_runner.exe < manytests/do_not_type/004let_poly.ml
  Infer error:

  $ ./lambda_lift_runner.exe < manytests/do_not_type/015tuples.ml
  Infer error:


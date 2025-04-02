  $ ./match_elimination_runner.exe << EOF
  > let fac n =
  > let rec fack n k =
  > if n<=1 then k 1
  > else fack (n - 1) ((fun k n m -> k (m * n)) k n)
  > in
  > fack n (fun x -> x)
  > EOF
  let fac = (fun n -> let rec fack = (fun n_ac0 -> (fun k -> if ((( <= ) n_ac0) 1)
  then (k 1)
  else ((fack ((( - ) n_ac0) 1)) (((fun k_ac1 -> (fun n_ac2 -> (fun m -> (k_ac1 ((( * ) m) n_ac2))))) k) n_ac0)))) in
  ((fack n) (fun x -> x)))

  $ ./match_elimination_runner.exe << EOF
  > let f x = match x with
  > | 1 -> true
  > | _ -> false
  > EOF
  let f = (fun x -> let match_tmp_me0 = x in
  if (((=) match_tmp_me0) 1)
  then true
  else let match_tmp_me1 = match_tmp_me0 in
  if true
  then false
  else false)

  $ ./match_elimination_runner.exe < manytests/typed/001fac.ml
  let rec fac = (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/002fac.ml
  let rec fac_cps = (fun n -> (fun k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) (fun p -> (k ((( * ) p) n))))))
  
  let main = let () = (print_int ((fac_cps 4) (fun print_int_ac0 -> print_int_ac0))) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/003fib.ml
  let rec fib_acc = (fun a -> (fun b -> (fun n -> if ((( = ) n) 1)
  then b
  else let n1 = ((( - ) n) 1) in
  let ab = ((( + ) a) b) in
  (((fib_acc b) ab) n1))))
  
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
  
  let test3 = (fun a -> (fun b -> (fun c -> let a_ac0 = (print_int a) in
  let b_ac1 = (print_int b) in
  let c_ac2 = (print_int c) in
  0)))
  
  let test10 = (fun a -> (fun b -> (fun c -> (fun d -> (fun e -> (fun f -> (fun g -> (fun h -> (fun i -> (fun j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j)))))))))))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/005fix.ml
  let rec fix = (fun f -> (fun x -> ((f (fix f)) x)))
  
  let fac = (fun self -> (fun n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1)))))
  
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
  let foo = (fun a -> (fun b -> (fun c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))))
  
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
  let _start = (fun () -> (fun () -> (fun a -> (fun () -> (fun b -> (fun _c -> (fun () -> (fun d -> (fun __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))))))))))
  
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (( ~- ) 1))) 10000) (( ~- ) 555555)))
  $ ./match_elimination_runner.exe < manytests/typed/008ascription.ml
  let addi = (fun f -> (fun g -> (fun x -> ((f x) (g x)))))
  
  let main = let () = (print_int (((addi (fun x -> (fun b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2)))) (fun _start -> ((( = ) ((( / ) _start) 2)) 0))) 4)) in
  0

  $ ./match_elimination_runner.exe < manytests/typed/009let_poly.ml
  let temp = let f = (fun x -> x) in
  ((f 1), (f true))

  $ ./match_elimination_runner.exe < manytests/typed/011mapcps.ml
  let rec map = (fun f -> (fun xs -> (fun k -> let match_tmp_me0 = xs in
  if (is_nil match_tmp_me0)
  then (k [])
  else let match_tmp_me1 = match_tmp_me0 in
  if if (is_cons match_tmp_me1)
  then if true
  then true
  else false
  else false
  then (((map f) tl) (fun tl_ac0 -> (k ((f h)::tl_ac0))))
  else false)))
  
  let rec iter = (fun f -> (fun xs -> let match_tmp_me2 = xs in
  if (is_nil match_tmp_me2)
  then ()
  else let match_tmp_me3 = match_tmp_me2 in
  if if (is_cons match_tmp_me3)
  then if true
  then true
  else false
  else false
  then let w = (f h) in
  ((iter f) tl)
  else false))
  
  let main = ((iter print_int) (((map (fun x -> ((( + ) x) 1))) (1::(2::(3::[])))) (fun x -> x)))
  $ ./match_elimination_runner.exe < manytests/typed/012fibcps.ml
  let rec fib = (fun n -> (fun k -> if ((( < ) n) 2)
  then (k n)
  else ((fib ((( - ) n) 1)) (fun a -> ((fib ((( - ) n) 2)) (fun b -> (k ((( + ) a) b))))))))
  
  let main = (print_int ((fib 6) (fun x -> x)))
  $ ./match_elimination_runner.exe < manytests/typed/013foldfoldr.ml
  let id = (fun x -> x)
  
  let rec fold_right = (fun f -> (fun acc -> (fun xs -> let match_tmp_me0 = xs in
  if (is_nil match_tmp_me0)
  then acc
  else let match_tmp_me1 = match_tmp_me0 in
  if if (is_cons match_tmp_me1)
  then if true
  then true
  else false
  else false
  then ((f h) (((fold_right f) acc) tl))
  else false)))
  
  let foldl = (fun f -> (fun a -> (fun bs -> ((((fold_right (fun b -> (fun g -> (fun x -> (g ((f x) b)))))) id) bs) a))))
  
  let main = (print_int (((foldl (fun x -> (fun y -> ((( * ) x) y)))) 1) (1::(2::(3::[])))))

  $ ./match_elimination_runner.exe < manytests/typed/015tuples.ml
  Fatal error: exception Failure("Only simple let bindings with 1 identifier are supported after alpha conversion")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.(>>=) in file "lib/anf/match_elimination.ml", line 34, characters 17-20
  Called from Fml_lib__Match_elimination.StateMonad.run in file "lib/anf/match_elimination.ml" (inlined), line 68, characters 18-23
  Called from Fml_lib__Match_elimination.match_elimination in file "lib/anf/match_elimination.ml", line 210, characters 2-150
  Called from Dune__exe__Match_elimination_runner in file "tests/match_elimination_runner.ml", line 24, characters 17-38
  [2]

  $ ./match_elimination_runner.exe < manytests/typed/016lists.ml
  let rec length = (fun xs -> let match_tmp_me0 = xs in
  if (is_nil match_tmp_me0)
  then 0
  else let match_tmp_me1 = match_tmp_me0 in
  if if (is_cons match_tmp_me1)
  then if true
  then true
  else false
  else false
  then ((( + ) 1) (length tl))
  else false)
  
  let length_tail = let rec helper = (fun acc -> (fun xs -> let match_tmp_me2 = xs in
  if (is_nil match_tmp_me2)
  then acc
  else let match_tmp_me3 = match_tmp_me2 in
  if if (is_cons match_tmp_me3)
  then if true
  then true
  else false
  else false
  then ((helper ((( + ) acc) 1)) tl)
  else false)) in
  (helper 0)
  
  let rec map = (fun f -> (fun xs -> let match_tmp_me4 = xs in
  if (is_nil match_tmp_me4)
  then []
  else let match_tmp_me5 = match_tmp_me4 in
  if if (is_cons match_tmp_me5)
  then if true
  then (is_nil (tl match_tmp_me5))
  else false
  else false
  then ((f a)::[])
  else let match_tmp_me6 = match_tmp_me5 in
  if if (is_cons match_tmp_me6)
  then if true
  then if (is_cons (tl match_tmp_me6))
  then if true
  then (is_nil (tl (tl match_tmp_me6)))
  else false
  else false
  else false
  else false
  then ((f a)::((f b)::[]))
  else let match_tmp_me7 = match_tmp_me6 in
  if if (is_cons match_tmp_me7)
  then if true
  then if (is_cons (tl match_tmp_me7))
  then if true
  then if (is_cons (tl (tl match_tmp_me7)))
  then if true
  then (is_nil (tl (tl (tl match_tmp_me7))))
  else false
  else false
  else false
  else false
  else false
  else false
  then ((f a)::((f b)::((f c)::[])))
  else let match_tmp_me8 = match_tmp_me7 in
  if if (is_cons match_tmp_me8)
  then if true
  then if (is_cons (tl match_tmp_me8))
  then if true
  then if (is_cons (tl (tl match_tmp_me8)))
  then if true
  then if (is_cons (tl (tl (tl match_tmp_me8))))
  then if true
  then true
  else false
  else false
  else false
  else false
  else false
  else false
  else false
  else false
  then ((f a)::((f b)::((f c)::((f d)::((map f) tl)))))
  else false))
  
  let rec append = (fun xs -> (fun ys -> let match_tmp_me9 = xs in
  if (is_nil match_tmp_me9)
  then ys
  else let match_tmp_me10 = match_tmp_me9 in
  if if (is_cons match_tmp_me10)
  then if true
  then true
  else false
  else false
  then (x::((append xs_ac0) ys))
  else false))
  
  let concat = let rec helper = (fun xs -> let match_tmp_me11 = xs in
  if (is_nil match_tmp_me11)
  then []
  else let match_tmp_me12 = match_tmp_me11 in
  if if (is_cons match_tmp_me12)
  then if true
  then true
  else false
  else false
  then ((append h) (helper tl))
  else false) in
  helper
  
  let rec iter = (fun f -> (fun xs -> let match_tmp_me13 = xs in
  if (is_nil match_tmp_me13)
  then ()
  else let match_tmp_me14 = match_tmp_me13 in
  if if (is_cons match_tmp_me14)
  then if true
  then true
  else false
  else false
  then let () = (f h) in
  ((iter f) tl)
  else false))
  
  let rec cartesian = (fun xs -> (fun ys -> let match_tmp_me15 = xs in
  if (is_nil match_tmp_me15)
  then []
  else let match_tmp_me16 = match_tmp_me15 in
  if if (is_cons match_tmp_me16)
  then if true
  then true
  else false
  else false
  then ((append ((map (fun a -> (h, a))) ys)) ((cartesian tl) ys))
  else false))
  
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


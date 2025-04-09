  $ ./lambda_lift_runner.exe << EOF
  > let f = let y x = x + 1 in y 3;;
  > EOF
  let lam_ll0 = (fun ( + ) x -> ((( + ) x) 1))
  let f = let y = (lam_ll0 ( + )) in
  (y 3)

  $ ./lambda_lift_runner.exe << EOF
  > let length xs = match xs with
  > | a::b::[] -> 2
  > | a::[] -> 1
  > | [] -> 0
  > EOF
  let lam_ll0 = (fun is_cons is_cons tl_list_get is_empty tl_list_get tl_list_get hd_list_get hd_list_get tl_list_get is_cons is_empty tl_list_get hd_list_get is_empty failwith "no matching" xs -> if if (is_cons xs)
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
  else (failwith "no matching"))
  let length = ((((((((((((((((lam_ll0 is_cons) is_cons) tl_list_get) is_empty) tl_list_get) tl_list_get) hd_list_get) hd_list_get) tl_list_get) is_cons) is_empty) tl_list_get) hd_list_get) is_empty) failwith) "no matching")

  $ ./lambda_lift_runner.exe << EOF
  > let is_empty x = x+1
  > let rec length xs = match xs with
  > | [] -> 0
  > | _::tl -> 1 + length xs
  > EOF
  let lam_ll0 = (fun ( + ) x -> ((( + ) x) 1))
  let is_empty_ac0 = (lam_ll0 ( + ))
  
  let rec lam_ll1 = (fun is_empty is_cons hd_list_get tl_list_get ( + ) length failwith "no matching" xs -> if (is_empty xs)
  then 0
  else if (is_cons xs)
  then let _ = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((( + ) 1) (length xs))
  else (failwith "no matching"))
  and length = ((((((((lam_ll1 is_empty) is_cons) hd_list_get) tl_list_get) ( + )) length) failwith) "no matching")

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
  let lam_ll5 = (fun k_ac1 ( * ) n_ac2 m -> (k_ac1 ((( * ) m) n_ac2)))
  let lam_ll4 = (fun k_ac1 ( * ) n_ac2 -> (((lam_ll5 k_ac1) ( * )) n_ac2))
  let lam_ll3 = (fun ( * ) k_ac1 -> ((lam_ll4 k_ac1) ( * )))
  let lam_ll2 = (fun ( <= ) n_ac0 fack ( - ) n_ac0 ( * ) n_ac0 k -> if ((( <= ) n_ac0) 1)
  then (k 1)
  else ((fack ((( - ) n_ac0) 1)) (((lam_ll3 ( * )) k) n_ac0)))
  let lam_ll1 = (fun ( <= ) fack ( - ) ( * ) n_ac0 -> (((((((lam_ll2 ( <= )) n_ac0) fack) ( - )) n_ac0) ( * )) n_ac0))
  let lam_ll6 = (fun x -> x)
  let lam_ll0 = (fun ( <= ) fack ( - ) ( * ) n -> let rec fack = ((((lam_ll1 ( <= )) fack) ( - )) ( * )) in
  ((fack n) lam_ll6))
  let fac = ((((lam_ll0 ( <= )) fack) ( - )) ( * ))

  $ ./lambda_lift_runner.exe << EOF
  > let f x = match x with
  > | 1 -> 12
  > | 12 -> 12
  > | _ -> 325
  > EOF
  let lam_ll0 = (fun (=) (=) failwith "no matching" x -> if (((=) x) 1)
  then 12
  else if (((=) x) 12)
  then 12
  else if true
  then 325
  else (failwith "no matching"))
  let f = ((((lam_ll0 (=)) (=)) failwith) "no matching")

  $ ./lambda_lift_runner.exe < manytests/typed/001fac.ml
  let rec lam_ll0 = (fun ( <= ) ( * ) fac ( - ) n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (fac ((( - ) n) 1))))
  and fac = ((((lam_ll0 ( <= )) ( * )) fac) ( - ))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/002fac.ml
  let rec lam_ll2 = (fun k ( * ) n p -> (k ((( * ) p) n)))
  and lam_ll1 = (fun ( = ) n fac_cps ( - ) n ( * ) n k -> if ((( = ) n) 1)
  then (k 1)
  else ((fac_cps ((( - ) n) 1)) (((lam_ll2 k) ( * )) n)))
  and lam_ll0 = (fun ( = ) fac_cps ( - ) ( * ) n -> (((((((lam_ll1 ( = )) n) fac_cps) ( - )) n) ( * )) n))
  and fac_cps = ((((lam_ll0 ( = )) fac_cps) ( - )) ( * ))
  
  let lam_ll3 = (fun print_int_ac0 -> print_int_ac0)
  let main = let () = (print_int ((fac_cps 4) lam_ll3)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/003fib.ml
  let rec lam_ll2 = (fun ( = ) b ( - ) ( + ) a b fib_acc b n -> if ((( = ) n) 1)
  then b
  else let n1 = ((( - ) n) 1) in
  let ab = ((( + ) a) b) in
  (((fib_acc b) ab) n1))
  and lam_ll1 = (fun ( = ) ( - ) ( + ) a fib_acc b -> ((((((((lam_ll2 ( = )) b) ( - )) ( + )) a) b) fib_acc) b))
  and lam_ll0 = (fun ( = ) ( - ) ( + ) fib_acc a -> (((((lam_ll1 ( = )) ( - )) ( + )) a) fib_acc))
  and fib_acc = ((((lam_ll0 ( = )) ( - )) ( + )) fib_acc)
  
  let rec lam_ll3 = (fun ( < ) ( + ) fib ( - ) fib ( - ) n -> if ((( < ) n) 2)
  then n
  else ((( + ) (fib ((( - ) n) 1))) (fib ((( - ) n) 2))))
  and fib = ((((((lam_ll3 ( < )) ( + )) fib) ( - )) fib) ( - ))
  
  let main = let () = (print_int (((fib_acc 0) 1) 4)) in
  let () = (print_int (fib 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/004manyargs.ml
  let lam_ll0 = (fun ( = ) f -> if ((( = ) 1) 1)
  then f
  else f)
  let wrap = (lam_ll0 ( = ))
  
  let lam_ll3 = (fun print_int a print_int b print_int c -> let a_ac0 = (print_int a) in
  let b_ac1 = (print_int b) in
  let c_ac2 = (print_int c) in
  0)
  let lam_ll2 = (fun print_int a print_int print_int b -> (((((lam_ll3 print_int) a) print_int) b) print_int))
  let lam_ll1 = (fun print_int print_int print_int a -> ((((lam_ll2 print_int) a) print_int) print_int))
  let test3 = (((lam_ll1 print_int) print_int) print_int)
  
  let lam_ll13 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  let lam_ll12 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a b c d e f g h i -> ((((((((((((((((((lam_ll13 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) a) b) c) d) e) f) g) h) i))
  let lam_ll11 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a b c d e f g h -> (((((((((((((((((lam_ll12 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) a) b) c) d) e) f) g) h))
  let lam_ll10 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a b c d e f g -> ((((((((((((((((lam_ll11 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) a) b) c) d) e) f) g))
  let lam_ll9 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a b c d e f -> (((((((((((((((lam_ll10 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) a) b) c) d) e) f))
  let lam_ll8 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a b c d e -> ((((((((((((((lam_ll9 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) a) b) c) d) e))
  let lam_ll7 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a b c d -> (((((((((((((lam_ll8 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) a) b) c) d))
  let lam_ll6 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a b c -> ((((((((((((lam_ll7 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) a) b) c))
  let lam_ll5 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a b -> (((((((((((lam_ll6 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) a) b))
  let lam_ll4 = (fun ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) ( + ) a -> ((((((((((lam_ll5 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) a))
  let test10 = (((((((((lam_ll4 ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + )) ( + ))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let () = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/005fix.ml
  let rec lam_ll1 = (fun f fix f x -> ((f (fix f)) x))
  and lam_ll0 = (fun fix f -> (((lam_ll1 f) fix) f))
  and fix = (lam_ll0 fix)
  
  let lam_ll3 = (fun ( <= ) ( * ) self ( - ) n -> if ((( <= ) n) 1)
  then 1
  else ((( * ) n) (self ((( - ) n) 1))))
  let lam_ll2 = (fun ( <= ) ( * ) ( - ) self -> ((((lam_ll3 ( <= )) ( * )) self) ( - )))
  let fac = (((lam_ll2 ( <= )) ( * )) ( - ))
  
  let main = let () = (print_int ((fix fac) 6)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/006partial.ml
  let lam_ll1 = (fun ( + ) foo -> ((( + ) foo) 2))
  let lam_ll2 = (fun ( * ) foo -> ((( * ) foo) 10))
  let lam_ll0 = (fun ( + ) ( * ) b -> if b
  then (lam_ll1 ( + ))
  else (lam_ll2 ( * )))
  let foo = ((lam_ll0 ( + )) ( * ))
  
  let lam_ll3 = (fun foo foo foo foo x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))))
  let foo_ac0 = ((((lam_ll3 foo) foo) foo) foo)
  
  let main = let () = (print_int (foo_ac0 11)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/006partial2.ml
  let lam_ll2 = (fun print_int a print_int b print_int ( + ) a ( * ) b c -> let () = (print_int a) in
  let () = (print_int b) in
  let () = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  let lam_ll1 = (fun print_int a print_int print_int ( + ) a ( * ) b -> (((((((((lam_ll2 print_int) a) print_int) b) print_int) ( + )) a) ( * )) b))
  let lam_ll0 = (fun print_int print_int print_int ( + ) ( * ) a -> (((((((lam_ll1 print_int) a) print_int) print_int) ( + )) a) ( * )))
  let foo = (((((lam_ll0 print_int) print_int) print_int) ( + )) ( * ))
  
  let main = let foo_ac0 = (foo 1) in
  let foo_ac1 = (foo_ac0 2) in
  let foo_ac2 = (foo_ac1 3) in
  let () = (print_int foo_ac2) in
  0
  $ ./lambda_lift_runner.exe < manytests/typed/006partial3.ml
  let lam_ll2 = (fun print_int c -> (print_int c))
  let lam_ll1 = (fun print_int print_int b -> let () = (print_int b) in
  (lam_ll2 print_int))
  let lam_ll0 = (fun print_int print_int print_int a -> let () = (print_int a) in
  ((lam_ll1 print_int) print_int))
  let foo = (((lam_ll0 print_int) print_int) print_int)
  
  let main = let () = (((foo 4) 8) 9) in
  0
  $ ./lambda_lift_runner.exe < manytests/typed/007order.ml
  let lam_ll8 = (fun print_int ( + ) a b print_int ( + ) ( / ) ( * ) a b _c d __ -> let () = (print_int ((( + ) a) b)) in
  let () = (print_int __) in
  ((( + ) ((( / ) ((( * ) a) b)) _c)) d))
  let lam_ll7 = (fun print_int ( + ) a b print_int ( + ) ( / ) ( * ) a b _c d -> ((((((((((((lam_ll8 print_int) ( + )) a) b) print_int) ( + )) ( / )) ( * )) a) b) _c) d))
  let lam_ll6 = (fun print_int ( + ) a b print_int ( + ) ( / ) ( * ) a b _c () -> (((((((((((lam_ll7 print_int) ( + )) a) b) print_int) ( + )) ( / )) ( * )) a) b) _c))
  let lam_ll5 = (fun print_int ( + ) a b print_int ( + ) ( / ) ( * ) a b _c -> (((((((((((lam_ll6 print_int) ( + )) a) b) print_int) ( + )) ( / )) ( * )) a) b) _c))
  let lam_ll4 = (fun print_int ( + ) a print_int ( + ) ( / ) ( * ) a b -> ((((((((((lam_ll5 print_int) ( + )) a) b) print_int) ( + )) ( / )) ( * )) a) b))
  let lam_ll3 = (fun print_int ( + ) a print_int ( + ) ( / ) ( * ) a () -> ((((((((lam_ll4 print_int) ( + )) a) print_int) ( + )) ( / )) ( * )) a))
  let lam_ll2 = (fun print_int ( + ) print_int ( + ) ( / ) ( * ) a -> ((((((((lam_ll3 print_int) ( + )) a) print_int) ( + )) ( / )) ( * )) a))
  let lam_ll1 = (fun print_int ( + ) print_int ( + ) ( / ) ( * ) () -> ((((((lam_ll2 print_int) ( + )) print_int) ( + )) ( / )) ( * )))
  let lam_ll0 = (fun print_int ( + ) print_int ( + ) ( / ) ( * ) () -> ((((((lam_ll1 print_int) ( + )) print_int) ( + )) ( / )) ( * )))
  let _start = ((((((lam_ll0 print_int) ( + )) print_int) ( + )) ( / )) ( * ))
  
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (( ~- ) 1))) 10000) (( ~- ) 555555)))
  $ ./lambda_lift_runner.exe < manytests/typed/008ascription.ml
  let lam_ll2 = (fun f g x -> ((f x) (g x)))
  let lam_ll1 = (fun f g -> ((lam_ll2 f) g))
  let lam_ll0 = (fun f -> (lam_ll1 f))
  let addi = lam_ll0
  
  let lam_ll4 = (fun ( + ) x ( * ) x b -> if b
  then ((( + ) x) 1)
  else ((( * ) x) 2))
  let lam_ll3 = (fun ( + ) ( * ) x -> ((((lam_ll4 ( + )) x) ( * )) x))
  let lam_ll5 = (fun ( = ) ( / ) _start -> ((( = ) ((( / ) _start) 2)) 0))
  let main = let () = (print_int (((addi ((lam_ll3 ( + )) ( * ))) ((lam_ll5 ( = )) ( / ))) 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/009let_poly.ml
  let lam_ll0 = (fun x -> x)
  let temp = let f = lam_ll0 in
  ((f 1), (f true))

  $ ./lambda_lift_runner.exe < manytests/typed/011mapcps.ml
  let rec lam_ll3 = (fun k f h tl_ac0 -> (k ((f h)::tl_ac0)))
  and lam_ll2 = (fun is_empty xs is_cons xs hd_list_get xs tl_list_get xs map f f failwith "no matching" k -> if (is_empty xs)
  then (k [])
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  (((map f) tl) (((lam_ll3 k) f) h))
  else (failwith "no matching"))
  and lam_ll1 = (fun is_empty is_cons hd_list_get tl_list_get map f f failwith "no matching" xs -> (((((((((((((lam_ll2 is_empty) xs) is_cons) xs) hd_list_get) xs) tl_list_get) xs) map) f) f) failwith) "no matching"))
  and lam_ll0 = (fun is_empty is_cons hd_list_get tl_list_get map failwith "no matching" f -> (((((((((lam_ll1 is_empty) is_cons) hd_list_get) tl_list_get) map) f) f) failwith) "no matching"))
  and map = (((((((lam_ll0 is_empty) is_cons) hd_list_get) tl_list_get) map) failwith) "no matching")
  
  let rec lam_ll5 = (fun is_empty is_cons hd_list_get tl_list_get f iter f failwith "no matching" xs -> if (is_empty xs)
  then ()
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  let w = (f h) in
  ((iter f) tl)
  else (failwith "no matching"))
  and lam_ll4 = (fun is_empty is_cons hd_list_get tl_list_get iter failwith "no matching" f -> (((((((((lam_ll5 is_empty) is_cons) hd_list_get) tl_list_get) f) iter) f) failwith) "no matching"))
  and iter = (((((((lam_ll4 is_empty) is_cons) hd_list_get) tl_list_get) iter) failwith) "no matching")
  
  let lam_ll6 = (fun ( + ) x -> ((( + ) x) 1))
  let lam_ll7 = (fun x -> x)
  let main = ((iter print_int) (((map (lam_ll6 ( + ))) (1::(2::(3::[])))) lam_ll7))
  $ ./lambda_lift_runner.exe < manytests/typed/012fibcps.ml
  let rec lam_ll3 = (fun k ( + ) a b -> (k ((( + ) a) b)))
  and lam_ll2 = (fun fib ( - ) n k ( + ) a -> ((fib ((( - ) n) 2)) (((lam_ll3 k) ( + )) a)))
  and lam_ll1 = (fun ( < ) n n fib ( - ) n fib ( - ) n ( + ) k -> if ((( < ) n) 2)
  then (k n)
  else ((fib ((( - ) n) 1)) (((((lam_ll2 fib) ( - )) n) k) ( + ))))
  and lam_ll0 = (fun ( < ) fib ( - ) fib ( - ) ( + ) n -> ((((((((((lam_ll1 ( < )) n) n) fib) ( - )) n) fib) ( - )) n) ( + )))
  and fib = ((((((lam_ll0 ( < )) fib) ( - )) fib) ( - )) ( + ))
  
  let lam_ll4 = (fun x -> x)
  let main = (print_int ((fib 6) lam_ll4))
  $ ./lambda_lift_runner.exe < manytests/typed/013foldfoldr.ml
  let lam_ll0 = (fun x -> x)
  let id = lam_ll0
  
  let rec lam_ll3 = (fun is_empty acc is_cons hd_list_get tl_list_get f fold_right f acc failwith "no matching" xs -> if (is_empty xs)
  then acc
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((f h) (((fold_right f) acc) tl))
  else (failwith "no matching"))
  and lam_ll2 = (fun is_empty is_cons hd_list_get tl_list_get f fold_right f failwith "no matching" acc -> (((((((((((lam_ll3 is_empty) acc) is_cons) hd_list_get) tl_list_get) f) fold_right) f) acc) failwith) "no matching"))
  and lam_ll1 = (fun is_empty is_cons hd_list_get tl_list_get fold_right failwith "no matching" f -> (((((((((lam_ll2 is_empty) is_cons) hd_list_get) tl_list_get) f) fold_right) f) failwith) "no matching"))
  and fold_right = (((((((lam_ll1 is_empty) is_cons) hd_list_get) tl_list_get) fold_right) failwith) "no matching")
  
  let lam_ll9 = (fun g f b x -> (g ((f x) b)))
  let lam_ll8 = (fun f b g -> (((lam_ll9 g) f) b))
  let lam_ll7 = (fun f b -> ((lam_ll8 f) b))
  let lam_ll6 = (fun fold_right f id a bs -> ((((fold_right (lam_ll7 f)) id) bs) a))
  let lam_ll5 = (fun fold_right f id a -> ((((lam_ll6 fold_right) f) id) a))
  let lam_ll4 = (fun fold_right id f -> (((lam_ll5 fold_right) f) id))
  let foldl = ((lam_ll4 fold_right) id)
  
  let lam_ll11 = (fun ( * ) x y -> ((( * ) x) y))
  let lam_ll10 = (fun ( * ) x -> ((lam_ll11 ( * )) x))
  let main = (print_int (((foldl (lam_ll10 ( * ))) 1) (1::(2::(3::[])))))

  $ ./lambda_lift_runner.exe < manytests/typed/015tuples.ml
  let rec lam_ll1 = (fun f fix f x -> ((f (fix f)) x))
  and lam_ll0 = (fun fix f -> (((lam_ll1 f) fix) f))
  and fix = (lam_ll0 fix)
  
  let lam_ll3 = (fun tuple_get tuple_get f f p -> let a = ((tuple_get p) 0) in
  let b = ((tuple_get p) 1) in
  ((f a), (f b)))
  let lam_ll2 = (fun tuple_get tuple_get f -> ((((lam_ll3 tuple_get) tuple_get) f) f))
  let map = ((lam_ll2 tuple_get) tuple_get)
  
  let lam_ll8 = (fun li self l_ac0 x -> ((li (self l_ac0)) x))
  let lam_ll7 = (fun self l_ac0 li -> (((lam_ll8 li) self) l_ac0))
  let lam_ll6 = (fun map self l_ac0 -> ((map ((lam_ll7 self) l_ac0)) l_ac0))
  let lam_ll5 = (fun map self -> ((lam_ll6 map) self))
  let lam_ll4 = (fun fix map l -> ((fix (lam_ll5 map)) l))
  let fixpoly = ((lam_ll4 fix) map)
  
  let lam_ll10 = (fun tuple_get p tuple_get p ( = ) ( - ) n -> let e = ((tuple_get p) 0) in
  let o = ((tuple_get p) 1) in
  if ((( = ) n) 0)
  then 1
  else (o ((( - ) n) 1)))
  let lam_ll9 = (fun tuple_get tuple_get ( = ) ( - ) p -> ((((((lam_ll10 tuple_get) p) tuple_get) p) ( = )) ( - )))
  let feven = ((((lam_ll9 tuple_get) tuple_get) ( = )) ( - ))
  
  let lam_ll12 = (fun tuple_get p tuple_get p ( = ) ( - ) n -> let e = ((tuple_get p) 0) in
  let o = ((tuple_get p) 1) in
  if ((( = ) n) 0)
  then 0
  else (e ((( - ) n) 1)))
  let lam_ll11 = (fun tuple_get tuple_get ( = ) ( - ) p -> ((((((lam_ll12 tuple_get) p) tuple_get) p) ( = )) ( - )))
  let fodd = ((((lam_ll11 tuple_get) tuple_get) ( = )) ( - ))
  
  let tie = (fixpoly (feven, fodd))
  
  let rec lam_ll13 = (fun ( = ) modd ( - ) n -> if ((( = ) n) 0)
  then 1
  else (modd ((( - ) n) 1)))
  and meven = (((lam_ll13 ( = )) modd) ( - ))
  and lam_ll14 = (fun ( = ) meven ( - ) n -> if ((( = ) n) 0)
  then 1
  else (meven ((( - ) n) 1)))
  and modd = (((lam_ll14 ( = )) meven) ( - ))
  
  let main = let () = (print_int (modd 1)) in
  let () = (print_int (meven 2)) in
  let even = ((tuple_get tie) 0) in
  let odd = ((tuple_get tie) 1) in
  let () = (print_int (odd 3)) in
  let () = (print_int (even 4)) in
  0

  $ ./lambda_lift_runner.exe < manytests/typed/016lists.ml
  let rec lam_ll0 = (fun is_empty is_cons hd_list_get tl_list_get ( + ) length failwith "no matching" xs -> if (is_empty xs)
  then 0
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((( + ) 1) (length tl))
  else (failwith "no matching"))
  and length = ((((((((lam_ll0 is_empty) is_cons) hd_list_get) tl_list_get) ( + )) length) failwith) "no matching")
  
  let lam_ll2 = (fun is_empty acc is_cons hd_list_get tl_list_get helper ( + ) acc failwith "no matching" xs -> if (is_empty xs)
  then acc
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((helper ((( + ) acc) 1)) tl)
  else (failwith "no matching"))
  let lam_ll1 = (fun is_empty is_cons hd_list_get tl_list_get helper ( + ) failwith "no matching" acc -> ((((((((((lam_ll2 is_empty) acc) is_cons) hd_list_get) tl_list_get) helper) ( + )) acc) failwith) "no matching"))
  let length_tail = let rec helper = ((((((((lam_ll1 is_empty) is_cons) hd_list_get) tl_list_get) helper) ( + )) failwith) "no matching") in
  (helper 0)
  
  let rec lam_ll4 = (fun is_empty is_cons is_empty tl_list_get hd_list_get f is_cons is_cons tl_list_get is_empty tl_list_get tl_list_get hd_list_get hd_list_get tl_list_get f f is_cons is_cons tl_list_get is_cons tl_list_get tl_list_get is_empty tl_list_get tl_list_get tl_list_get hd_list_get hd_list_get tl_list_get hd_list_get tl_list_get tl_list_get f f f is_cons is_cons tl_list_get is_cons tl_list_get tl_list_get is_cons tl_list_get tl_list_get tl_list_get hd_list_get hd_list_get tl_list_get hd_list_get tl_list_get tl_list_get hd_list_get tl_list_get tl_list_get tl_list_get tl_list_get tl_list_get tl_list_get tl_list_get f f f f map f failwith "no matching" xs -> if (is_empty xs)
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
  else (failwith "no matching"))
  and lam_ll3 = (fun is_empty is_cons is_empty tl_list_get hd_list_get is_cons is_cons tl_list_get is_empty tl_list_get tl_list_get hd_list_get hd_list_get tl_list_get is_cons is_cons tl_list_get is_cons tl_list_get tl_list_get is_empty tl_list_get tl_list_get tl_list_get hd_list_get hd_list_get tl_list_get hd_list_get tl_list_get tl_list_get is_cons is_cons tl_list_get is_cons tl_list_get tl_list_get is_cons tl_list_get tl_list_get tl_list_get hd_list_get hd_list_get tl_list_get hd_list_get tl_list_get tl_list_get hd_list_get tl_list_get tl_list_get tl_list_get tl_list_get tl_list_get tl_list_get tl_list_get map failwith "no matching" f -> ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((lam_ll4 is_empty) is_cons) is_empty) tl_list_get) hd_list_get) f) is_cons) is_cons) tl_list_get) is_empty) tl_list_get) tl_list_get) hd_list_get) hd_list_get) tl_list_get) f) f) is_cons) is_cons) tl_list_get) is_cons) tl_list_get) tl_list_get) is_empty) tl_list_get) tl_list_get) tl_list_get) hd_list_get) hd_list_get) tl_list_get) hd_list_get) tl_list_get) tl_list_get) f) f) f) is_cons) is_cons) tl_list_get) is_cons) tl_list_get) tl_list_get) is_cons) tl_list_get) tl_list_get) tl_list_get) hd_list_get) hd_list_get) tl_list_get) hd_list_get) tl_list_get) tl_list_get) hd_list_get) tl_list_get) tl_list_get) tl_list_get) tl_list_get) tl_list_get) tl_list_get) tl_list_get) f) f) f) f) map) f) failwith) "no matching"))
  and map = (((((((((((((((((((((((((((((((((((((((((((((((((((((((((lam_ll3 is_empty) is_cons) is_empty) tl_list_get) hd_list_get) is_cons) is_cons) tl_list_get) is_empty) tl_list_get) tl_list_get) hd_list_get) hd_list_get) tl_list_get) is_cons) is_cons) tl_list_get) is_cons) tl_list_get) tl_list_get) is_empty) tl_list_get) tl_list_get) tl_list_get) hd_list_get) hd_list_get) tl_list_get) hd_list_get) tl_list_get) tl_list_get) is_cons) is_cons) tl_list_get) is_cons) tl_list_get) tl_list_get) is_cons) tl_list_get) tl_list_get) tl_list_get) hd_list_get) hd_list_get) tl_list_get) hd_list_get) tl_list_get) tl_list_get) hd_list_get) tl_list_get) tl_list_get) tl_list_get) tl_list_get) tl_list_get) tl_list_get) tl_list_get) map) failwith) "no matching")
  
  let rec lam_ll6 = (fun is_empty xs is_cons xs hd_list_get xs tl_list_get xs append failwith "no matching" ys -> if (is_empty xs)
  then ys
  else if (is_cons xs)
  then let x = (hd_list_get xs) in
  let xs_ac0 = (tl_list_get xs) in
  (x::((append xs_ac0) ys))
  else (failwith "no matching"))
  and lam_ll5 = (fun is_empty is_cons hd_list_get tl_list_get append failwith "no matching" xs -> (((((((((((lam_ll6 is_empty) xs) is_cons) xs) hd_list_get) xs) tl_list_get) xs) append) failwith) "no matching"))
  and append = (((((((lam_ll5 is_empty) is_cons) hd_list_get) tl_list_get) append) failwith) "no matching")
  
  let lam_ll7 = (fun is_empty is_cons hd_list_get tl_list_get append helper failwith "no matching" xs -> if (is_empty xs)
  then []
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((append h) (helper tl))
  else (failwith "no matching"))
  let concat = let rec helper = ((((((((lam_ll7 is_empty) is_cons) hd_list_get) tl_list_get) append) helper) failwith) "no matching") in
  helper
  
  let rec lam_ll9 = (fun is_empty is_cons hd_list_get tl_list_get f iter f failwith "no matching" xs -> if (is_empty xs)
  then ()
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  let () = (f h) in
  ((iter f) tl)
  else (failwith "no matching"))
  and lam_ll8 = (fun is_empty is_cons hd_list_get tl_list_get iter failwith "no matching" f -> (((((((((lam_ll9 is_empty) is_cons) hd_list_get) tl_list_get) f) iter) f) failwith) "no matching"))
  and iter = (((((((lam_ll8 is_empty) is_cons) hd_list_get) tl_list_get) iter) failwith) "no matching")
  
  let rec lam_ll12 = (fun h a -> (h, a))
  and lam_ll11 = (fun is_empty xs is_cons xs hd_list_get xs tl_list_get xs append map cartesian failwith "no matching" ys -> if (is_empty xs)
  then []
  else if (is_cons xs)
  then let h = (hd_list_get xs) in
  let tl = (tl_list_get xs) in
  ((append ((map (lam_ll12 h)) ys)) ((cartesian tl) ys))
  else (failwith "no matching"))
  and lam_ll10 = (fun is_empty is_cons hd_list_get tl_list_get append map cartesian failwith "no matching" xs -> (((((((((((((lam_ll11 is_empty) xs) is_cons) xs) hd_list_get) xs) tl_list_get) xs) append) map) cartesian) failwith) "no matching"))
  and cartesian = (((((((((lam_ll10 is_empty) is_cons) hd_list_get) tl_list_get) append) map) cartesian) failwith) "no matching")
  
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


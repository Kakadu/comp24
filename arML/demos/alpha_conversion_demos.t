MANYTESTS
  $ ./start_alpha_conversion_demos.exe < manytests/typed/001fac.ml
  let rec fac n = (if (( <= ) n 1) then 1 else (( * ) n (fac (( - ) n 1))))
  
  let main = let () = (print_int (fac 4)) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/002fac.ml
  let rec fac_cps n k = (if (( = ) n 1) then (k 1) else (fac_cps (( - ) n 1) (ll_1 n k)))
  and ll_1 cc_1 cc_0 p = (cc_0 (( * ) p cc_1))
  
  let ll_0 ac_0 = ac_0
  
  let main = let () = (print_int (fac_cps 4 ll_0)) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n = (if (( = ) n 1) then b else let n1 = (( - ) n 1) in
  let ab = (( + ) a b) in
  (fib_acc b ab n1))
  
  let rec fib ac_0 = (if (( < ) ac_0 2) then ac_0 else (( + ) (fib (( - ) ac_0 1)) (fib (( - ) ac_0 2))))
  
  let main = let () = (print_int (fib_acc 0 1 4)) in
  let () = (print_int (fib 4)) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/004manyargs.ml
  let wrap f = (if (( = ) 1 1) then f else f)
  
  let test3 a b c = let ac_0 = (print_int a) in
  let ac_1 = (print_int b) inlet ac_2 = (print_int c) in
  0
  
  let test10 ac_6 ac_5 ac_4 d e ac_3 g h i j = (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) ac_6 ac_5) ac_4) d) e) ac_3) g) h) i) j)
  
  let main = let rez = (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000) in
  let () = (print_int rez) inlet temp2 = (wrap test3 1 10 100) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/005fix.ml
  let rec fix f x = (f (fix f) x)
  
  let fac self n = (if (( <= ) n 1) then 1 else (( * ) n (self (( - ) n 1))))
  
  let main = let () = (print_int (fix fac 6)) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/006partial.ml
  let ll_0 foo = (( + ) foo 2)
  
  let ll_1 ac_0 = (( * ) ac_0 10)
  
  let ac_1 b = (if b then ll_0 else ll_1)
  
  let ac_2 x = (ac_1 true (ac_1 false (ac_1 true (ac_1 false x))))
  
  let main = let () = (print_int (ac_2 11)) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/006partial2.ml
  let foo a b c = let () = (print_int a) inlet () = (print_int b) in
  let () = (print_int c) in
  (( + ) a (( * ) b c))
  
  let main = let ac_0 = (foo 1) inlet ac_1 = (ac_0 2) in
  let cc_0 = (ac_1 3) inlet () = (print_int cc_0) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/006partial3.ml
  let ll_1 c = (print_int c)
  
  let ll_0 b = let () = (print_int b) in
  ll_1
  
  let foo a = let () = (print_int a) in
  ll_0
  
  let main = let () = (foo 4 8 9) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/007order.ml
  let _start () () a () b _c () d __ = let () = (print_int (( + ) a b)) in
  let () = (print_int __) in
  (( + ) (( / ) (( * ) a b) _c) d)
  
  let main = (print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (U- 1)) 10000 (U- 555555)))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/008ascription.ml
  let addi f g x = ((f x ((g x) : bool)) : int)
  let ll_0 ac_0 b = (if b then (( + ) ac_0 1) else (( * ) ac_0 2))
  
  let ll_1 _start = (( = ) (( / ) _start 2) 0)
  
  let main = let () = (print_int (addi ll_0 ll_1 4)) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/009let_poly.ml
  let ll_0 x = x
  
  let temp = ((ll_0 1), (ll_0 true))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/015tuples.ml
  let rec fix f x = (f (fix f) x)
  
  let map ac_0 p = let a = (unpack_tuple p 0) in
  let b = (unpack_tuple p 1) in
  ((ac_0 a), (ac_0 b))
  
  let ll_1 cc_1 cc_0 li ac_1 = (li (cc_1 cc_0) ac_1)
  
  let ll_0 self l = (map (ll_1 self l) l)
  
  let fixpoly ac_2 = (fix ll_0 ac_2)
  
  let feven ac_3 n = let e = (unpack_tuple ac_3 0) in
  let o = (unpack_tuple ac_3 1) in
  (if (( = ) n 0) then 1 else (o (( - ) n 1)))
  
  let fodd ac_5 ac_4 = let e = (unpack_tuple ac_5 0) in
  let o = (unpack_tuple ac_5 1) in
  (if (( = ) ac_4 0) then 0 else (e (( - ) ac_4 1)))
  
  let tie = (fixpoly (feven, fodd))
  
  let rec meven ac_7 = (if (( = ) ac_7 0) then 1 else (modd (( - ) ac_7 1)))
  and modd ac_6 = (if (( = ) ac_6 0) then 1 else (meven (( - ) ac_6 1)))
  
  let main = let () = (print_int (modd 1)) in
  let () = (print_int (meven 2)) inlet even = (unpack_tuple tie 0) in
  let odd = (unpack_tuple tie 1) inlet () = (print_int (odd 3)) in
  let () = (print_int (even 4)) in
  0

  $ ./start_alpha_conversion_demos.exe < manytests/typed/016lists.ml
  let rec length xs = (if (equal (get_list_length xs) 0) then 0 else (if (less_than 0 (get_list_length xs)) then let h = (unpack_list_hd xs) in
  let tl = (unpack_list_tl xs) in
  (( + ) 1 (length tl)) else pattern_matching_failure))
  
  let rec ll_2 acc ac_0 = (if (equal (get_list_length ac_0) 0) then acc else (if (less_than 0 (get_list_length ac_0)) then let h = (unpack_list_hd ac_0) in
  let tl = (unpack_list_tl ac_0) in
  (ll_2 (( + ) acc 1) tl) else pattern_matching_failure))
  
  let length_tail = (ll_2 0)
  
  let rec map f ac_1 = (if (equal (get_list_length ac_1) 0) then [] else (if (logic_and (less_than 0 (get_list_length ac_1)) (equal (get_list_length (unpack_list_tl ac_1)) 0)) then let a = (unpack_list_hd ac_1) in
  ((f a) :: []) else (if (logic_and (less_than 1 (get_list_length ac_1)) (logic_and (less_than 0 (get_list_length (unpack_list_tl ac_1))) (equal (get_list_length (unpack_list_tl (unpack_list_tl ac_1))) 0))) then let a = (unpack_list_hd ac_1) in
  let b = (unpack_list_hd (unpack_list_tl ac_1)) in
  ((f a) :: ((f b) :: [])) else (if (logic_and (less_than 2 (get_list_length ac_1)) (logic_and (less_than 1 (get_list_length (unpack_list_tl ac_1))) (logic_and (less_than 0 (get_list_length (unpack_list_tl (unpack_list_tl ac_1)))) (equal (get_list_length (unpack_list_tl (unpack_list_tl (unpack_list_tl ac_1)))) 0)))) then let a = (unpack_list_hd ac_1) in
  let b = (unpack_list_hd (unpack_list_tl ac_1)) in
  let c = (unpack_list_hd (unpack_list_tl (unpack_list_tl ac_1))) in
  ((f a) :: ((f b) :: ((f c) :: []))) else (if (logic_and (less_than 3 (get_list_length ac_1)) (logic_and (less_than 2 (get_list_length (unpack_list_tl ac_1))) (logic_and (less_than 1 (get_list_length (unpack_list_tl (unpack_list_tl ac_1)))) (less_than 0 (get_list_length (unpack_list_tl (unpack_list_tl (unpack_list_tl ac_1)))))))) then let a = (unpack_list_hd ac_1) in
  let b = (unpack_list_hd (unpack_list_tl ac_1)) in
  let c = (unpack_list_hd (unpack_list_tl (unpack_list_tl ac_1))) in
  let d = (unpack_list_hd (unpack_list_tl (unpack_list_tl (unpack_list_tl ac_1)))) in
  let tl = (unpack_list_tl (unpack_list_tl (unpack_list_tl (unpack_list_tl ac_1)))) in
  ((f a) :: ((f b) :: ((f c) :: ((f d) :: (map f tl))))) else pattern_matching_failure)))))
  
  let rec append ac_2 ys = (if (equal (get_list_length ac_2) 0) then ys else (if (less_than 0 (get_list_length ac_2)) then let x = (unpack_list_hd ac_2) in
  let ac_3 = (unpack_list_tl ac_2) in
  (x :: (append ac_3 ys)) else pattern_matching_failure))
  
  let rec ll_1 ac_4 = (if (equal (get_list_length ac_4) 0) then [] else (if (less_than 0 (get_list_length ac_4)) then let h = (unpack_list_hd ac_4) in
  let tl = (unpack_list_tl ac_4) in
  (append h (ll_1 tl)) else pattern_matching_failure))
  
  let concat = ll_1
  
  let rec iter ac_6 ac_5 = (if (equal (get_list_length ac_5) 0) then () else (if (less_than 0 (get_list_length ac_5)) then let h = (unpack_list_hd ac_5) in
  let tl = (unpack_list_tl ac_5) inlet () = (ac_6 h) in
  (iter ac_6 tl) else pattern_matching_failure))
  
  let rec cartesian ac_8 ac_7 = (if (equal (get_list_length ac_8) 0) then [] else (if (less_than 0 (get_list_length ac_8)) then let h = (unpack_list_hd ac_8) in
  let tl = (unpack_list_tl ac_8) in
  (append (map (ll_0 h) ac_7) (cartesian tl ac_7)) else pattern_matching_failure))
  and ll_0 cc_0 a = (cc_0, a)
  
  let main = let () = (iter print_int (1 :: (2 :: (3 :: [])))) in
  let () = (print_int (length (cartesian (1 :: (2 :: [])) (1 :: (2 :: (3 :: (4 :: []))))))) in
  0
MANYTESTS
  $ ./start_alpha_conversion_demos.exe < manytests/typed/001fac.ml
  let rec fac n = (if (( <= ) n 1) then 1 else (( * ) n (fac (( - ) n 1))))
  let main = (let () = (print_int (fac 4)) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/002fac.ml
  let ll_0 ac_0 = ac_0
  let rec fac_cps n k = (if (( = ) n 1) then (k 1) else (fac_cps (( - ) n 1) (ll_1 n k))) and ll_1 cc_1 cc_0 p = (cc_0 (( * ) p cc_1))
  let main = (let () = (ac_0 (fac_cps 4 ll_0)) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n = (if (( = ) n 1) then b else (let n1 = (( - ) n 1) in (let ab = (( + ) a b) in (fib_acc b ab n1))))
  let rec fib ac_0 = (if (( < ) ac_0 2) then ac_0 else (( + ) (fib (( - ) ac_0 1)) (fib (( - ) ac_0 2))))
  let main = (let () = (print_int (fib_acc 0 1 4)) in (let ac_1 = (print_int (fib 4)) in 0))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/004manyargs.ml
  let wrap f = (if (( = ) 1 1) then f else f)
  let test3 a b c = (let ac_0 = (print_int ac_0) in (let ac_1 = (print_int ac_1) in (let ac_2 = (print_int ac_2) in 0)))
  let test10 ac_6 ac_5 ac_4 d e ac_3 g h i j = (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) ac_6 ac_5) ac_4) d) e) ac_3) g) h) i) j)
  let main = (let rez = (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000) in (let () = (print_int rez) in (let temp2 = (wrap test3 1 10 100) in 0)))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/005fix.ml
  let rec fix f x = (f (fix f) x)
  let fac self n = (if (( <= ) n 1) then 1 else (( * ) n (self (( - ) n 1))))
  let main = (let () = (print_int (fix fac 6)) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/006partial.ml
  let ll_0 foo = (( + ) foo 2)
  let ll_1 ac_0 = (( * ) ac_0 10)
  let ac_1 b = (if b then ll_0 else ll_1)
  let ac_2 x = (ac_2 true (ac_2 false (ac_2 true (ac_2 false x))))
  let main = (let () = (print_int (ac_2 11)) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/006partial2.ml
  let foo a b c = (let () = (print_int a) in (let ac_0 = (print_int b) in (let ac_1 = (print_int c) in (( + ) a (( * ) b c)))))
  let main = (let ac_2 = (ac_2 1) in (let ac_3 = (ac_3 2) in (let cc_0 = (ac_3 3) in (let ac_4 = (print_int cc_0) in 0))))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/006partial3.ml
  let ll_1 c = (print_int c)
  let ll_0 b = (let () = (print_int b) in ll_1)
  let foo a = (let ac_0 = (print_int a) in ll_0)
  let main = (let ac_1 = (foo 4 8 9) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/007order.ml
  let _start ac_2 ac_1 a ac_0 b _c () d __ = (let ac_3 = (print_int (( + ) a b)) in (let ac_4 = (print_int __) in (( + ) (( / ) (( * ) a b) _c) d)))
  let main = (print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (U- 1)) 10000 (U- 555555)))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/008ascription.ml
  let ll_0 x b = (if b then (( + ) x 1) else (( * ) x 2))
  let ll_1 _start = (( = ) (( / ) _start 2) 0)
  let addi f g ac_0 = (f ac_0 (g ac_0))
  let main = (let () = (print_int (addi ll_0 ll_1 4)) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/009let_poly.ml
  let ll_0 x = x
  let temp = ((ll_0 1), (ll_0 true))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/015tuples.ml
  let ll_1 cc_1 cc_0 li x = (li (cc_1 cc_0) x)
  let ll_0 self l = (map (ll_1 self l) l)
  let rec fix f ac_0 = (f (fix f) ac_0)
  let map ac_1 p = (let a = (unpack_tuple p 0) in (let b = (unpack_tuple p 1) in ((ac_1 a), (ac_1 b))))
  let fixpoly ac_2 = (fix ll_0 ac_2)
  let feven ac_3 n = (let e = (unpack_tuple ac_3 0) in (let o = (unpack_tuple ac_3 1) in (if (( == ) n 0) then 1 else (o (( - ) n 1)))))
  let fodd ac_5 ac_4 = (let ac_6 = (unpack_tuple ac_5 0) in (let ac_7 = (unpack_tuple ac_5 1) in (if (( == ) ac_4 0) then 0 else (ac_6 (( - ) ac_4 1)))))
  let tie = (fixpoly (feven, fodd))
  let rec meven ac_9 = (if (( = ) ac_9 0) then 1 else (modd (( - ) ac_9 1))) and modd ac_8 = (if (( = ) ac_8 0) then 1 else (meven (( - ) ac_8 1)))
  let main = (let () = (print_int (modd 1)) in (let ac_10 = (print_int (meven 2)) in (let even = (unpack_tuple tie 0) in (let odd = (unpack_tuple tie 1) in (let ac_11 = (print_int (odd 3)) in (let ac_12 = (print_int (even 4)) in 0))))))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/016lists.ml
  let rec ll_2 acc xs = (if (equal (get_list_length xs) 0) then acc else (if (less_than 0 (get_list_length xs)) then (let h = (unpack_list_hd xs) in (let tl = (unpack_list_tl xs) in (ll_2 (( + ) acc 1) tl))) else pattern_matching_failure))
  let rec ll_1 ac_0 = (if (equal (get_list_length ac_0) 0) then [] else (if (less_than 0 (get_list_length ac_0)) then (let ac_1 = (unpack_list_hd ac_0) in (let ac_2 = (unpack_list_tl ac_0) in (append ac_1 (ll_1 ac_2)))) else pattern_matching_failure))
  let rec length ac_3 = (if (equal (get_list_length ac_3) 0) then 0 else (if (less_than 0 (get_list_length ac_3)) then (let ac_4 = (unpack_list_hd ac_3) in (let ac_5 = (unpack_list_tl ac_3) in (( + ) 1 (length ac_5)))) else pattern_matching_failure))
  let length_tail = (ll_2 0)
  let rec map f ac_6 = (if (equal (get_list_length ac_6) 0) then [] else (if (logic_and (less_than 0 (get_list_length ac_6)) (equal (get_list_length (unpack_list_tl ac_6)) 0)) then (let a = (unpack_list_hd ac_6) in ((f ac_10) :: [])) else (if (logic_and (less_than 1 (get_list_length ac_6)) (logic_and (less_than 0 (get_list_length (unpack_list_tl ac_6))) (equal (get_list_length (unpack_list_tl (unpack_list_tl ac_6))) 0))) then (let ac_7 = (unpack_list_hd ac_6) in (let b = (unpack_list_hd (unpack_list_tl ac_6)) in ((f ac_7) :: ((f ac_11) :: [])))) else (if (logic_and (less_than 2 (get_list_length ac_6)) (logic_and (less_than 1 (get_list_length (unpack_list_tl ac_6))) (logic_and (less_than 0 (get_list_length (unpack_list_tl (unpack_list_tl ac_6)))) (equal (get_list_length (unpack_list_tl (unpack_list_tl (unpack_list_tl ac_6)))) 0)))) then (let ac_8 = (unpack_list_hd ac_6) in (let ac_9 = (unpack_list_hd (unpack_list_tl ac_6)) in (let c = (unpack_list_hd (unpack_list_tl (unpack_list_tl ac_6))) in ((f ac_8) :: ((f ac_9) :: ((f ac_12) :: [])))))) else (if (logic_and (less_than 3 (get_list_length ac_6)) (logic_and (less_than 2 (get_list_length (unpack_list_tl ac_6))) (logic_and (less_than 1 (get_list_length (unpack_list_tl (unpack_list_tl ac_6)))) (less_than 0 (get_list_length (unpack_list_tl (unpack_list_tl (unpack_list_tl ac_6)))))))) then (let ac_10 = (unpack_list_hd ac_6) in (let ac_11 = (unpack_list_hd (unpack_list_tl ac_6)) in (let ac_12 = (unpack_list_hd (unpack_list_tl (unpack_list_tl ac_6))) in (let d = (unpack_list_hd (unpack_list_tl (unpack_list_tl (unpack_list_tl ac_6)))) in (let ac_13 = (unpack_list_tl (unpack_list_tl (unpack_list_tl (unpack_list_tl ac_6)))) in ((f ac_10) :: ((f ac_11) :: ((f ac_12) :: ((f d) :: (map f ac_13)))))))))) else pattern_matching_failure)))))
  let rec append ac_14 ys = (if (equal (get_list_length ac_14) 0) then ys else (if (less_than 0 (get_list_length ac_14)) then (let x = (unpack_list_hd ac_14) in (let ac_15 = (unpack_list_tl ac_15) in (x :: (append ac_15 ys)))) else pattern_matching_failure))
  let concat = ll_1
  let rec iter ac_17 ac_16 = (if (equal (get_list_length ac_16) 0) then () else (if (less_than 0 (get_list_length ac_16)) then (let ac_18 = (unpack_list_hd ac_16) in (let ac_19 = (unpack_list_tl ac_16) in (let () = (ac_17 ac_18) in (iter ac_17 ac_19)))) else pattern_matching_failure))
  let rec cartesian ac_22 ac_21 = (if (equal (get_list_length ac_22) 0) then [] else (if (less_than 0 (get_list_length ac_22)) then (let ac_23 = (unpack_list_hd ac_22) in (let ac_24 = (unpack_list_tl ac_22) in (append (map (ll_0 ac_23) ac_21) (cartesian ac_24 ac_21)))) else pattern_matching_failure)) and ll_0 cc_0 ac_20 = (cc_0, ac_20)
  let main = (let ac_25 = (iter print_int (1 :: (2 :: (3 :: [])))) in (let ac_26 = (print_int (length (cartesian (1 :: (2 :: [])) (1 :: (2 :: (3 :: (4 :: []))))))) in 0))

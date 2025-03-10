MANYTESTS
  $ ./start_pm_elimination_demos.exe < manytests/typed/001fac.ml
  let rec fac n = (if (( <= ) n 1) then 1 else (( * ) n (fac (( - ) n 1))))
  let main = (let () = (print_int (fac 4)) in 0)

  $ ./start_pm_elimination_demos.exe < manytests/typed/002fac.ml
  let rec fac_cps n k = (if (( = ) n 1) then (k 1) else (fac_cps (( - ) n 1) (ll_1 n k))) and ll_1 cc_1 cc_0 p = (cc_0 (( * ) p cc_1))
  let ll_0 print_int = print_int
  let main = (let () = (print_int (fac_cps 4 ll_0)) in 0)

  $ ./start_pm_elimination_demos.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n = (if (( = ) n 1) then b else (let n1 = (( - ) n 1) in (let ab = (( + ) a b) in (fib_acc b ab n1))))
  let rec fib n = (if (( < ) n 2) then n else (( + ) (fib (( - ) n 1)) (fib (( - ) n 2))))
  let main = (let () = (print_int (fib_acc 0 1 4)) in (let () = (print_int (fib 4)) in 0))

  $ ./start_pm_elimination_demos.exe < manytests/typed/004manyargs.ml
  let wrap f = (if (( = ) 1 1) then f else f)
  let test3 a b c = (let a = (print_int a) in (let b = (print_int b) in (let c = (print_int c) in 0)))
  let test10 a b c d e f g h i j = (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) a b) c) d) e) f) g) h) i) j)
  let main = (let rez = (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000) in (let () = (print_int rez) in (let temp2 = (wrap test3 1 10 100) in 0)))

  $ ./start_pm_elimination_demos.exe < manytests/typed/005fix.ml
  let rec fix f x = (f (fix f) x)
  let fac self n = (if (( <= ) n 1) then 1 else (( * ) n (self (( - ) n 1))))
  let main = (let () = (print_int (fix fac 6)) in 0)

  $ ./start_pm_elimination_demos.exe < manytests/typed/006partial.ml
  let ll_0 foo = (( + ) foo 2)
  let ll_1 foo = (( * ) foo 10)
  let foo b = (if b then ll_0 else ll_1)
  let foo x = (foo true (foo false (foo true (foo false x))))
  let main = (let () = (print_int (foo 11)) in 0)

  $ ./start_pm_elimination_demos.exe < manytests/typed/006partial2.ml
  let foo a b c = (let () = (print_int a) in (let () = (print_int b) in (let () = (print_int c) in (( + ) a (( * ) b c)))))
  let main = (let foo = (foo 1) in (let foo = (foo 2) in (let cc_0 = (foo 3) in (let () = (print_int cc_0) in 0))))

  $ ./start_pm_elimination_demos.exe < manytests/typed/006partial3.ml
  let ll_1 c = (print_int c)
  let ll_0 b = (let () = (print_int b) in ll_1)
  let foo a = (let () = (print_int a) in ll_0)
  let main = (let () = (foo 4 8 9) in 0)

  $ ./start_pm_elimination_demos.exe < manytests/typed/007order.ml
  let _start () () a () b _c () d __ = (let () = (print_int (( + ) a b)) in (let () = (print_int __) in (( + ) (( / ) (( * ) a b) _c) d)))
  let main = (print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (U- 1)) 10000 (U- 555555)))

  $ ./start_pm_elimination_demos.exe < manytests/typed/008ascription.ml
  let addi f g x = ((f x ((g x) : bool)) : int)
  let ll_0 x b = (if b then (( + ) x 1) else (( * ) x 2))
  let ll_1 _start = (( = ) (( / ) _start 2) 0)
  let main = (let () = (print_int (addi ll_0 ll_1 4)) in 0)

  $ ./start_pm_elimination_demos.exe < manytests/typed/009let_poly.ml
  let ll_0 x = x
  let temp = ((ll_0 1), (ll_0 true))

  $ ./start_pm_elimination_demos.exe < manytests/typed/015tuples.ml
  let rec fix f x = (f (fix f) x)
  let map f p = (let a = (unpack_tuple p 0) in (let b = (unpack_tuple p 1) in ((f a), (f b))))
  let ll_1 cc_1 cc_0 li x = (li (cc_1 cc_0) x)
  let ll_0 self l = (map (ll_1 self l) l)
  let fixpoly l = (fix ll_0 l)
  let feven p n = (let e = (unpack_tuple p 0) in (let o = (unpack_tuple p 1) in (if (( = ) n 0) then 1 else (o (( - ) n 1)))))
  let fodd p n = (let e = (unpack_tuple p 0) in (let o = (unpack_tuple p 1) in (if (( = ) n 0) then 0 else (e (( - ) n 1)))))
  let tie = (fixpoly (feven, fodd))
  let rec meven n = (if (( = ) n 0) then 1 else (modd (( - ) n 1))) and modd n = (if (( = ) n 0) then 1 else (meven (( - ) n 1)))
  let main = (let () = (print_int (modd 1)) in (let () = (print_int (meven 2)) in (let even = (unpack_tuple tie 0) in (let odd = (unpack_tuple tie 1) in (let () = (print_int (odd 3)) in (let () = (print_int (even 4)) in 0))))))

  $ ./start_pm_elimination_demos.exe < manytests/typed/016lists.ml
  let rec length xs = (if (equal (get_list_length xs) 0) then 0 else (if (less_than 0 (get_list_length xs)) then (let h = (unpack_list_hd xs) in (let tl = (unpack_list_tl xs) in (( + ) 1 (length tl)))) else pattern_matching_failure))
  let rec ll_2 acc xs = (if (equal (get_list_length xs) 0) then acc else (if (less_than 0 (get_list_length xs)) then (let h = (unpack_list_hd xs) in (let tl = (unpack_list_tl xs) in (ll_2 (( + ) acc 1) tl))) else pattern_matching_failure))
  let length_tail = (ll_2 0)
  let rec map f xs = (if (equal (get_list_length xs) 0) then [] else (if (logic_and (less_than 0 (get_list_length xs)) (equal (get_list_length (unpack_list_tl xs)) 0)) then (let a = (unpack_list_hd xs) in ((f a) :: [])) else (if (logic_and (less_than 1 (get_list_length xs)) (logic_and (less_than 0 (get_list_length (unpack_list_tl xs))) (equal (get_list_length (unpack_list_tl (unpack_list_tl xs))) 0))) then (let a = (unpack_list_hd xs) in (let b = (unpack_list_hd (unpack_list_tl xs)) in ((f a) :: ((f b) :: [])))) else (if (logic_and (less_than 2 (get_list_length xs)) (logic_and (less_than 1 (get_list_length (unpack_list_tl xs))) (logic_and (less_than 0 (get_list_length (unpack_list_tl (unpack_list_tl xs)))) (equal (get_list_length (unpack_list_tl (unpack_list_tl (unpack_list_tl xs)))) 0)))) then (let a = (unpack_list_hd xs) in (let b = (unpack_list_hd (unpack_list_tl xs)) in (let c = (unpack_list_hd (unpack_list_tl (unpack_list_tl xs))) in ((f a) :: ((f b) :: ((f c) :: [])))))) else (if (logic_and (less_than 3 (get_list_length xs)) (logic_and (less_than 2 (get_list_length (unpack_list_tl xs))) (logic_and (less_than 1 (get_list_length (unpack_list_tl (unpack_list_tl xs)))) (less_than 0 (get_list_length (unpack_list_tl (unpack_list_tl (unpack_list_tl xs)))))))) then (let a = (unpack_list_hd xs) in (let b = (unpack_list_hd (unpack_list_tl xs)) in (let c = (unpack_list_hd (unpack_list_tl (unpack_list_tl xs))) in (let d = (unpack_list_hd (unpack_list_tl (unpack_list_tl (unpack_list_tl xs)))) in (let tl = (unpack_list_tl (unpack_list_tl (unpack_list_tl (unpack_list_tl xs)))) in ((f a) :: ((f b) :: ((f c) :: ((f d) :: (map f tl)))))))))) else pattern_matching_failure)))))
  let rec append xs ys = (if (equal (get_list_length xs) 0) then ys else (if (less_than 0 (get_list_length xs)) then (let x = (unpack_list_hd xs) in (let xs = (unpack_list_tl xs) in (x :: (append xs ys)))) else pattern_matching_failure))
  let rec ll_1 xs = (if (equal (get_list_length xs) 0) then [] else (if (less_than 0 (get_list_length xs)) then (let h = (unpack_list_hd xs) in (let tl = (unpack_list_tl xs) in (append h (ll_1 tl)))) else pattern_matching_failure))
  let concat = ll_1
  let rec iter f xs = (if (equal (get_list_length xs) 0) then () else (if (less_than 0 (get_list_length xs)) then (let h = (unpack_list_hd xs) in (let tl = (unpack_list_tl xs) in (let () = (f h) in (iter f tl)))) else pattern_matching_failure))
  let rec cartesian xs ys = (if (equal (get_list_length xs) 0) then [] else (if (less_than 0 (get_list_length xs)) then (let h = (unpack_list_hd xs) in (let tl = (unpack_list_tl xs) in (append (map (ll_0 h) ys) (cartesian tl ys)))) else pattern_matching_failure)) and ll_0 cc_0 a = (cc_0, a)
  let main = (let () = (iter print_int (1 :: (2 :: (3 :: [])))) in (let () = (print_int (length (cartesian (1 :: (2 :: [])) (1 :: (2 :: (3 :: (4 :: []))))))) in 0))

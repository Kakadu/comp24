MANYTESTS
  $ ./start_alpha_conversion_demos.exe < manytests/typed/001fac.ml
  let rec fac n = (if (( <= ) n 1) then 1 else (( * ) n (fac (( - ) n 1))))
  let main = (let () = (print_int (fac 4)) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/002fac.ml
  let rec fac_cps n k = (if (( = ) n 1) then (k 1) else (fac_cps (( - ) n 1) (ll_1 n k))) and ll_1 cc_1 cc_0 p = (cc_0 (( * ) p cc_1))
  let ll_0 ac_0 = ac_0
  let main = (let () = (print_int (fac_cps 4 ll_0)) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n = (if (( = ) n 1) then b else (let n1 = (( - ) n 1) in (let ab = (( + ) a b) in (fib_acc b ab n1))))
  let rec fib ac_0 = (if (( < ) ac_0 2) then ac_0 else (( + ) (fib (( - ) ac_0 1)) (fib (( - ) ac_0 2))))
  let main = (let () = (print_int (fib_acc 0 1 4)) in (let () = (print_int (fib 4)) in 0))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/004manyargs.ml
  let wrap f = (if (( = ) 1 1) then f else f)
  let test3 a b c = (let ac_0 = (print_int a) in (let ac_1 = (print_int b) in (let ac_2 = (print_int c) in 0)))
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
  let ac_2 x = (ac_1 true (ac_1 false (ac_1 true (ac_1 false x))))
  let main = (let () = (print_int (ac_2 11)) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/006partial2.ml
  let foo a b c = (let () = (print_int a) in (let () = (print_int b) in (let () = (print_int c) in (( + ) a (( * ) b c)))))
  let main = (let ac_0 = (foo 1) in (let ac_1 = (ac_0 2) in (let cc_0 = (ac_1 3) in (let () = (print_int cc_0) in 0))))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/006partial3.ml
  let ll_1 c = (print_int c)
  let ll_0 b = (let () = (print_int b) in ll_1)
  let foo a = (let () = (print_int a) in ll_0)
  let main = (let () = (foo 4 8 9) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/007order.ml
  let _start () () a () b _c () d __ = (let () = (print_int (( + ) a b)) in (let () = (print_int __) in (( + ) (( / ) (( * ) a b) _c) d)))
  let main = (print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (U- 1)) 10000 (U- 555555)))

  $ ./start_alpha_conversion_demos.exe < manytests/typed/008ascription.ml
  let addi f g x = ((f x ((g x) : bool)) : int)
  let ll_0 ac_0 b = (if b then (( + ) ac_0 1) else (( * ) ac_0 2))
  let ll_1 _start = (( = ) (( / ) _start 2) 0)
  let main = (let () = (print_int (addi ll_0 ll_1 4)) in 0)

  $ ./start_alpha_conversion_demos.exe < manytests/typed/009let_poly.ml
  let ll_0 x = x
  let temp = ((ll_0 1), (ll_0 true))

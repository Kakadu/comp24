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

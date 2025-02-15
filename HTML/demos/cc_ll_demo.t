  $ ./cc_ll_demo.exe < manytests/do_not_type/001.ml
  let recfac n = if (n <= 1) then 1 else (n * fac (n - 1))

  $ ./cc_ll_demo.exe < manytests/do_not_type/002if.ml
  let main = if true then 1 else false

  $ ./cc_ll_demo.exe < manytests/do_not_type/003occurs.ml
  let fun-3 f x = (f (fun-2 x));;
  let fun-2 x f = ((x x) f);;
  let fun-1 f x = (f (fun-0 x));;
  let fun-0 x f = ((x x) f);;
  let fix f = ((fun-1 f) (fun-3 f))

  $ ./cc_ll_demo.exe < manytests/do_not_type/004let_poly.ml
  let fun-1 x = x;;
  let fun-0 f = ((f 1), (f true));;
  let temp = (fun-0 fun-1)

  $ ./cc_ll_demo.exe < manytests/do_not_type/015tuples.ml
  let rec (a, b) = (a, b)

PASS
  $ ./cc_ll_demo.exe < manytests/typed/001fac.ml
  let rec fac n = if (n <= 1) then 1 else (n * fac (n - 1));;
  let main = let () = (print_int (fac 4))
  in 0

NO LAMBDA LIFT????? fun print_int -> print_int
  $ ./cc_ll_demo.exe < manytests/typed/002fac.ml
  let fun-1 print_int = print_int;;
  let fun-0 n k p = k (p * n);;
  let rec fac_cps n k = if (n = 1) then (k 1) else (fac_cps (n - 1) ((fun-0 n) k));;
  let main = let () = (print_int ((fac_cps 4) fun-1))
  in 0

PASS
  $ ./cc_ll_demo.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n = if (n = 1) then b else let n1 = (n - 1)
  in let ab = (a + b)
  in (((fib_acc b) ab) n1);;
  let rec fib n = if (n < 2) then n else (fib (n - 1) + fib (n - 2));;
  let main = let () = (print_int (((fib_acc 0) 1) 4))
  in let () = (print_int (fib 4))
  in 0

PASS
  $ ./cc_ll_demo.exe < manytests/typed/004manyargs.ml
  let wrap f = if (1 = 1) then f else f;;
  let test3 a b c = let a = (print_int a)
  in let b = (print_int b)
  in let c = (print_int c)
  in 0;;
  let test10 a b c d e f g h i j = (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j);;
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
  in let () = (print_int rez)
  in let temp2 = ((((wrap test3) 1) 10) 100)
  in 0

PASS
  $ ./cc_ll_demo.exe < manytests/typed/005fix.ml
  let rec fix f x = ((f (fix f)) x);;
  let fac self n = if (n <= 1) then 1 else (n * self (n - 1));;
  let main = let () = (print_int ((fix fac) 6))
  in 0

LAMBDA LIFT???
  $ ./cc_ll_demo.exe < manytests/typed/006partial.ml
  let fun-1 foo = (foo * 10);;
  let fun-0 foo = (foo + 2);;
  let foo b = if b then fun-0 else fun-1;;
  let foo x = ((foo true) ((foo false) ((foo true) ((foo false) x))));;
  let main = let () = (print_int (foo 11))
  in 0

PASS
  $ ./cc_ll_demo.exe < manytests/typed/006partial2.ml
  let foo a b c = let () = (print_int a)
  in let () = (print_int b)
  in let () = (print_int c)
  in (a + (b * c));;
  let main = let foo = (foo 1)
  in let foo = (foo 2)
  in let foo = (foo 3)
  in let () = (print_int foo)
  in 0

DON'T LIKE THIS OUTPUT, SHOULD BE 
let  ll_1 print_int c  = (print_int c)
let  ll_0 print_int b  = (let  () = (print_int b) in (ll_1 print_int))
let  foo a  = (let  () = (print_int a) in (ll_0 print_int))
let  main  = (let  () = (((foo 4) 8) 9) in 0)
  $ ./cc_ll_demo.exe < manytests/typed/006partial3.ml
  let fun-1 b = let () = (print_int b)
  in fun-0;;
  let fun-0 c = (print_int c);;
  let foo a = let () = (print_int a)
  in fun-1;;
  let main = let () = (((foo 4) 8) 9)
  in 0


  $ ./cc_ll_demo.exe < manytests/typed/007order.ml
  let _start () () a () b _c () d __ = let () = print_int (a + b)
  in let () = (print_int __)
  in (((a * b) / _c) + d);;
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))

NO LAMBDA LIFT AS WELL
  $ ./cc_ll_demo.exe < manytests/typed/008ascription.ml
  let fun-1 _start = ((_start / 2) = 0);;
  let fun-0 x = if b then (x + 1) else (x * 2);;
  let addi f g x = (((f x) ((g x) : bool)) : int);;
  let main = let () = (print_int (((addi fun-0) fun-1) 4))
  in 0
PASS
  $ ./cc_ll_demo.exe < manytests/typed/009let_poly.ml
  let fun-0 x = x;;
  let temp = let f = fun-0
  in ((f 1), (f true))

  $ ./cc_ll_demo.exe < manytests/typed/015tuples.ml
  Fatal error: exception Failure("todo")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Anf__Cc.closure_convert_decl_list.helper in file "lib/anf/cc.ml", line 289, characters 28-61
  Called from Anf__Cc.closure_convert_decl_list.helper in file "lib/anf/cc.ml", line 290, characters 17-37
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 27, characters 21-27
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 26, characters 20-23
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 27, characters 21-27
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 26, characters 20-23
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 27, characters 21-27
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 26, characters 20-23
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 27, characters 21-27
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 26, characters 20-23
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 27, characters 21-27
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 26, characters 20-23
  Called from Anf__Cc.CounterWriterMonad.bind in file "lib/anf/cc.ml", line 27, characters 21-27
  Called from Anf__Cc.closure_convert in file "lib/anf/cc.ml", line 298, characters 24-67
  Called from Dune__exe__Cc_ll_demo.cc_ll_test in file "demos/cc_ll_demo.ml", line 4, characters 15-44
  [2]

  $ ./cc_ll_demo.exe < manytests/typed/016lists.ml
  let fun-2 h a = (h, a);;
  let fun-1 append xs = match xs with
  | [] -> []
  | h :: tl -> ((append h) (helper tl));;
  let fun-0 acc = match xs with
  | [] -> acc
  | h :: tl -> (helper (acc + 1) tl);;
  let rec length xs = match xs with
  | [] -> 0
  | h :: tl -> (1 + (length tl));;
  let length_tail = let rec helper = fun-0
  in (helper 0);;
  let rec map f xs = match xs with
  | [] -> []
  | a :: [] -> (f a) :: []
  | a :: b :: [] -> (f a) :: (f b) :: []
  | a :: b :: c :: [] -> (f a) :: (f b) :: (f c) :: []
  | a :: b :: c :: d :: tl -> (f a) :: (f b) :: (f c) :: (f d) :: ((map f) tl);;
  let rec append xs ys = match xs with
  | [] -> ys
  | x :: xs -> x :: ((append xs) ys);;
  let concat = let rec helper = (fun-1 append)
  in helper;;
  let rec iter f xs = match xs with
  | [] -> ()
  | h :: tl -> let () = (f h)
  in ((iter f) tl);;
  let rec cartesian xs ys = match xs with
  | [] -> []
  | h :: tl -> ((append ((map (fun-2 h)) ys)) ((cartesian tl) ys));;
  let main = let () = ((iter print_int) 1 :: 2 :: 3 :: [])
  in let () = (print_int (length ((cartesian 1 :: 2 :: []) 1 :: 2 :: 3 :: 4 :: [])))
  in 0


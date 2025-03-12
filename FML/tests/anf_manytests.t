  $ ./anf_runner.exe << EOF
  > let fac n =
  > let rec fack n k =
  > if n<=1 then k 1
  > else fack (n - 1) ((fun k n m -> k (m * n)) k n)
  > in
  > fack n (fun x -> x)
  > EOF
  let a4 a1 a2 m = let a6 = ( * ) m a2 in
  a1 a6
  
  let rec a3 a0 k = let a7 = ( <= ) a0 1 in
  if a7
  then k 1
  else let a9 = a4 k a0 in
  let a8 = ( - ) a0 1 in
  a3 a8 a9
  
  let a5 x = x
  
  let fac n = a3 n a5
  Типы после приведения в ANF:
  val a4 : (int -> 'a) -> int -> int -> 'a
  val a3 : int -> (int -> 'a) -> 'a
  val a5 : 'a -> 'a
  val fac : int -> int
  $ ./anf_runner.exe < manytests/typed/001fac.ml
  let rec fac n = let a0 = ( <= ) n 1 in
  if a0
  then 1
  else let a2 = ( - ) n 1 in
  let a1 = fac a2 in
  ( * ) n a1
  
  let main = let a3 = fac 4 in
  let () = print_int a3 in
  0
  Типы после приведения в ANF:
  val fac : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/002fac.ml
  let a1 k n p = let a3 = ( * ) p n in
  k a3
  
  let rec fac_cps n k = let a4 = ( = ) n 1 in
  if a4
  then k 1
  else let a6 = a1 k n in
  let a5 = ( - ) n 1 in
  fac_cps a5 a6
  
  let a2 a0 = a0
  
  let main = let a7 = fac_cps 4 a2 in
  let () = print_int a7 in
  0
  Типы после приведения в ANF:
  val a1 : (int -> 'a) -> int -> int -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val a2 : 'a -> 'a
  val main : int

  $ ./anf_runner.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n = let a0 = ( = ) n 1 in
  if a0
  then b
  else let n1 = ( - ) n 1 in
  let ab = ( + ) a b in
  fib_acc b ab n1
  
  let rec fib n = let a1 = ( < ) n 2 in
  if a1
  then n
  else let a5 = ( - ) n 2 in
  let a4 = fib a5 in
  let a3 = ( - ) n 1 in
  let a2 = fib a3 in
  ( + ) a2 a4
  
  let main = let a6 = fib_acc 0 1 4 in
  let () = print_int a6 in
  let a7 = fib 4 in
  let () = print_int a7 in
  0
  Типы после приведения в ANF:
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/004manyargs.ml
  let wrap f = let a3 = ( = ) 1 1 in
  if a3
  then f
  else f
  
  let test3 a b c = let a0 = print_int a in
  let a1 = print_int b in
  let a2 = print_int c in
  0
  
  let test10 a b c d e f g h i j = let a11 = ( + ) a b in
  let a10 = ( + ) a11 c in
  let a9 = ( + ) a10 d in
  let a8 = ( + ) a9 e in
  let a7 = ( + ) a8 f in
  let a6 = ( + ) a7 g in
  let a5 = ( + ) a6 h in
  let a4 = ( + ) a5 i in
  ( + ) a4 j
  
  let main = let rez = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0
  Типы после приведения в ANF:
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/005fix.ml
  let rec fix f x = let a0 = fix f in
  f a0 x
  
  let fac self n = let a1 = ( <= ) n 1 in
  if a1
  then 1
  else let a3 = ( - ) n 1 in
  let a2 = self a3 in
  ( * ) n a2
  
  let main = let a4 = fix fac 6 in
  let () = print_int a4 in
  0
  Типы после приведения в ANF:
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/006partial.ml
  let a1 foo = ( + ) foo 2
  
  let a2 foo = ( * ) foo 10
  
  let foo b = if b
  then a1
  else a2
  
  let a0 x = let a5 = foo false x in
  let a4 = foo true a5 in
  let a3 = foo false a4 in
  foo true a3
  
  let main = let a6 = a0 11 in
  let () = print_int a6 in
  0
  Типы после приведения в ANF:
  val a1 : int -> int
  val a2 : int -> int
  val foo : bool -> int -> int
  val a0 : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/006partial2.ml
  let foo a b c = let () = print_int a in
  let () = print_int b in
  let () = print_int c in
  let a3 = ( * ) b c in
  ( + ) a a3
  
  let main = let a0 = foo 1 in
  let a1 = a0 2 in
  let a2 = a1 3 in
  let () = print_int a2 in
  0
  Типы после приведения в ANF:
  val foo : int -> int -> int -> int
  val main : int
  $ ./anf_runner.exe < manytests/typed/006partial3.ml
  let a1 c = print_int c
  
  let a0 b = let () = print_int b in
  a1
  
  let foo a = let () = print_int a in
  a0
  
  let main = let () = foo 4 8 9 in
  0
  Типы после приведения в ANF:
  val a1 : int -> unit
  val a0 : int -> int -> unit
  val foo : int -> int -> int -> unit
  val main : int
  $ ./anf_runner.exe < manytests/typed/007order.ml
  let _start a0 a1 a a2 b _c a3 d __ = let a4 = (a0, a1, a2, a3) in
  let a5 = ( + ) a b in
  let () = print_int a5 in
  let () = print_int __ in
  let a7 = ( * ) a b in
  let a6 = ( / ) a7 _c in
  ( + ) a6 d
  
  let main = let a14 = ( ~- ) 555555 in
  let a13 = ( ~- ) 1 in
  let a12 = print_int a13 in
  let a11 = print_int 4 in
  let a10 = print_int 2 in
  let a9 = print_int 1 in
  let a8 = _start a9 a10 3 a11 100 1000 a12 10000 a14 in
  print_int a8
  Типы после приведения в ANF:
  val _start : 'a -> 'b -> int -> 'c -> int -> int -> 'd -> int -> int -> int
  val main : unit
  $ ./anf_runner.exe < manytests/typed/008ascription.ml
  let addi f g x = let a2 = g x in
  f x a2
  
  let a0 x b = if b
  then ( + ) x 1
  else ( * ) x 2
  
  let a1 _start = let a3 = ( / ) _start 2 in
  ( = ) a3 0
  
  let main = let a4 = addi a0 a1 4 in
  let () = print_int a4 in
  0
  Типы после приведения в ANF:
  val addi : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val a0 : int -> bool -> int
  val a1 : int -> bool
  val main : int

  $ ./anf_runner.exe < manytests/typed/009let_poly.ml
  let a0 x = x
  
  let temp = let a1 = a0 1 in
  let a2 = a0 true in
  (a1, a2)
  Типы после приведения в ANF:
  val a0 : 'a -> 'a
  val temp : int * bool

  $ ./anf_runner.exe < manytests/typed/012fibcps.ml
  let a1 a k b = let a3 = ( + ) a b in
  k a3
  
  let a0 fib k n a = let a5 = a1 a k in
  let a4 = ( - ) n 2 in
  fib a4 a5
  
  let rec fib n k = let a6 = ( < ) n 2 in
  if a6
  then k n
  else let a8 = a0 fib k n in
  let a7 = ( - ) n 1 in
  fib a7 a8
  
  let a2 x = x
  
  let main = let a9 = fib 6 a2 in
  print_int a9
  Типы после приведения в ANF:
  val a1 : int -> (int -> 'a) -> int -> 'a
  val a0 : (int -> (int -> 'a) -> 'b) -> (int -> 'a) -> int -> int -> 'b
  val fib : int -> (int -> 'a) -> 'a
  val a2 : 'a -> 'a
  val main : unit

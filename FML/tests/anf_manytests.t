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
  ;;
  
  let rec a3 a0 k = let a7 = ( <= ) a0 1 in
  if a7
  then k 1
  else let a9 = a4 k a0 in
  let a8 = ( - ) a0 1 in
  a3 a8 a9
  ;;
  
  let a5 x = x
  ;;
  
  let fac n = a3 n a5
  ;;
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
  ;;
  
  let main = let a3 = fac 4 in
  let () = print_int a3 in
  0
  ;;
  Типы после приведения в ANF:
  val fac : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/002fac.ml
  let a1 k n p = let a3 = ( * ) p n in
  k a3
  ;;
  
  let rec fac_cps n k = let a4 = ( = ) n 1 in
  if a4
  then k 1
  else let a6 = a1 k n in
  let a5 = ( - ) n 1 in
  fac_cps a5 a6
  ;;
  
  let a2 a0 = a0
  ;;
  
  let main = let a7 = fac_cps 4 a2 in
  let () = print_int a7 in
  0
  ;;
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
  ;;
  
  let rec fib n = let a1 = ( < ) n 2 in
  if a1
  then n
  else let a5 = ( - ) n 2 in
  let a4 = fib a5 in
  let a3 = ( - ) n 1 in
  let a2 = fib a3 in
  ( + ) a2 a4
  ;;
  
  let main = let a6 = fib_acc 0 1 4 in
  let () = print_int a6 in
  let a7 = fib 4 in
  let () = print_int a7 in
  0
  ;;
  Типы после приведения в ANF:
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/004manyargs.ml
  let wrap f = let a3 = ( = ) 1 1 in
  if a3
  then f
  else f
  ;;
  
  let test3 a b c = let a0 = print_int a in
  let a1 = print_int b in
  let a2 = print_int c in
  0
  ;;
  
  let test10 a b c d e f g h i j = let a11 = ( + ) a b in
  let a10 = ( + ) a11 c in
  let a9 = ( + ) a10 d in
  let a8 = ( + ) a9 e in
  let a7 = ( + ) a8 f in
  let a6 = ( + ) a7 g in
  let a5 = ( + ) a6 h in
  let a4 = ( + ) a5 i in
  ( + ) a4 j
  ;;
  
  let main = let rez = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0
  ;;
  Типы после приведения в ANF:
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/005fix.ml
  let rec fix f x = let a0 = fix f in
  f a0 x
  ;;
  
  let fac self n = let a1 = ( <= ) n 1 in
  if a1
  then 1
  else let a3 = ( - ) n 1 in
  let a2 = self a3 in
  ( * ) n a2
  ;;
  
  let main = let a4 = fix fac 6 in
  let () = print_int a4 in
  0
  ;;
  Типы после приведения в ANF:
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/006partial.ml
  let a1 foo = ( + ) foo 2
  ;;
  
  let a2 foo = ( * ) foo 10
  ;;
  
  let foo b = if b
  then a1
  else a2
  ;;
  
  let a0 x = let a5 = foo false x in
  let a4 = foo true a5 in
  let a3 = foo false a4 in
  foo true a3
  ;;
  
  let main = let a6 = a0 11 in
  let () = print_int a6 in
  0
  ;;
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
  ;;
  
  let main = let a0 = foo 1 in
  let a1 = a0 2 in
  let a2 = a1 3 in
  let () = print_int a2 in
  0
  ;;
  Типы после приведения в ANF:
  val foo : int -> int -> int -> int
  val main : int
  $ ./anf_runner.exe < manytests/typed/006partial3.ml
  let a1 c = print_int c
  ;;
  
  let a0 b = let () = print_int b in
  a1
  ;;
  
  let foo a = let () = print_int a in
  a0
  ;;
  
  let main = let () = foo 4 8 9 in
  0
  ;;
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
  ;;
  
  let main = let a14 = ( ~- ) 555555 in
  let a13 = ( ~- ) 1 in
  let a12 = print_int a13 in
  let a11 = print_int 4 in
  let a10 = print_int 2 in
  let a9 = print_int 1 in
  let a8 = _start a9 a10 3 a11 100 1000 a12 10000 a14 in
  print_int a8
  ;;
  Типы после приведения в ANF:
  val _start : 'a -> 'b -> int -> 'c -> int -> int -> 'd -> int -> int -> int
  val main : unit
  $ ./anf_runner.exe < manytests/typed/008ascription.ml
  let addi f g x = let a2 = g x in
  f x a2
  ;;
  
  let a0 x b = if b
  then ( + ) x 1
  else ( * ) x 2
  ;;
  
  let a1 _start = let a3 = ( / ) _start 2 in
  ( = ) a3 0
  ;;
  
  let main = let a4 = addi a0 a1 4 in
  let () = print_int a4 in
  0
  ;;
  Типы после приведения в ANF:
  val addi : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val a0 : int -> bool -> int
  val a1 : int -> bool
  val main : int

  $ ./anf_runner.exe < manytests/typed/009let_poly.ml
  let a0 x = x
  ;;
  
  let temp = let a1 = a0 1 in
  let a2 = a0 true in
  (a1, a2)
  ;;
  Типы после приведения в ANF:
  val a0 : 'a -> 'a
  val temp : int * bool

  $ ./anf_runner.exe < manytests/typed/011mapcps.ml
  let a1 f h k a0 = let a5 = f h in
  let a4 = (a5::a0) in
  k a4
  ;;
  
  let rec map f xs k = let a6 = is_empty xs in
  if a6
  then let a7 = [] in
  k a7
  else let a9 = is_empty xs in
  let a8 = not a9 in
  if a8
  then let h = list_head xs in
  let tl = list_tail xs in
  let a10 = a1 f h k in
  map f tl a10
  else fail_match
  ;;
  
  let rec iter f xs = let a11 = is_empty xs in
  if a11
  then ()
  else let a13 = is_empty xs in
  let a12 = not a13 in
  if a12
  then let h = list_head xs in
  let tl = list_tail xs in
  let w = f h in
  iter f tl
  else fail_match
  ;;
  
  let a2 x = ( + ) x 1
  ;;
  
  let a3 x = x
  ;;
  
  let main = let a17 = (3::[]) in
  let a16 = (2::a17) in
  let a15 = (1::a16) in
  let a14 = map a2 a15 a3 in
  iter print_int a14
  ;;
  Типы после приведения в ANF:
  val a1 : ('a -> 'b) -> 'a -> ('b list -> 'c) -> 'b list -> 'c
  val map : ('a -> 'b) -> 'a list -> ('b list -> 'c) -> 'c
  val iter : ('a -> 'b) -> 'a list -> unit
  val a2 : int -> int
  val a3 : 'a -> 'a
  val main : unit
  $ ./anf_runner.exe < manytests/typed/012fibcps.ml
  let a1 a k b = let a3 = ( + ) a b in
  k a3
  ;;
  
  let a0 fib k n a = let a5 = a1 a k in
  let a4 = ( - ) n 2 in
  fib a4 a5
  ;;
  
  let rec fib n k = let a6 = ( < ) n 2 in
  if a6
  then k n
  else let a8 = a0 fib k n in
  let a7 = ( - ) n 1 in
  fib a7 a8
  ;;
  
  let a2 x = x
  ;;
  
  let main = let a9 = fib 6 a2 in
  print_int a9
  ;;
  Типы после приведения в ANF:
  val a1 : int -> (int -> 'a) -> int -> 'a
  val a0 : (int -> (int -> 'a) -> 'b) -> (int -> 'a) -> int -> int -> 'b
  val fib : int -> (int -> 'a) -> 'a
  val a2 : 'a -> 'a
  val main : unit
  $ ./anf_runner.exe < manytests/typed/013foldfoldr.ml
  let id x = x
  ;;
  
  let rec fold_right f acc xs = let a2 = is_empty xs in
  if a2
  then acc
  else let a4 = is_empty xs in
  let a3 = not a4 in
  if a3
  then let h = list_head xs in
  let tl = list_tail xs in
  let a5 = fold_right f acc tl in
  f h a5
  else fail_match
  ;;
  
  let a0 f b g x = let a6 = f x b in
  g a6
  ;;
  
  let foldl f a bs = let a7 = a0 f in
  fold_right a7 id bs a
  ;;
  
  let a1 x y = ( * ) x y
  ;;
  
  let main = let a11 = (3::[]) in
  let a10 = (2::a11) in
  let a9 = (1::a10) in
  let a8 = foldl a1 1 a9 in
  print_int a8
  ;;
  Типы после приведения в ANF:
  val id : 'a -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
  val a0 : ('a -> 'b -> 'c) -> 'b -> ('c -> 'd) -> 'a -> 'd
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val a1 : int -> int -> int
  val main : unit

  $ ./anf_runner.exe < manytests/typed/015tuples.ml
  let rec fix f x = let a3 = fix f in
  f a3 x
  ;;
  
  let map f p = let a = tuple_element p 0 in
  let b = tuple_element p 1 in
  let a4 = f a in
  let a5 = f b in
  (a4, a5)
  ;;
  
  let a2 a0 self li x = let a6 = self a0 in
  li a6 x
  ;;
  
  let a1 self a0 = let a7 = a2 a0 self in
  map a7 a0
  ;;
  
  let fixpoly l = fix a1 l
  ;;
  
  let feven p n = let e = tuple_element p 0 in
  let o = tuple_element p 1 in
  let a8 = ( = ) n 0 in
  if a8
  then 1
  else let a9 = ( - ) n 1 in
  o a9
  ;;
  
  let fodd p n = let e = tuple_element p 0 in
  let o = tuple_element p 1 in
  let a10 = ( = ) n 0 in
  if a10
  then 0
  else let a11 = ( - ) n 1 in
  e a11
  ;;
  
  let tie = let a12 = (feven, fodd) in
  fixpoly a12
  ;;
  
  let rec meven n = let a13 = ( = ) n 0 in
  if a13
  then 1
  else let a14 = ( - ) n 1 in
  modd a14
  and modd n = let a15 = ( = ) n 0 in
  if a15
  then 1
  else let a16 = ( - ) n 1 in
  meven a16
  ;;
  
  let main = let a17 = modd 1 in
  let () = print_int a17 in
  let a18 = meven 2 in
  let () = print_int a18 in
  let even = tuple_element tie 0 in
  let odd = tuple_element tie 1 in
  let a19 = odd 3 in
  let () = print_int a19 in
  let a20 = even 4 in
  let () = print_int a20 in
  0
  ;;
  Типы после приведения в ANF:
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('a -> 'b) -> 'c -> 'b * 'b
  val a2 : 'a -> ('a -> 'b) -> ('b -> 'c -> 'd) -> 'c -> 'd
  val a1 : ('a -> 'b) -> 'a -> ('c -> 'd) * ('c -> 'd)
  val fixpoly : 'a -> ('b -> 'c) * ('b -> 'c)
  val feven : 'a -> int -> int
  val fodd : 'a -> int -> int
  val tie : ('a -> 'b) * ('a -> 'b)
  val meven : int -> int
  val modd : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/016lists.ml
  let rec length xs = let a4 = is_empty xs in
  if a4
  then 0
  else let a6 = is_empty xs in
  let a5 = not a6 in
  if a5
  then let h = list_head xs in
  let tl = list_tail xs in
  let a7 = length tl in
  ( + ) 1 a7
  else fail_match
  ;;
  
  let rec a1 acc xs = let a8 = is_empty xs in
  if a8
  then acc
  else let a10 = is_empty xs in
  let a9 = not a10 in
  if a9
  then let h = list_head xs in
  let tl = list_tail xs in
  let a11 = ( + ) acc 1 in
  a1 a11 tl
  else fail_match
  ;;
  
  let length_tail = a1 0
  ;;
  
  let rec map f xs = let a12 = is_empty xs in
  if a12
  then []
  else let a17 = list_tail xs in
  let a16 = is_empty a17 in
  let a15 = is_empty xs in
  let a14 = not a15 in
  let a13 = ( && ) a14 a16 in
  if a13
  then let a = list_head xs in
  let a18 = f a in
  (a18::[])
  else let a24 = list_tail xs in
  let a23 = list_tail a24 in
  let a22 = is_empty a23 in
  let a21 = is_empty xs in
  let a20 = not a21 in
  let a19 = ( && ) a20 a22 in
  if a19
  then let a = list_head xs in
  let a25 = list_tail xs in
  let b = list_head a25 in
  let a26 = f a in
  let a28 = f b in
  let a27 = (a28::[]) in
  (a26::a27)
  else let a35 = list_tail xs in
  let a34 = list_tail a35 in
  let a33 = list_tail a34 in
  let a32 = is_empty a33 in
  let a31 = is_empty xs in
  let a30 = not a31 in
  let a29 = ( && ) a30 a32 in
  if a29
  then let a = list_head xs in
  let a36 = list_tail xs in
  let b = list_head a36 in
  let a38 = list_tail xs in
  let a37 = list_tail a38 in
  let c = list_head a37 in
  let a39 = f a in
  let a41 = f b in
  let a43 = f c in
  let a42 = (a43::[]) in
  let a40 = (a41::a42) in
  (a39::a40)
  else let a45 = is_empty xs in
  let a44 = not a45 in
  if a44
  then let a = list_head xs in
  let a46 = list_tail xs in
  let b = list_head a46 in
  let a48 = list_tail xs in
  let a47 = list_tail a48 in
  let c = list_head a47 in
  let a51 = list_tail xs in
  let a50 = list_tail a51 in
  let a49 = list_tail a50 in
  let d = list_head a49 in
  let a54 = list_tail xs in
  let a53 = list_tail a54 in
  let a52 = list_tail a53 in
  let tl = list_tail a52 in
  let a55 = f a in
  let a57 = f b in
  let a59 = f c in
  let a61 = f d in
  let a62 = map f tl in
  let a60 = (a61::a62) in
  let a58 = (a59::a60) in
  let a56 = (a57::a58) in
  (a55::a56)
  else fail_match
  ;;
  
  let rec append xs ys = let a63 = is_empty xs in
  if a63
  then ys
  else let a65 = is_empty xs in
  let a64 = not a65 in
  if a64
  then let x = list_head xs in
  let a0 = list_tail xs in
  let a66 = append a0 ys in
  (x::a66)
  else fail_match
  ;;
  
  let rec a2 xs = let a67 = is_empty xs in
  if a67
  then []
  else let a69 = is_empty xs in
  let a68 = not a69 in
  if a68
  then let h = list_head xs in
  let tl = list_tail xs in
  let a70 = a2 tl in
  append h a70
  else fail_match
  ;;
  
  let concat = a2
  ;;
  
  let rec iter f xs = let a71 = is_empty xs in
  if a71
  then ()
  else let a73 = is_empty xs in
  let a72 = not a73 in
  if a72
  then let h = list_head xs in
  let tl = list_tail xs in
  let () = f h in
  iter f tl
  else fail_match
  ;;
  
  let a3 h a = (h, a)
  ;;
  
  let rec cartesian xs ys = let a74 = is_empty xs in
  if a74
  then []
  else let a76 = is_empty xs in
  let a75 = not a76 in
  if a75
  then let h = list_head xs in
  let tl = list_tail xs in
  let a79 = cartesian tl ys in
  let a78 = a3 h in
  let a77 = map a78 ys in
  append a77 a79
  else fail_match
  ;;
  
  let main = let a82 = (3::[]) in
  let a81 = (2::a82) in
  let a80 = (1::a81) in
  let () = iter print_int a80 in
  let a90 = (4::[]) in
  let a89 = (3::a90) in
  let a88 = (2::a89) in
  let a87 = (1::a88) in
  let a86 = (2::[]) in
  let a85 = (1::a86) in
  let a84 = cartesian a85 a87 in
  let a83 = length a84 in
  let () = print_int a83 in
  0
  ;;
  Типы после приведения в ANF:
  val length : 'a list -> int
  val a1 : int -> 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val a2 : 'a list list -> 'a list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val a3 : 'a -> 'b -> 'a * 'b
  val cartesian : 'a list -> 'b list -> 'a * 'b list
  val main : int

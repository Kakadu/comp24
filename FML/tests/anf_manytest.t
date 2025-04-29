  $ ./anf_runner.exe << EOF
  > let main = 
  > let rec fib n k =
  > if n < 2
  > then k n
  > else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b))) in print_int(fib 6 (fun x -> x))
  > EOF
  let lam_ll2 a k b = let anf0 = ( + ) a b in
  k anf0
  ;;
  
  let lam_ll1 fib_ll0 k n a = let anf1 = lam_ll2 a k in
  let anf2 = ( - ) n 2 in
  fib_ll0 anf2 anf1
  ;;
  
  let rec fib_ll0 n k = let anf3 = ( < ) n 2 in
  if anf3
  then k n
  else let anf4 = lam_ll1 fib_ll0 k n in
  let anf5 = ( - ) n 1 in
  fib_ll0 anf5 anf4
  ;;
  
  let lam_ll3 x = x
  ;;
  
  let main = let anf6 = fib_ll0 6 lam_ll3 in
  print_int anf6
  ;;
  
  Типы до приведения в ANF:
  val main : unit
  
  Типы после приведения в ANF:
  val lam_ll2 : int -> (int -> 'a) -> int -> 'a
  val lam_ll1 : (int -> (int -> 'a) -> 'b) -> (int -> 'a) -> int -> int -> 'b
  val fib_ll0 : int -> (int -> 'a) -> 'a
  val lam_ll3 : 'a -> 'a
  val main : unit
  $ ./anf_runner.exe << EOF
  > let f x = let g y = x + y in g 5;;
  > EOF
  let g_ll0 x y = ( + ) x y
  ;;
  
  let f x = g_ll0 x 5
  ;;
  
  Типы до приведения в ANF:
  val f : int -> int
  
  Типы после приведения в ANF:
  val g_ll0 : int -> int -> int
  val f : int -> int

  $ ./anf_runner.exe << EOF
  > let length xs = match xs with
  > | a::b::[] -> 2
  > | a::[] -> 1
  > | [] -> 0
  > EOF
  let length xs = let anf2 = tl_list_get xs in
  let anf1 = is_cons anf2 in
  let anf6 = tl_list_get xs in
  let anf5 = tl_list_get anf6 in
  let anf4 = is_empty anf5 in
  let anf7 = is_cons xs in
  let anf3 = ( && ) anf7 anf4 in
  let anf0 = ( && ) anf3 anf1 in
  if anf0
  then let a = hd_list_get xs in
  let anf8 = tl_list_get xs in
  let b = hd_list_get anf8 in
  2
  else let anf11 = tl_list_get xs in
  let anf10 = is_empty anf11 in
  let anf12 = is_cons xs in
  let anf9 = ( && ) anf12 anf10 in
  if anf9
  then let a = hd_list_get xs in
  1
  else let anf13 = is_empty xs in
  if anf13
  then 0
  else fail_match 1
  ;;
  
  Типы до приведения в ANF:
  val length : 'a list -> int
  
  Типы после приведения в ANF:
  val length : 'a list -> int

  $ ./anf_runner.exe << EOF
  > let is_empty x = x+1
  > 
  > let rec length xs = match xs with
  > | [] -> 0
  > | _::tl -> 1 + length xs
  > EOF
  let is_empty_ac0 x = ( + ) x 1
  ;;
  
  let rec length xs = let anf0 = is_empty xs in
  if anf0
  then 0
  else let anf1 = is_cons xs in
  if anf1
  then let tl = tl_list_get xs in
  let anf2 = length xs in
  ( + ) 1 anf2
  else fail_match 1
  ;;
  
  Типы до приведения в ANF:
  val is_empty : int -> int
  val length : 'a list -> int
  
  Типы после приведения в ANF:
  val is_empty_ac0 : int -> int
  val length : 'a list -> int

  $ ./anf_runner.exe << EOF
  > let fac n =
  > let rec fack n k =
  > if n<=1 then k 1
  > else fack (n - 1) ((fun k n m -> k (m * n)) k n)
  > in
  > fack n (fun x -> x)
  > EOF
  let lam_ll1 k_ac1 n_ac2 m = let anf0 = ( * ) m n_ac2 in
  k_ac1 anf0
  ;;
  
  let rec fack_ll0 n_ac0 k = let anf1 = ( <= ) n_ac0 1 in
  if anf1
  then k 1
  else let anf2 = lam_ll1 k n_ac0 in
  let anf3 = ( - ) n_ac0 1 in
  fack_ll0 anf3 anf2
  ;;
  
  let lam_ll2 x = x
  ;;
  
  let fac n = fack_ll0 n lam_ll2
  ;;
  
  Типы до приведения в ANF:
  val fac : int -> int
  
  Типы после приведения в ANF:
  val lam_ll1 : (int -> 'a) -> int -> int -> 'a
  val fack_ll0 : int -> (int -> 'a) -> 'a
  val lam_ll2 : 'a -> 'a
  val fac : int -> int

  $ ./anf_runner.exe << EOF
  > let f x = match x with
  > | 1 -> 12
  > | 12 -> 12
  > | _ -> 325
  > EOF
  let f x = let anf0 = ( = ) x 1 in
  if anf0
  then 12
  else let anf1 = ( = ) x 12 in
  if anf1
  then 12
  else if true
  then 325
  else fail_match 1
  ;;
  
  Типы до приведения в ANF:
  val f : int -> int
  
  Типы после приведения в ANF:
  val f : int -> int

  $ ./anf_runner.exe < manytests/typed/001fac.ml
  let rec fac n = let anf0 = ( <= ) n 1 in
  if anf0
  then 1
  else let anf2 = ( - ) n 1 in
  let anf1 = fac anf2 in
  ( * ) n anf1
  ;;
  
  let main = let anf3 = fac 4 in
  let () = print_int anf3 in
  0
  ;;
  
  Типы до приведения в ANF:
  val fac : int -> int
  val main : int
  
  Типы после приведения в ANF:
  val fac : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/002fac.ml
  let lam_ll0 k n p = let anf0 = ( * ) p n in
  k anf0
  ;;
  
  let rec fac_cps n k = let anf1 = ( = ) n 1 in
  if anf1
  then k 1
  else let anf2 = lam_ll0 k n in
  let anf3 = ( - ) n 1 in
  fac_cps anf3 anf2
  ;;
  
  let lam_ll1 print_int_ac0 = print_int_ac0
  ;;
  
  let main = let anf4 = fac_cps 4 lam_ll1 in
  let () = print_int anf4 in
  0
  ;;
  
  Типы до приведения в ANF:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Типы после приведения в ANF:
  val lam_ll0 : (int -> 'a) -> int -> int -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val lam_ll1 : 'a -> 'a
  val main : int

  $ ./anf_runner.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n = let anf0 = ( = ) n 1 in
  if anf0
  then b
  else let n1 = ( - ) n 1 in
  let ab = ( + ) a b in
  fib_acc b ab n1
  ;;
  
  let rec fib n = let anf1 = ( < ) n 2 in
  if anf1
  then n
  else let anf3 = ( - ) n 2 in
  let anf2 = fib anf3 in
  let anf5 = ( - ) n 1 in
  let anf4 = fib anf5 in
  ( + ) anf4 anf2
  ;;
  
  let main = let anf6 = fib_acc 0 1 4 in
  let () = print_int anf6 in
  let anf7 = fib 4 in
  let () = print_int anf7 in
  0
  ;;
  
  Типы до приведения в ANF:
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int
  
  Типы после приведения в ANF:
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/004manyargs.ml
  let wrap f = let anf0 = ( = ) 1 1 in
  if anf0
  then f
  else f
  ;;
  
  let test3 a b c = let a_ac0 = print_int a in
  let b_ac1 = print_int b in
  let c_ac2 = print_int c in
  0
  ;;
  
  let test10 a b c d e f g h i j = let anf8 = ( + ) a b in
  let anf7 = ( + ) anf8 c in
  let anf6 = ( + ) anf7 d in
  let anf5 = ( + ) anf6 e in
  let anf4 = ( + ) anf5 f in
  let anf3 = ( + ) anf4 g in
  let anf2 = ( + ) anf3 h in
  let anf1 = ( + ) anf2 i in
  ( + ) anf1 j
  ;;
  
  let main = let rez = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0
  ;;
  
  Типы до приведения в ANF:
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  
  Типы после приведения в ANF:
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/005fix.ml
  let rec fix f x = let anf0 = fix f in
  f anf0 x
  ;;
  
  let fac self n = let anf1 = ( <= ) n 1 in
  if anf1
  then 1
  else let anf3 = ( - ) n 1 in
  let anf2 = self anf3 in
  ( * ) n anf2
  ;;
  
  let main = let anf4 = fix fac 6 in
  let () = print_int anf4 in
  0
  ;;
  
  Типы до приведения в ANF:
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int
  
  Типы после приведения в ANF:
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/006partial.ml
  let lam_ll0 foo = ( + ) foo 2
  ;;
  
  let lam_ll1 foo = ( * ) foo 10
  ;;
  
  let foo b = if b
  then lam_ll0
  else lam_ll1
  ;;
  
  let foo_ac0 x = let anf2 = foo false x in
  let anf1 = foo true anf2 in
  let anf0 = foo false anf1 in
  foo true anf0
  ;;
  
  let main = let anf3 = foo_ac0 11 in
  let () = print_int anf3 in
  0
  ;;
  
  Типы до приведения в ANF:
  val foo : int -> int
  val main : int
  
  Типы после приведения в ANF:
  val lam_ll0 : int -> int
  val lam_ll1 : int -> int
  val foo : bool -> int -> int
  val foo_ac0 : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/006partial2.ml
  let foo a b c = let () = print_int a in
  let () = print_int b in
  let () = print_int c in
  let anf0 = ( * ) b c in
  ( + ) a anf0
  ;;
  
  let main = let foo_ac0 = foo 1 in
  let foo_ac1 = foo_ac0 2 in
  let foo_ac2 = foo_ac1 3 in
  let () = print_int foo_ac2 in
  0
  ;;
  
  Типы до приведения в ANF:
  val foo : int -> int -> int -> int
  val main : int
  
  Типы после приведения в ANF:
  val foo : int -> int -> int -> int
  val main : int
  $ ./anf_runner.exe < manytests/typed/006partial3.ml
  let lam_ll1 c = print_int c
  ;;
  
  let lam_ll0 b = let () = print_int b in
  lam_ll1
  ;;
  
  let foo a = let () = print_int a in
  lam_ll0
  ;;
  
  let main = let () = foo 4 8 9 in
  0
  ;;
  
  Типы до приведения в ANF:
  val foo : int -> int -> int -> unit
  val main : int
  
  Типы после приведения в ANF:
  val lam_ll1 : int -> unit
  val lam_ll0 : int -> int -> unit
  val foo : int -> int -> int -> unit
  val main : int
  $ ./anf_runner.exe < manytests/typed/007order.ml
  let _start_ac0 () () a () b _c () d __ = let anf0 = ( + ) a b in
  let () = print_int anf0 in
  let () = print_int __ in
  let anf2 = ( * ) a b in
  let anf1 = ( / ) anf2 _c in
  ( + ) anf1 d
  ;;
  
  let main = let anf4 = ( ~- ) 555555 in
  let anf6 = ( ~- ) 1 in
  let anf5 = print_int anf6 in
  let anf7 = print_int 4 in
  let anf8 = print_int 2 in
  let anf9 = print_int 1 in
  let anf3 = _start_ac0 anf9 anf8 3 anf7 100 1000 anf5 10000 anf4 in
  print_int anf3
  ;;
  
  Типы до приведения в ANF:
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  
  Типы после приведения в ANF:
  val _start_ac0 : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  $ ./anf_runner.exe < manytests/typed/008ascription.ml
  let addi f g x = let anf0 = g x in
  f x anf0
  ;;
  
  let lam_ll0 x b = if b
  then ( + ) x 1
  else ( * ) x 2
  ;;
  
  let lam_ll1 _start_ac0 = let anf1 = ( / ) _start_ac0 2 in
  ( = ) anf1 0
  ;;
  
  let main = let anf2 = addi lam_ll0 lam_ll1 4 in
  let () = print_int anf2 in
  0
  ;;
  
  Типы до приведения в ANF:
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int
  
  Типы после приведения в ANF:
  val addi : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val lam_ll0 : int -> bool -> int
  val lam_ll1 : int -> bool
  val main : int

  $ ./anf_runner.exe < manytests/typed/009let_poly.ml
  let f_ll0 x = x
  ;;
  
  let temp = let anf0 = f_ll0 1 in
  let anf1 = f_ll0 true in
  (anf0, anf1)
  ;;
  
  Типы до приведения в ANF:
  val temp : int * bool
  
  Типы после приведения в ANF:
  val f_ll0 : 'a -> 'a
  val temp : int * bool

  $ ./anf_runner.exe < manytests/typed/011mapcps.ml
  let lam_ll0 f h k tl_ac0 = let anf1 = f h in
  let anf0 = (anf1::tl_ac0) in
  k anf0
  ;;
  
  let rec map f xs k = let anf2 = is_empty xs in
  if anf2
  then k []
  else let anf3 = is_cons xs in
  if anf3
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let anf4 = lam_ll0 f h k in
  map f tl anf4
  else fail_match 1
  ;;
  
  let rec iter f xs = let anf5 = is_empty xs in
  if anf5
  then ()
  else let anf6 = is_cons xs in
  if anf6
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let w = f h in
  iter f tl
  else fail_match 1
  ;;
  
  let lam_ll1 x = ( + ) x 1
  ;;
  
  let lam_ll2 x = x
  ;;
  
  let main = let anf10 = (3::[]) in
  let anf9 = (2::anf10) in
  let anf8 = (1::anf9) in
  let anf7 = map lam_ll1 anf8 lam_ll2 in
  iter print_int anf7
  ;;
  
  Типы до приведения в ANF:
  val map : ('a -> 'b) -> 'a list -> ('b list -> 'c) -> 'c
  val iter : ('a -> 'b) -> 'a list -> unit
  val main : unit
  
  Типы после приведения в ANF:
  val lam_ll0 : ('a -> 'b) -> 'a -> ('b list -> 'c) -> 'b list -> 'c
  val map : ('a -> 'b) -> 'a list -> ('b list -> 'c) -> 'c
  val iter : ('a -> 'b) -> 'a list -> unit
  val lam_ll1 : int -> int
  val lam_ll2 : 'a -> 'a
  val main : unit
  $ ./anf_runner.exe < manytests/typed/012fibcps.ml
  let lam_ll1 a k b = let anf0 = ( + ) a b in
  k anf0
  ;;
  
  let lam_ll0 fib k n a = let anf1 = lam_ll1 a k in
  let anf2 = ( - ) n 2 in
  fib anf2 anf1
  ;;
  
  let rec fib n k = let anf3 = ( < ) n 2 in
  if anf3
  then k n
  else let anf4 = lam_ll0 fib k n in
  let anf5 = ( - ) n 1 in
  fib anf5 anf4
  ;;
  
  let lam_ll2 x = x
  ;;
  
  let main = let anf6 = fib 6 lam_ll2 in
  print_int anf6
  ;;
  
  Типы до приведения в ANF:
  val fib : int -> (int -> 'a) -> 'a
  val main : unit
  
  Типы после приведения в ANF:
  val lam_ll1 : int -> (int -> 'a) -> int -> 'a
  val lam_ll0 : (int -> (int -> 'a) -> 'b) -> (int -> 'a) -> int -> int -> 'b
  val fib : int -> (int -> 'a) -> 'a
  val lam_ll2 : 'a -> 'a
  val main : unit
  $ ./anf_runner.exe < manytests/typed/013foldfoldr.ml
  let id x = x
  ;;
  
  let rec fold_right f acc xs = let anf0 = is_empty xs in
  if anf0
  then acc
  else let anf1 = is_cons xs in
  if anf1
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let anf2 = fold_right f acc tl in
  f h anf2
  else fail_match 1
  ;;
  
  let lam_ll1 f b g x = let anf3 = f x b in
  g anf3
  ;;
  
  let lam_ll0 fold_right f a bs = let anf4 = lam_ll1 f in
  fold_right anf4 id bs a
  ;;
  
  let foldl = lam_ll0 fold_right
  ;;
  
  let lam_ll2 x y = ( * ) x y
  ;;
  
  let main = let anf8 = (3::[]) in
  let anf7 = (2::anf8) in
  let anf6 = (1::anf7) in
  let anf5 = foldl lam_ll2 1 anf6 in
  print_int anf5
  ;;
  
  Типы до приведения в ANF:
  val id : 'a -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val main : unit
  
  Типы после приведения в ANF:
  val id : 'a -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
  val lam_ll1 : ('a -> 'b -> 'c) -> 'b -> ('c -> 'd) -> 'a -> 'd
  val lam_ll0 : (('a -> ('b -> 'c) -> 'd -> 'c) -> ('e -> 'e) -> 'f -> 'g -> 'h) -> ('d -> 'a -> 'b) -> 'g -> 'f -> 'h
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val lam_ll2 : int -> int -> int
  val main : unit

  $ ./anf_runner.exe < manytests/typed/015tuples.ml
  let rec fix f x = let anf0 = fix f in
  f anf0 x
  ;;
  
  let map f p = let a = tuple_get p 0 in
  let b = tuple_get p 1 in
  let anf1 = f a in
  let anf2 = f b in
  (anf1, anf2)
  ;;
  
  let lam_ll2 l_ac0 self li x = let anf3 = self l_ac0 in
  li anf3 x
  ;;
  
  let lam_ll1 self l_ac0 = let anf4 = lam_ll2 l_ac0 self in
  map anf4 l_ac0
  ;;
  
  let lam_ll0 fix l = fix lam_ll1 l
  ;;
  
  let fixpoly = lam_ll0 fix
  ;;
  
  let feven p n = let e = tuple_get p 0 in
  let o = tuple_get p 1 in
  let anf5 = ( = ) n 0 in
  if anf5
  then 1
  else let anf6 = ( - ) n 1 in
  o anf6
  ;;
  
  let fodd p n = let e = tuple_get p 0 in
  let o = tuple_get p 1 in
  let anf7 = ( = ) n 0 in
  if anf7
  then 0
  else let anf8 = ( - ) n 1 in
  e anf8
  ;;
  
  let tie = let anf9 = (feven, fodd) in
  fixpoly anf9
  ;;
  
  let rec meven n = let anf10 = ( = ) n 0 in
  if anf10
  then 1
  else let anf11 = ( - ) n 1 in
  modd anf11
  and modd n = let anf12 = ( = ) n 0 in
  if anf12
  then 1
  else let anf13 = ( - ) n 1 in
  meven anf13
  ;;
  
  let main = let anf14 = modd 1 in
  let () = print_int anf14 in
  let anf15 = meven 2 in
  let () = print_int anf15 in
  let even = tuple_get tie 0 in
  let odd = tuple_get tie 1 in
  let anf16 = odd 3 in
  let () = print_int anf16 in
  let anf17 = even 4 in
  let () = print_int anf17 in
  0
  ;;
  
  Типы до приведения в ANF:
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val feven : 'a * (int -> int) -> int -> int
  val fodd : (int -> int) * 'a -> int -> int
  val tie : (int -> int) * (int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int
  
  Типы после приведения в ANF:
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('a -> 'b) -> 'c -> 'b * 'b
  val lam_ll2 : 'a -> ('a -> 'b) -> ('b -> 'c -> 'd) -> 'c -> 'd
  val lam_ll1 : ('a -> 'b) -> 'a -> ('c -> 'd) * ('c -> 'd)
  val lam_ll0 : ((('a -> 'b) -> 'a -> ('c -> 'd) * ('c -> 'd)) -> 'e -> 'f) -> 'e -> 'f
  val fixpoly : 'a -> ('b -> 'c) * ('b -> 'c)
  val feven : 'a -> int -> int
  val fodd : 'a -> int -> int
  val tie : ('a -> 'b) * ('a -> 'b)
  val meven : int -> int
  val modd : int -> int
  val main : int

  $ ./anf_runner.exe < manytests/typed/016lists.ml
  let rec length xs = let anf0 = is_empty xs in
  if anf0
  then 0
  else let anf1 = is_cons xs in
  if anf1
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let anf2 = length tl in
  ( + ) 1 anf2
  else fail_match 1
  ;;
  
  let rec helper_ll0 acc xs = let anf3 = is_empty xs in
  if anf3
  then acc
  else let anf4 = is_cons xs in
  if anf4
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let anf5 = ( + ) acc 1 in
  helper_ll0 anf5 tl
  else fail_match 1
  ;;
  
  let length_tail = helper_ll0 0
  ;;
  
  let rec map f xs = let anf6 = is_empty xs in
  if anf6
  then []
  else let anf9 = tl_list_get xs in
  let anf8 = is_empty anf9 in
  let anf10 = is_cons xs in
  let anf7 = ( && ) anf10 anf8 in
  if anf7
  then let a = hd_list_get xs in
  let anf11 = f a in
  (anf11::[])
  else let anf14 = tl_list_get xs in
  let anf13 = is_cons anf14 in
  let anf18 = tl_list_get xs in
  let anf17 = tl_list_get anf18 in
  let anf16 = is_empty anf17 in
  let anf19 = is_cons xs in
  let anf15 = ( && ) anf19 anf16 in
  let anf12 = ( && ) anf15 anf13 in
  if anf12
  then let a = hd_list_get xs in
  let anf20 = tl_list_get xs in
  let b = hd_list_get anf20 in
  let anf21 = f a in
  let anf23 = f b in
  let anf22 = (anf23::[]) in
  (anf21::anf22)
  else let anf26 = tl_list_get xs in
  let anf25 = is_cons anf26 in
  let anf30 = tl_list_get xs in
  let anf29 = tl_list_get anf30 in
  let anf28 = is_cons anf29 in
  let anf35 = tl_list_get xs in
  let anf34 = tl_list_get anf35 in
  let anf33 = tl_list_get anf34 in
  let anf32 = is_empty anf33 in
  let anf36 = is_cons xs in
  let anf31 = ( && ) anf36 anf32 in
  let anf27 = ( && ) anf31 anf28 in
  let anf24 = ( && ) anf27 anf25 in
  if anf24
  then let a = hd_list_get xs in
  let anf37 = tl_list_get xs in
  let b = hd_list_get anf37 in
  let anf39 = tl_list_get xs in
  let anf38 = tl_list_get anf39 in
  let c = hd_list_get anf38 in
  let anf40 = f a in
  let anf42 = f b in
  let anf44 = f c in
  let anf43 = (anf44::[]) in
  let anf41 = (anf42::anf43) in
  (anf40::anf41)
  else let anf47 = tl_list_get xs in
  let anf46 = is_cons anf47 in
  let anf51 = tl_list_get xs in
  let anf50 = tl_list_get anf51 in
  let anf49 = is_cons anf50 in
  let anf56 = tl_list_get xs in
  let anf55 = tl_list_get anf56 in
  let anf54 = tl_list_get anf55 in
  let anf53 = is_cons anf54 in
  let anf57 = is_cons xs in
  let anf52 = ( && ) anf57 anf53 in
  let anf48 = ( && ) anf52 anf49 in
  let anf45 = ( && ) anf48 anf46 in
  if anf45
  then let a = hd_list_get xs in
  let anf58 = tl_list_get xs in
  let b = hd_list_get anf58 in
  let anf60 = tl_list_get xs in
  let anf59 = tl_list_get anf60 in
  let c = hd_list_get anf59 in
  let anf63 = tl_list_get xs in
  let anf62 = tl_list_get anf63 in
  let anf61 = tl_list_get anf62 in
  let d = hd_list_get anf61 in
  let anf66 = tl_list_get xs in
  let anf65 = tl_list_get anf66 in
  let anf64 = tl_list_get anf65 in
  let tl = tl_list_get anf64 in
  let anf67 = f a in
  let anf69 = f b in
  let anf71 = f c in
  let anf73 = f d in
  let anf74 = map f tl in
  let anf72 = (anf73::anf74) in
  let anf70 = (anf71::anf72) in
  let anf68 = (anf69::anf70) in
  (anf67::anf68)
  else fail_match 1
  ;;
  
  let rec append xs ys = let anf75 = is_empty xs in
  if anf75
  then ys
  else let anf76 = is_cons xs in
  if anf76
  then let x = hd_list_get xs in
  let xs_ac0 = tl_list_get xs in
  let anf77 = append xs_ac0 ys in
  (x::anf77)
  else fail_match 1
  ;;
  
  let rec helper_ll1 append xs = let anf78 = is_empty xs in
  if anf78
  then []
  else let anf79 = is_cons xs in
  if anf79
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let anf80 = helper_ll1 append tl in
  append h anf80
  else fail_match 1
  ;;
  
  let concat = helper_ll1 append
  ;;
  
  let rec iter f xs = let anf81 = is_empty xs in
  if anf81
  then ()
  else let anf82 = is_cons xs in
  if anf82
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let () = f h in
  iter f tl
  else fail_match 1
  ;;
  
  let lam_ll2 h a = (h, a)
  ;;
  
  let rec cartesian xs ys = let anf83 = is_empty xs in
  if anf83
  then []
  else let anf84 = is_cons xs in
  if anf84
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let anf85 = cartesian tl ys in
  let anf87 = lam_ll2 h in
  let anf86 = map anf87 ys in
  append anf86 anf85
  else fail_match 1
  ;;
  
  let main = let anf90 = (3::[]) in
  let anf89 = (2::anf90) in
  let anf88 = (1::anf89) in
  let () = iter print_int anf88 in
  let anf96 = (4::[]) in
  let anf95 = (3::anf96) in
  let anf94 = (2::anf95) in
  let anf93 = (1::anf94) in
  let anf98 = (2::[]) in
  let anf97 = (1::anf98) in
  let anf92 = cartesian anf97 anf93 in
  let anf91 = length anf92 in
  let () = print_int anf91 in
  0
  ;;
  
  Типы до приведения в ANF:
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val cartesian : 'a list -> 'b list -> 'a * 'b list
  val main : int
  
  Типы после приведения в ANF:
  val length : 'a list -> int
  val helper_ll0 : int -> 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val helper_ll1 : ('a -> 'b list -> 'b list) -> 'a list -> 'b list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val lam_ll2 : 'a -> 'b -> 'a * 'b
  val cartesian : 'a list -> 'b list -> 'a * 'b list
  val main : int

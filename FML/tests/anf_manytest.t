  $ ./anf_runner.exe << EOF
  > let f x = let g y = x + y in g 5;;
  > EOF
  let g_ll0 x y = ( + ) x y
  ;;
  
  let f x = g_ll0 x 5
  ;;

  $ ./anf_runner.exe << EOF
  > let length xs = match xs with
  > | a::b::[] -> 2
  > | a::[] -> 1
  > | [] -> 0
  > EOF
  let length xs = let anf1 = is_cons xs in
  let anf0 = if anf1
  then let anf3 = tl_list_get xs in
  let anf2 = is_cons anf3 in
  if anf2
  then let anf5 = tl_list_get xs in
  let anf4 = tl_list_get anf5 in
  is_empty anf4
  else false
  else false in
  if anf0
  then let a = hd_list_get xs in
  let anf6 = tl_list_get xs in
  let b = hd_list_get anf6 in
  2
  else let anf8 = is_cons xs in
  let anf7 = if anf8
  then let anf9 = tl_list_get xs in
  is_empty anf9
  else false in
  if anf7
  then let a = hd_list_get xs in
  1
  else let anf10 = is_empty xs in
  if anf10
  then 0
  else fail
  ;;

  $ ./anf_runner.exe << EOF
  > let is_empty x = x+1
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
  else fail
  ;;

  $ ./anf_runner.exe << EOF
  > let (a, b) = (5,6)
  > EOF
  let tmp_me0 = (5, 6)
  ;;
  
  let a = tuple_get tmp_me0 0
  ;;
  
  let b = tuple_get tmp_me0 1
  ;;

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
  else let anf2 = ( - ) n_ac0 1 in
  let anf3 = lam_ll1 k n_ac0 in
  fack_ll0 anf2 anf3
  ;;
  
  let lam_ll2 x = x
  ;;
  
  let fac n = fack_ll0 n lam_ll2
  ;;

  $ ./anf_runner.exe << EOF
  > let f x = match x with
  > | 1 -> 12
  > | 12 -> 12
  > | _ -> 325
  > EOF
  let lam_ll0 (=) x = let anf0 = (=) x 1 in
  if anf0
  then 12
  else let anf1 = (=) x 12 in
  if anf1
  then 12
  else if true
  then 325
  else fail
  ;;
  
  let f = lam_ll0 (=)
  ;;

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

  $ ./anf_runner.exe < manytests/typed/002fac.ml
  let lam_ll0 k n p = let anf0 = ( * ) p n in
  k anf0
  ;;
  
  let rec fac_cps n k = let anf1 = ( = ) n 1 in
  if anf1
  then k 1
  else let anf2 = ( - ) n 1 in
  let anf3 = lam_ll0 k n in
  fac_cps anf2 anf3
  ;;
  
  let lam_ll1 print_int_ac0 = print_int_ac0
  ;;
  
  let main = let anf4 = fac_cps 4 lam_ll1 in
  let () = print_int anf4 in
  0
  ;;

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
  else let anf3 = ( - ) n 1 in
  let anf2 = fib anf3 in
  let anf5 = ( - ) n 2 in
  let anf4 = fib anf5 in
  ( + ) anf2 anf4
  ;;
  
  let main = let anf6 = fib_acc 0 1 4 in
  let () = print_int anf6 in
  let anf7 = fib 4 in
  let () = print_int anf7 in
  0
  ;;

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
  $ ./anf_runner.exe < manytests/typed/007order.ml
  let _start () () a () b _c () d __ = let anf0 = ( + ) a b in
  let () = print_int anf0 in
  let () = print_int __ in
  let anf2 = ( * ) a b in
  let anf1 = ( / ) anf2 _c in
  ( + ) anf1 d
  ;;
  
  let main = let anf4 = print_int 1 in
  let anf5 = print_int 2 in
  let anf6 = print_int 4 in
  let anf8 = ( ~- ) 1 in
  let anf7 = print_int anf8 in
  let anf9 = ( ~- ) 555555 in
  let anf3 = _start anf4 anf5 3 anf6 100 1000 anf7 10000 anf9 in
  print_int anf3
  ;;
  $ ./anf_runner.exe < manytests/typed/008ascription.ml
  let addi f g x = let anf0 = g x in
  f x anf0
  ;;
  
  let lam_ll0 x b = if b
  then ( + ) x 1
  else ( * ) x 2
  ;;
  
  let lam_ll1 _start = let anf1 = ( / ) _start 2 in
  ( = ) anf1 0
  ;;
  
  let main = let anf2 = addi lam_ll0 lam_ll1 4 in
  let () = print_int anf2 in
  0
  ;;

  $ ./anf_runner.exe < manytests/typed/009let_poly.ml
  let f_ll0 x = x
  ;;
  
  let temp = let anf0 = f_ll0 1 in
  let anf1 = f_ll0 true in
  (anf0, anf1)
  ;;

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
  else fail
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
  else fail
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
  $ ./anf_runner.exe < manytests/typed/012fibcps.ml
  let lam_ll1 a k b = let anf0 = ( + ) a b in
  k anf0
  ;;
  
  let lam_ll0 k n a = let anf1 = ( - ) n 2 in
  let anf2 = lam_ll1 a k in
  fib anf1 anf2
  ;;
  
  let rec fib n k = let anf3 = ( < ) n 2 in
  if anf3
  then k n
  else let anf4 = ( - ) n 1 in
  let anf5 = lam_ll0 k n in
  fib anf4 anf5
  ;;
  
  let lam_ll2 x = x
  ;;
  
  let main = let anf6 = fib 6 lam_ll2 in
  print_int anf6
  ;;
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
  else fail
  ;;
  
  let lam_ll0 f b g x = let anf3 = f x b in
  g anf3
  ;;
  
  let foldl f a bs = let anf4 = lam_ll0 f in
  fold_right anf4 id bs a
  ;;
  
  let lam_ll1 x y = ( * ) x y
  ;;
  
  let main = let anf8 = (3::[]) in
  let anf7 = (2::anf8) in
  let anf6 = (1::anf7) in
  let anf5 = foldl lam_ll1 1 anf6 in
  print_int anf5
  ;;

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
  
  let lam_ll1 l_ac0 self li x = let anf3 = self l_ac0 in
  li anf3 x
  ;;
  
  let lam_ll0 self l_ac0 = let anf4 = lam_ll1 l_ac0 self in
  map anf4 l_ac0
  ;;
  
  let fixpoly l = fix lam_ll0 l
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
  else fail
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
  else fail
  ;;
  
  let length_tail = helper_ll0 0
  ;;
  
  let rec map f xs = let anf6 = is_empty xs in
  if anf6
  then []
  else let anf8 = is_cons xs in
  let anf7 = if anf8
  then let anf9 = tl_list_get xs in
  is_empty anf9
  else false in
  if anf7
  then let a = hd_list_get xs in
  let anf10 = f a in
  (anf10::[])
  else let anf12 = is_cons xs in
  let anf11 = if anf12
  then let anf14 = tl_list_get xs in
  let anf13 = is_cons anf14 in
  if anf13
  then let anf16 = tl_list_get xs in
  let anf15 = tl_list_get anf16 in
  is_empty anf15
  else false
  else false in
  if anf11
  then let a = hd_list_get xs in
  let anf17 = tl_list_get xs in
  let b = hd_list_get anf17 in
  let anf18 = f a in
  let anf20 = f b in
  let anf19 = (anf20::[]) in
  (anf18::anf19)
  else let anf22 = is_cons xs in
  let anf21 = if anf22
  then let anf24 = tl_list_get xs in
  let anf23 = is_cons anf24 in
  if anf23
  then let anf27 = tl_list_get xs in
  let anf26 = tl_list_get anf27 in
  let anf25 = is_cons anf26 in
  if anf25
  then let anf30 = tl_list_get xs in
  let anf29 = tl_list_get anf30 in
  let anf28 = tl_list_get anf29 in
  is_empty anf28
  else false
  else false
  else false in
  if anf21
  then let a = hd_list_get xs in
  let anf31 = tl_list_get xs in
  let b = hd_list_get anf31 in
  let anf33 = tl_list_get xs in
  let anf32 = tl_list_get anf33 in
  let c = hd_list_get anf32 in
  let anf34 = f a in
  let anf36 = f b in
  let anf38 = f c in
  let anf37 = (anf38::[]) in
  let anf35 = (anf36::anf37) in
  (anf34::anf35)
  else let anf40 = is_cons xs in
  let anf39 = if anf40
  then let anf42 = tl_list_get xs in
  let anf41 = is_cons anf42 in
  if anf41
  then let anf45 = tl_list_get xs in
  let anf44 = tl_list_get anf45 in
  let anf43 = is_cons anf44 in
  if anf43
  then let anf48 = tl_list_get xs in
  let anf47 = tl_list_get anf48 in
  let anf46 = tl_list_get anf47 in
  is_cons anf46
  else false
  else false
  else false in
  if anf39
  then let a = hd_list_get xs in
  let anf49 = tl_list_get xs in
  let b = hd_list_get anf49 in
  let anf51 = tl_list_get xs in
  let anf50 = tl_list_get anf51 in
  let c = hd_list_get anf50 in
  let anf54 = tl_list_get xs in
  let anf53 = tl_list_get anf54 in
  let anf52 = tl_list_get anf53 in
  let d = hd_list_get anf52 in
  let anf57 = tl_list_get xs in
  let anf56 = tl_list_get anf57 in
  let anf55 = tl_list_get anf56 in
  let tl = tl_list_get anf55 in
  let anf58 = f a in
  let anf60 = f b in
  let anf62 = f c in
  let anf64 = f d in
  let anf65 = map f tl in
  let anf63 = (anf64::anf65) in
  let anf61 = (anf62::anf63) in
  let anf59 = (anf60::anf61) in
  (anf58::anf59)
  else fail
  ;;
  
  let rec append xs ys = let anf66 = is_empty xs in
  if anf66
  then ys
  else let anf67 = is_cons xs in
  if anf67
  then let x = hd_list_get xs in
  let xs_ac0 = tl_list_get xs in
  let anf68 = append xs_ac0 ys in
  (x::anf68)
  else fail
  ;;
  
  let rec helper_ll1 xs = let anf69 = is_empty xs in
  if anf69
  then []
  else let anf70 = is_cons xs in
  if anf70
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let anf71 = helper_ll1 tl in
  append h anf71
  else fail
  ;;
  
  let concat = helper_ll1
  ;;
  
  let rec iter f xs = let anf72 = is_empty xs in
  if anf72
  then ()
  else let anf73 = is_cons xs in
  if anf73
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let () = f h in
  iter f tl
  else fail
  ;;
  
  let lam_ll2 h a = (h, a)
  ;;
  
  let rec cartesian xs ys = let anf74 = is_empty xs in
  if anf74
  then []
  else let anf75 = is_cons xs in
  if anf75
  then let h = hd_list_get xs in
  let tl = tl_list_get xs in
  let anf77 = lam_ll2 h in
  let anf76 = map anf77 ys in
  let anf78 = cartesian tl ys in
  append anf76 anf78
  else fail
  ;;
  
  let main = let anf81 = (3::[]) in
  let anf80 = (2::anf81) in
  let anf79 = (1::anf80) in
  let () = iter print_int anf79 in
  let anf85 = (2::[]) in
  let anf84 = (1::anf85) in
  let anf89 = (4::[]) in
  let anf88 = (3::anf89) in
  let anf87 = (2::anf88) in
  let anf86 = (1::anf87) in
  let anf83 = cartesian anf84 anf86 in
  let anf82 = length anf83 in
  let () = print_int anf82 in
  0
  ;;

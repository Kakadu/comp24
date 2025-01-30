  $ dune exec ./anf_demo.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  let  ll_1 nested2 nested3 i  = let anf_app_0 = ( + ) nested2 in
   let anf_app_1 = anf_app_0 nested3 in
   anf_app_1
  let  ll_0 nested2 nested3 x  = let anf_app_0 = ( + ) x in
   let anf_app_1 = ll_1 nested2 in
   let anf_app_2 = anf_app_1 nested3 in
   let anf_app_3 = anf_app_2 8 in
   let anf_app_4 = anf_app_0 anf_app_3 in
   anf_app_4
  let  nested1  = let nested2 = 5 in
   let nested3 = 6 in
   let anf_app_0 = ll_0 nested2 in
   let anf_app_1 = anf_app_0 nested3 in
   let anf_app_2 = anf_app_1 55 in
   anf_app_2

  $ dune exec ./anf_demo.exe << EOF
  > let rec fact_cps n cont =
  > if (n = 0) then
  >  cont 1
  > else
  >  fact_cps (n - 1) (fun acc -> cont (n * acc))
  > EOF
  let  ll_0 cont n acc  = let anf_app_0 = ( * ) n in
   let anf_app_1 = anf_app_0 acc in
   let anf_app_2 = cont anf_app_1 in
   anf_app_2
  let rec fact_cps n cont  = let anf_app_0 = ( = ) n in
   let anf_app_1 = anf_app_0 0 in
   let anf_ifthenelse_9 = if anf_app_1 then let anf_app_2 = cont 1 in
   anf_app_2 else let anf_app_3 = ( - ) n in
   let anf_app_4 = anf_app_3 1 in
   let anf_app_5 = fact_cps anf_app_4 in
   let anf_app_6 = ll_0 cont in
   let anf_app_7 = anf_app_6 n in
   let anf_app_8 = anf_app_5 anf_app_7 in
   anf_app_8 in
   anf_ifthenelse_9

  $ dune exec ./anf_demo.exe << EOF
  > let rec fib_cps n cont =
  > if (n = 0) then
  >  cont 0
  > else if (n = 1) then
  >   cont 1
  > else
  >   fib_cps (n - 1) (fun a ->
  >   fib_cps (n - 2) (fun b ->
  >   cont (a + b)))
  > EOF
  let  ll_1 a cont b  = let anf_app_0 = ( + ) a in
   let anf_app_1 = anf_app_0 b in
   let anf_app_2 = cont anf_app_1 in
   anf_app_2
  let  ll_0 cont fib_cps n a  = let anf_app_0 = ( - ) n in
   let anf_app_1 = anf_app_0 2 in
   let anf_app_2 = fib_cps anf_app_1 in
   let anf_app_3 = ll_1 a in
   let anf_app_4 = anf_app_3 cont in
   let anf_app_5 = anf_app_2 anf_app_4 in
   anf_app_5
  let rec fib_cps n cont  = let anf_app_0 = ( = ) n in
   let anf_app_1 = anf_app_0 0 in
   let anf_ifthenelse_14 = if anf_app_1 then let anf_app_2 = cont 0 in
   anf_app_2 else let anf_app_3 = ( = ) n in
   let anf_app_4 = anf_app_3 1 in
   let anf_ifthenelse_13 = if anf_app_4 then let anf_app_5 = cont 1 in
   anf_app_5 else let anf_app_6 = ( - ) n in
   let anf_app_7 = anf_app_6 1 in
   let anf_app_8 = fib_cps anf_app_7 in
   let anf_app_9 = ll_0 cont in
   let anf_app_10 = anf_app_9 fib_cps in
   let anf_app_11 = anf_app_10 n in
   let anf_app_12 = anf_app_8 anf_app_11 in
   anf_app_12 in
   anf_ifthenelse_13 in
   anf_ifthenelse_14

  $ dune exec ./anf_demo.exe << EOF
  > let test = let test5   = (5 + 5, 6 + 6, 7 + 7 * 8) in test5
  > EOF
  let  test  = let anf_app_0 = ( + ) 5 in
   let anf_app_1 = anf_app_0 5 in
   let anf_app_2 = ( + ) 6 in
   let anf_app_3 = anf_app_2 6 in
   let anf_app_4 = ( + ) 7 in
   let anf_app_5 = ( * ) 7 in
   let anf_app_6 = anf_app_5 8 in
   let anf_app_7 = anf_app_4 anf_app_6 in
   let anf_tuple_8 = (anf_app_1, anf_app_3, anf_app_7) in
   let test5 = anf_tuple_8 in
   test5

  $ dune exec ./anf_demo.exe < ./manytests/typed/001fac.ml
  let rec fac n  = let anf_app_0 = ( <= ) n in
   let anf_app_1 = anf_app_0 1 in
   let anf_ifthenelse_7 = if anf_app_1 then 1 else let anf_app_2 = ( * ) n in
   let anf_app_3 = ( - ) n in
   let anf_app_4 = anf_app_3 1 in
   let anf_app_5 = fac anf_app_4 in
   let anf_app_6 = anf_app_2 anf_app_5 in
   anf_app_6 in
   anf_ifthenelse_7
  let  main  = let anf_app_0 = fac 4 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/002fac.ml
  let  ll_0 k n p  = let anf_app_0 = ( * ) p in
   let anf_app_1 = anf_app_0 n in
   let anf_app_2 = k anf_app_1 in
   anf_app_2
  let rec fac_cps n k  = let anf_app_0 = ( = ) n in
   let anf_app_1 = anf_app_0 1 in
   let anf_ifthenelse_9 = if anf_app_1 then let anf_app_2 = k 1 in
   anf_app_2 else let anf_app_3 = ( - ) n in
   let anf_app_4 = anf_app_3 1 in
   let anf_app_5 = fac_cps anf_app_4 in
   let anf_app_6 = ll_0 k in
   let anf_app_7 = anf_app_6 n in
   let anf_app_8 = anf_app_5 anf_app_7 in
   anf_app_8 in
   anf_ifthenelse_9
  let  ll_1 print_int  = print_int
  let  main  = let anf_app_0 = fac_cps 4 in
   let anf_app_1 = anf_app_0 ll_1 in
   let anf_app_2 = print_int anf_app_1 in
   let () = anf_app_2 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/003fib.ml
  let rec fib_acc a b n  = let anf_app_0 = ( = ) n in
   let anf_app_1 = anf_app_0 1 in
   let anf_ifthenelse_9 = if anf_app_1 then b else let anf_app_2 = ( - ) n in
   let anf_app_3 = anf_app_2 1 in
   let n1 = anf_app_3 in
   let anf_app_4 = ( + ) a in
   let anf_app_5 = anf_app_4 b in
   let ab = anf_app_5 in
   let anf_app_6 = fib_acc b in
   let anf_app_7 = anf_app_6 ab in
   let anf_app_8 = anf_app_7 n1 in
   anf_app_8 in
   anf_ifthenelse_9
  let rec fib n  = let anf_app_0 = ( < ) n in
   let anf_app_1 = anf_app_0 2 in
   let anf_ifthenelse_10 = if anf_app_1 then n else let anf_app_2 = ( - ) n in
   let anf_app_3 = anf_app_2 1 in
   let anf_app_4 = fib anf_app_3 in
   let anf_app_5 = ( + ) anf_app_4 in
   let anf_app_6 = ( - ) n in
   let anf_app_7 = anf_app_6 2 in
   let anf_app_8 = fib anf_app_7 in
   let anf_app_9 = anf_app_5 anf_app_8 in
   anf_app_9 in
   anf_ifthenelse_10
  let  main  = let anf_app_0 = fib_acc 0 in
   let anf_app_1 = anf_app_0 1 in
   let anf_app_2 = anf_app_1 4 in
   let anf_app_3 = print_int anf_app_2 in
   let () = anf_app_3 in
   let anf_app_4 = fib 4 in
   let anf_app_5 = print_int anf_app_4 in
   let () = anf_app_5 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/004manyargs.ml
  let  wrap f  = let anf_app_0 = ( = ) 1 in
   let anf_app_1 = anf_app_0 1 in
   let anf_ifthenelse_2 = if anf_app_1 then f else f in
   anf_ifthenelse_2
  let  test3 a b c  = let anf_app_0 = print_int a in
   let a = anf_app_0 in
   let anf_app_1 = print_int b in
   let b = anf_app_1 in
   let anf_app_2 = print_int c in
   let c = anf_app_2 in
   0
  let  test10 a b c d e f g h i j  = let anf_app_0 = ( + ) a in
   let anf_app_1 = anf_app_0 b in
   let anf_app_2 = ( + ) anf_app_1 in
   let anf_app_3 = anf_app_2 c in
   let anf_app_4 = ( + ) anf_app_3 in
   let anf_app_5 = anf_app_4 d in
   let anf_app_6 = ( + ) anf_app_5 in
   let anf_app_7 = anf_app_6 e in
   let anf_app_8 = ( + ) anf_app_7 in
   let anf_app_9 = anf_app_8 f in
   let anf_app_10 = ( + ) anf_app_9 in
   let anf_app_11 = anf_app_10 g in
   let anf_app_12 = ( + ) anf_app_11 in
   let anf_app_13 = anf_app_12 h in
   let anf_app_14 = ( + ) anf_app_13 in
   let anf_app_15 = anf_app_14 i in
   let anf_app_16 = ( + ) anf_app_15 in
   let anf_app_17 = anf_app_16 j in
   anf_app_17
  let  main  = let anf_app_0 = wrap test10 in
   let anf_app_1 = anf_app_0 1 in
   let anf_app_2 = anf_app_1 10 in
   let anf_app_3 = anf_app_2 100 in
   let anf_app_4 = anf_app_3 1000 in
   let anf_app_5 = anf_app_4 10000 in
   let anf_app_6 = anf_app_5 100000 in
   let anf_app_7 = anf_app_6 1000000 in
   let anf_app_8 = anf_app_7 10000000 in
   let anf_app_9 = anf_app_8 100000000 in
   let anf_app_10 = anf_app_9 1000000000 in
   let rez = anf_app_10 in
   let anf_app_11 = print_int rez in
   let () = anf_app_11 in
   let anf_app_12 = wrap test3 in
   let anf_app_13 = anf_app_12 1 in
   let anf_app_14 = anf_app_13 10 in
   let anf_app_15 = anf_app_14 100 in
   let temp2 = anf_app_15 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/005fix.ml
  let rec fix f x  = let anf_app_0 = fix f in
   let anf_app_1 = f anf_app_0 in
   let anf_app_2 = anf_app_1 x in
   anf_app_2
  let  fac self n  = let anf_app_0 = ( <= ) n in
   let anf_app_1 = anf_app_0 1 in
   let anf_ifthenelse_7 = if anf_app_1 then 1 else let anf_app_2 = ( * ) n in
   let anf_app_3 = ( - ) n in
   let anf_app_4 = anf_app_3 1 in
   let anf_app_5 = self anf_app_4 in
   let anf_app_6 = anf_app_2 anf_app_5 in
   anf_app_6 in
   anf_ifthenelse_7
  let  main  = let anf_app_0 = fix fac in
   let anf_app_1 = anf_app_0 6 in
   let anf_app_2 = print_int anf_app_1 in
   let () = anf_app_2 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/006partial.ml
  let  ll_0 foo  = let anf_app_0 = ( + ) foo in
   let anf_app_1 = anf_app_0 2 in
   anf_app_1
  let  ll_1 foo  = let anf_app_0 = ( * ) foo in
   let anf_app_1 = anf_app_0 10 in
   anf_app_1
  let  foo b  = let anf_ifthenelse_0 = if b then ll_0 else ll_1 in
   anf_ifthenelse_0
  let  foo x  = let anf_app_0 = foo true in
   let anf_app_1 = foo false in
   let anf_app_2 = foo true in
   let anf_app_3 = foo false in
   let anf_app_4 = anf_app_3 x in
   let anf_app_5 = anf_app_2 anf_app_4 in
   let anf_app_6 = anf_app_1 anf_app_5 in
   let anf_app_7 = anf_app_0 anf_app_6 in
   anf_app_7
  let  main  = let anf_app_0 = foo 11 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/006partial2.ml
  let  foo a b c  = let anf_app_0 = print_int a in
   let () = anf_app_0 in
   let anf_app_1 = print_int b in
   let () = anf_app_1 in
   let anf_app_2 = print_int c in
   let () = anf_app_2 in
   let anf_app_3 = ( + ) a in
   let anf_app_4 = ( * ) b in
   let anf_app_5 = anf_app_4 c in
   let anf_app_6 = anf_app_3 anf_app_5 in
   anf_app_6
  let  main  = let anf_app_0 = foo 1 in
   let foo = anf_app_0 in
   let anf_app_1 = foo 2 in
   let foo = anf_app_1 in
   let anf_app_2 = foo 3 in
   let foo = anf_app_2 in
   let anf_app_3 = print_int foo in
   let () = anf_app_3 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/006partial3.ml
  let  ll_1 print_int c  = let anf_app_0 = print_int c in
   anf_app_0
  let  ll_0 print_int b  = let anf_app_0 = print_int b in
   let () = anf_app_0 in
   let anf_app_1 = ll_1 print_int in
   anf_app_1
  let  foo a  = let anf_app_0 = print_int a in
   let () = anf_app_0 in
   let anf_app_1 = ll_0 print_int in
   anf_app_1
  let  main  = let anf_app_0 = foo 4 in
   let anf_app_1 = anf_app_0 8 in
   let anf_app_2 = anf_app_1 9 in
   let () = anf_app_2 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/007order.ml
  let  _start () () a () b _c () d __  = let anf_app_0 = ( + ) a in
   let anf_app_1 = anf_app_0 b in
   let anf_app_2 = print_int anf_app_1 in
   let () = anf_app_2 in
   let anf_app_3 = print_int __ in
   let () = anf_app_3 in
   let anf_app_4 = ( * ) a in
   let anf_app_5 = anf_app_4 b in
   let anf_app_6 = ( / ) anf_app_5 in
   let anf_app_7 = anf_app_6 _c in
   let anf_app_8 = ( + ) anf_app_7 in
   let anf_app_9 = anf_app_8 d in
   anf_app_9
  let  main  = let anf_app_0 = print_int 1 in
   let anf_app_1 = _start anf_app_0 in
   let anf_app_2 = print_int 2 in
   let anf_app_3 = anf_app_1 anf_app_2 in
   let anf_app_4 = anf_app_3 3 in
   let anf_app_5 = print_int 4 in
   let anf_app_6 = anf_app_4 anf_app_5 in
   let anf_app_7 = anf_app_6 100 in
   let anf_app_8 = anf_app_7 1000 in
   let anf_app_9 = print_int -1 in
   let anf_app_10 = anf_app_8 anf_app_9 in
   let anf_app_11 = anf_app_10 10000 in
   let anf_app_12 = anf_app_11 -555555 in
   let anf_app_13 = print_int anf_app_12 in
   anf_app_13

  $ dune exec ./anf_demo.exe < ./manytests/typed/008ascription.ml
  let  addi f g x  = let anf_app_0 = f x in
   let anf_app_1 = g x in
   (anf_app_1 : bool)
  let  ll_0 x b  = let anf_ifthenelse_4 = if b then let anf_app_0 = ( + ) x in
   let anf_app_1 = anf_app_0 1 in
   anf_app_1 else let anf_app_2 = ( * ) x in
   let anf_app_3 = anf_app_2 2 in
   anf_app_3 in
   anf_ifthenelse_4
  let  ll_1 _start  = let anf_app_0 = ( / ) _start in
   let anf_app_1 = anf_app_0 2 in
   let anf_app_2 = ( = ) anf_app_1 in
   let anf_app_3 = anf_app_2 0 in
   anf_app_3
  let  main  = let anf_app_0 = addi ll_0 in
   let anf_app_1 = anf_app_0 ll_1 in
   let anf_app_2 = anf_app_1 4 in
   let anf_app_3 = print_int anf_app_2 in
   let () = anf_app_3 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/009let_poly.ml
  let  ll_0 x  = x
  let  temp  = let anf_app_0 = ll_0 1 in
   let anf_app_1 = ll_0 true in
   let anf_tuple_2 = (anf_app_0, anf_app_1) in
   anf_tuple_2

  $ dune exec ./anf_demo.exe < ./manytests/typed/015tuples.ml
  let rec fix f x  = let anf_app_0 = fix f in
   let anf_app_1 = f anf_app_0 in
   let anf_app_2 = anf_app_1 x in
   anf_app_2
  let  map f p  = let (a, b) = p in
   let anf_app_0 = f a in
   let anf_app_1 = f b in
   let anf_tuple_2 = (anf_app_0, anf_app_1) in
   anf_tuple_2
  let  ll_1 l self li x  = let anf_app_0 = self l in
   let anf_app_1 = li anf_app_0 in
   let anf_app_2 = anf_app_1 x in
   anf_app_2
  let  ll_0 self l  = let anf_app_0 = ll_1 l in
   let anf_app_1 = anf_app_0 self in
   let anf_app_2 = map anf_app_1 in
   let anf_app_3 = anf_app_2 l in
   anf_app_3
  let  fixpoly l  = let anf_app_0 = fix ll_0 in
   let anf_app_1 = anf_app_0 l in
   anf_app_1
  let  feven p n  = let (e, o) = p in
   let anf_app_0 = ( == ) n in
   let anf_app_1 = anf_app_0 0 in
   let anf_ifthenelse_5 = if anf_app_1 then 1 else let anf_app_2 = ( - ) n in
   let anf_app_3 = anf_app_2 1 in
   let anf_app_4 = o anf_app_3 in
   anf_app_4 in
   anf_ifthenelse_5
  let  fodd p n  = let (e, o) = p in
   let anf_app_0 = ( == ) n in
   let anf_app_1 = anf_app_0 0 in
   let anf_ifthenelse_5 = if anf_app_1 then 0 else let anf_app_2 = ( - ) n in
   let anf_app_3 = anf_app_2 1 in
   let anf_app_4 = e anf_app_3 in
   anf_app_4 in
   anf_ifthenelse_5
  let  tie  = let anf_tuple_0 = (feven, fodd) in
   let anf_app_1 = fixpoly anf_tuple_0 in
   anf_app_1
  let rec  meven n  = let anf_app_0 = ( = ) n in
   let anf_app_1 = anf_app_0 0 in
   let anf_ifthenelse_5 = if anf_app_1 then 1 else let anf_app_2 = ( - ) n in
   let anf_app_3 = anf_app_2 1 in
   let anf_app_4 = modd anf_app_3 in
   anf_app_4 in
   anf_ifthenelse_5  and  modd n  = let anf_app_6 = ( = ) n in
   let anf_app_7 = anf_app_6 0 in
   let anf_ifthenelse_11 = if anf_app_7 then 1 else let anf_app_8 = ( - ) n in
   let anf_app_9 = anf_app_8 1 in
   let anf_app_10 = meven anf_app_9 in
   anf_app_10 in
   anf_ifthenelse_11 
  let  main  = let anf_app_0 = modd 1 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   let anf_app_2 = meven 2 in
   let anf_app_3 = print_int anf_app_2 in
   let () = anf_app_3 in
   let (even, odd) = tie in
   let anf_app_4 = odd 3 in
   let anf_app_5 = print_int anf_app_4 in
   let () = anf_app_5 in
   let anf_app_6 = even 4 in
   let anf_app_7 = print_int anf_app_6 in
   let () = anf_app_7 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/016lists.ml
  let rec length xs  = match xs with
  | [] -> 0
  | (h :: tl) -> let anf_app_0 = ( + ) 1 in
   let anf_app_1 = length tl in
   let anf_app_2 = anf_app_0 anf_app_1 in
   anf_app_2
  let rec ll_0 acc xs  = match xs with
  | [] -> acc
  | (h :: tl) -> let anf_app_0 = ( + ) acc in
   let anf_app_1 = anf_app_0 1 in
   let anf_app_2 = ll_0 anf_app_1 in
   let anf_app_3 = anf_app_2 tl in
   anf_app_3
  let  length_tail  = let anf_app_0 = ll_0 0 in
   anf_app_0
  let rec map f xs  = match xs with
  | [] -> []
  | (a :: []) -> let anf_app_0 = f a in
   let anf_app_1 = ( :: ) anf_app_0 in
   let anf_app_2 = anf_app_1 [] in
   anf_app_2
  | (a :: (b :: [])) -> let anf_app_3 = f a in
   let anf_app_4 = ( :: ) anf_app_3 in
   let anf_app_5 = f b in
   let anf_app_6 = ( :: ) anf_app_5 in
   let anf_app_7 = anf_app_6 [] in
   let anf_app_8 = anf_app_4 anf_app_7 in
   anf_app_8
  | (a :: (b :: (c :: []))) -> let anf_app_9 = f a in
   let anf_app_10 = ( :: ) anf_app_9 in
   let anf_app_11 = f b in
   let anf_app_12 = ( :: ) anf_app_11 in
   let anf_app_13 = f c in
   let anf_app_14 = ( :: ) anf_app_13 in
   let anf_app_15 = anf_app_14 [] in
   let anf_app_16 = anf_app_12 anf_app_15 in
   let anf_app_17 = anf_app_10 anf_app_16 in
   anf_app_17
  | (a :: (b :: (c :: (d :: tl)))) -> let anf_app_18 = f a in
   let anf_app_19 = ( :: ) anf_app_18 in
   let anf_app_20 = f b in
   let anf_app_21 = ( :: ) anf_app_20 in
   let anf_app_22 = f c in
   let anf_app_23 = ( :: ) anf_app_22 in
   let anf_app_24 = f d in
   let anf_app_25 = ( :: ) anf_app_24 in
   let anf_app_26 = map f in
   let anf_app_27 = anf_app_26 tl in
   let anf_app_28 = anf_app_25 anf_app_27 in
   let anf_app_29 = anf_app_23 anf_app_28 in
   let anf_app_30 = anf_app_21 anf_app_29 in
   let anf_app_31 = anf_app_19 anf_app_30 in
   anf_app_31
  let rec append xs ys  = match xs with
  | [] -> ys
  | (x :: xs) -> let anf_app_0 = ( :: ) x in
   let anf_app_1 = append xs in
   let anf_app_2 = anf_app_1 ys in
   let anf_app_3 = anf_app_0 anf_app_2 in
   anf_app_3
  let rec ll_1 xs  = match xs with
  | [] -> []
  | (h :: tl) -> let anf_app_0 = append h in
   let anf_app_1 = ll_1 tl in
   let anf_app_2 = anf_app_0 anf_app_1 in
   anf_app_2
  let  concat  = ll_1
  let rec iter f xs  = match xs with
  | [] -> ()
  | (h :: tl) -> let anf_app_0 = f h in
   let () = anf_app_0 in
   let anf_app_1 = iter f in
   let anf_app_2 = anf_app_1 tl in
   anf_app_2
  let  ll_2 h a  = let anf_tuple_0 = (h, a) in
   anf_tuple_0
  let rec cartesian xs ys  = match xs with
  | [] -> []
  | (h :: tl) -> let anf_app_0 = ll_2 h in
   let anf_app_1 = map anf_app_0 in
   let anf_app_2 = anf_app_1 ys in
   let anf_app_3 = append anf_app_2 in
   let anf_app_4 = cartesian tl in
   let anf_app_5 = anf_app_4 ys in
   let anf_app_6 = anf_app_3 anf_app_5 in
   anf_app_6
  let  main  = let anf_app_0 = iter print_int in
   let anf_app_1 = ( :: ) 1 in
   let anf_app_2 = ( :: ) 2 in
   let anf_app_3 = ( :: ) 3 in
   let anf_app_4 = anf_app_3 [] in
   let anf_app_5 = anf_app_2 anf_app_4 in
   let anf_app_6 = anf_app_1 anf_app_5 in
   let anf_app_7 = anf_app_0 anf_app_6 in
   let () = anf_app_7 in
   let anf_app_8 = ( :: ) 1 in
   let anf_app_9 = ( :: ) 2 in
   let anf_app_10 = anf_app_9 [] in
   let anf_app_11 = anf_app_8 anf_app_10 in
   let anf_app_12 = cartesian anf_app_11 in
   let anf_app_13 = ( :: ) 1 in
   let anf_app_14 = ( :: ) 2 in
   let anf_app_15 = ( :: ) 3 in
   let anf_app_16 = ( :: ) 4 in
   let anf_app_17 = anf_app_16 [] in
   let anf_app_18 = anf_app_15 anf_app_17 in
   let anf_app_19 = anf_app_14 anf_app_18 in
   let anf_app_20 = anf_app_13 anf_app_19 in
   let anf_app_21 = anf_app_12 anf_app_20 in
   let anf_app_22 = length anf_app_21 in
   let anf_app_23 = print_int anf_app_22 in
   let () = anf_app_23 in
   0


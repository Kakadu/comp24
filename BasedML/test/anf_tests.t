  $ dune exec ./anf_demo.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  let  ll_1 nested2 nested3 i  = let anf_app_0 = ( + ) nested2 nested3 in
   anf_app_0
  let  ll_0 nested2 nested3 x  = let anf_app_0 = ll_1 nested2 nested3 8 in
   let anf_app_1 = ( + ) x anf_app_0 in
   anf_app_1
  let  nested1  = let nested2 = 5 in
   let nested3 = 6 in
   let anf_app_0 = ll_0 nested2 nested3 55 in
   anf_app_0

  $ dune exec ./anf_demo.exe << EOF
  > let rec fact_cps n cont =
  > if (n = 0) then
  >  cont 1
  > else
  >  fact_cps (n - 1) (fun acc -> cont (n * acc))
  > EOF
  let  ll_0 cont n acc  = let anf_app_0 = ( * ) n acc in
   let anf_app_1 = cont anf_app_0 in
   anf_app_1
  let rec fact_cps n cont  = let anf_app_0 = ( = ) n 0 in
   let anf_ifthenelse_5 = if anf_app_0 then let anf_app_1 = cont 1 in
   anf_app_1 else let anf_app_2 = ( - ) n 1 in
   let anf_app_3 = ll_0 cont n in
   let anf_app_4 = fact_cps anf_app_2 anf_app_3 in
   anf_app_4 in
   anf_ifthenelse_5

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
  let  ll_1 a cont b  = let anf_app_0 = ( + ) a b in
   let anf_app_1 = cont anf_app_0 in
   anf_app_1
  let  ll_0 cont fib_cps n a  = let anf_app_0 = ( - ) n 2 in
   let anf_app_1 = ll_1 a cont in
   let anf_app_2 = fib_cps anf_app_0 anf_app_1 in
   anf_app_2
  let rec fib_cps n cont  = let anf_app_0 = ( = ) n 0 in
   let anf_ifthenelse_8 = if anf_app_0 then let anf_app_1 = cont 0 in
   anf_app_1 else let anf_app_2 = ( = ) n 1 in
   let anf_ifthenelse_7 = if anf_app_2 then let anf_app_3 = cont 1 in
   anf_app_3 else let anf_app_4 = ( - ) n 1 in
   let anf_app_5 = ll_0 cont fib_cps n in
   let anf_app_6 = fib_cps anf_app_4 anf_app_5 in
   anf_app_6 in
   anf_ifthenelse_7 in
   anf_ifthenelse_8

  $ dune exec ./anf_demo.exe << EOF
  > let test = let test5   = (5 + 5, 6 + 6, 7 + 7 * 8) in test5
  > EOF
  let  test  = let anf_app_0 = ( + ) 5 5 in
   let anf_app_1 = ( + ) 6 6 in
   let anf_app_2 = ( * ) 7 8 in
   let anf_app_3 = ( + ) 7 anf_app_2 in
   let anf_tuple_4 = (anf_app_0, anf_app_1, anf_app_3) in
   let test5 = anf_tuple_4 in
   test5

  $ dune exec ./anf_demo.exe < ./manytests/typed/001fac.ml
  let rec fac n  = let anf_app_0 = ( <= ) n 1 in
   let anf_ifthenelse_4 = if anf_app_0 then 1 else let anf_app_1 = ( - ) n 1 in
   let anf_app_2 = fac anf_app_1 in
   let anf_app_3 = ( * ) n anf_app_2 in
   anf_app_3 in
   anf_ifthenelse_4
  let  main  = let anf_app_0 = fac 4 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/002fac.ml
  let  ll_0 k n p  = let anf_app_0 = ( * ) p n in
   let anf_app_1 = k anf_app_0 in
   anf_app_1
  let rec fac_cps n k  = let anf_app_0 = ( = ) n 1 in
   let anf_ifthenelse_5 = if anf_app_0 then let anf_app_1 = k 1 in
   anf_app_1 else let anf_app_2 = ( - ) n 1 in
   let anf_app_3 = ll_0 k n in
   let anf_app_4 = fac_cps anf_app_2 anf_app_3 in
   anf_app_4 in
   anf_ifthenelse_5
  let  ll_1 print_int  = print_int
  let  main  = let anf_app_0 = fac_cps 4 ll_1 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/003fib.ml
  let rec fib_acc a b n  = let anf_app_0 = ( = ) n 1 in
   let anf_ifthenelse_4 = if anf_app_0 then b else let anf_app_1 = ( - ) n 1 in
   let n1 = anf_app_1 in
   let anf_app_2 = ( + ) a b in
   let ab = anf_app_2 in
   let anf_app_3 = fib_acc b ab n1 in
   anf_app_3 in
   anf_ifthenelse_4
  let rec fib n  = let anf_app_0 = ( < ) n 2 in
   let anf_ifthenelse_6 = if anf_app_0 then n else let anf_app_1 = ( - ) n 1 in
   let anf_app_2 = fib anf_app_1 in
   let anf_app_3 = ( - ) n 2 in
   let anf_app_4 = fib anf_app_3 in
   let anf_app_5 = ( + ) anf_app_2 anf_app_4 in
   anf_app_5 in
   anf_ifthenelse_6
  let  main  = let anf_app_0 = fib_acc 0 1 4 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   let anf_app_2 = fib 4 in
   let anf_app_3 = print_int anf_app_2 in
   let () = anf_app_3 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/004manyargs.ml
  let  wrap f  = let anf_app_0 = ( = ) 1 1 in
   let anf_ifthenelse_1 = if anf_app_0 then f else f in
   anf_ifthenelse_1
  let  test3 a b c  = let anf_app_0 = print_int a in
   let a = anf_app_0 in
   let anf_app_1 = print_int b in
   let b = anf_app_1 in
   let anf_app_2 = print_int c in
   let c = anf_app_2 in
   0
  let  test10 a b c d e f g h i j  = let anf_app_0 = ( + ) a b in
   let anf_app_1 = ( + ) anf_app_0 c in
   let anf_app_2 = ( + ) anf_app_1 d in
   let anf_app_3 = ( + ) anf_app_2 e in
   let anf_app_4 = ( + ) anf_app_3 f in
   let anf_app_5 = ( + ) anf_app_4 g in
   let anf_app_6 = ( + ) anf_app_5 h in
   let anf_app_7 = ( + ) anf_app_6 i in
   let anf_app_8 = ( + ) anf_app_7 j in
   anf_app_8
  let  main  = let anf_app_0 = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
   let rez = anf_app_0 in
   let anf_app_1 = print_int rez in
   let () = anf_app_1 in
   let anf_app_2 = wrap test3 1 10 100 in
   let temp2 = anf_app_2 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/005fix.ml
  let rec fix f x  = let anf_app_0 = fix f in
   let anf_app_1 = f anf_app_0 x in
   anf_app_1
  let  fac self n  = let anf_app_0 = ( <= ) n 1 in
   let anf_ifthenelse_4 = if anf_app_0 then 1 else let anf_app_1 = ( - ) n 1 in
   let anf_app_2 = self anf_app_1 in
   let anf_app_3 = ( * ) n anf_app_2 in
   anf_app_3 in
   anf_ifthenelse_4
  let  main  = let anf_app_0 = fix fac 6 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/006partial.ml
  let  ll_0 foo  = let anf_app_0 = ( + ) foo 2 in
   anf_app_0
  let  ll_1 foo  = let anf_app_0 = ( * ) foo 10 in
   anf_app_0
  let  foo b  = let anf_ifthenelse_0 = if b then ll_0 else ll_1 in
   anf_ifthenelse_0
  let  foo x  = let anf_app_0 = foo false x in
   let anf_app_1 = foo true anf_app_0 in
   let anf_app_2 = foo false anf_app_1 in
   let anf_app_3 = foo true anf_app_2 in
   anf_app_3
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
   let anf_app_3 = ( * ) b c in
   let anf_app_4 = ( + ) a anf_app_3 in
   anf_app_4
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
  let  main  = let anf_app_0 = foo 4 8 9 in
   let () = anf_app_0 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/007order.ml
  let  _start () () a () b _c () d __  = let anf_app_0 = ( + ) a b in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   let anf_app_2 = print_int __ in
   let () = anf_app_2 in
   let anf_app_3 = ( * ) a b in
   let anf_app_4 = ( / ) anf_app_3 _c in
   let anf_app_5 = ( + ) anf_app_4 d in
   anf_app_5
  let  main  = let anf_app_0 = print_int 1 in
   let anf_app_1 = print_int 2 in
   let anf_app_2 = print_int 4 in
   let anf_app_3 = print_int -1 in
   let anf_app_4 = _start anf_app_0 anf_app_1 3 anf_app_2 100 1000 anf_app_3 10000 -555555 in
   let anf_app_5 = print_int anf_app_4 in
   anf_app_5

  $ dune exec ./anf_demo.exe < ./manytests/typed/008ascription.ml
  let  addi f g x  = let anf_app_0 = g x in
   (anf_app_0 : bool)
  let  ll_0 x b  = let anf_ifthenelse_2 = if b then let anf_app_0 = ( + ) x 1 in
   anf_app_0 else let anf_app_1 = ( * ) x 2 in
   anf_app_1 in
   anf_ifthenelse_2
  let  ll_1 _start  = let anf_app_0 = ( / ) _start 2 in
   let anf_app_1 = ( = ) anf_app_0 0 in
   anf_app_1
  let  main  = let anf_app_0 = addi ll_0 ll_1 4 in
   let anf_app_1 = print_int anf_app_0 in
   let () = anf_app_1 in
   0

  $ dune exec ./anf_demo.exe < ./manytests/typed/009let_poly.ml
  let  ll_0 x  = x
  let  temp  = let anf_app_0 = ll_0 1 in
   let anf_app_1 = ll_0 true in
   let anf_tuple_2 = (anf_app_0, anf_app_1) in
   anf_tuple_2

  $ dune exec ./anf_demo.exe < ./manytests/typed/015tuples.ml
  let rec fix f x  = let anf_app_0 = fix f in
   let anf_app_1 = f anf_app_0 x in
   anf_app_1
  let  map f p  = let (a, b) = p in
   let anf_app_0 = f a in
   let anf_app_1 = f b in
   let anf_tuple_2 = (anf_app_0, anf_app_1) in
   anf_tuple_2
  let  ll_1 l self li x  = let anf_app_0 = self l in
   let anf_app_1 = li anf_app_0 x in
   anf_app_1
  let  ll_0 self l  = let anf_app_0 = ll_1 l self in
   let anf_app_1 = map anf_app_0 l in
   anf_app_1
  let  fixpoly l  = let anf_app_0 = fix ll_0 l in
   anf_app_0
  let  feven p n  = let (e, o) = p in
   let anf_app_0 = ( == ) n 0 in
   let anf_ifthenelse_3 = if anf_app_0 then 1 else let anf_app_1 = ( - ) n 1 in
   let anf_app_2 = o anf_app_1 in
   anf_app_2 in
   anf_ifthenelse_3
  let  fodd p n  = let (e, o) = p in
   let anf_app_0 = ( == ) n 0 in
   let anf_ifthenelse_3 = if anf_app_0 then 0 else let anf_app_1 = ( - ) n 1 in
   let anf_app_2 = e anf_app_1 in
   anf_app_2 in
   anf_ifthenelse_3
  let  tie  = let anf_tuple_0 = (feven, fodd) in
   let anf_app_1 = fixpoly anf_tuple_0 in
   anf_app_1
  let rec  meven n  = let anf_app_0 = ( = ) n 0 in
   let anf_ifthenelse_3 = if anf_app_0 then 1 else let anf_app_1 = ( - ) n 1 in
   let anf_app_2 = modd anf_app_1 in
   anf_app_2 in
   anf_ifthenelse_3  and  modd n  = let anf_app_4 = ( = ) n 0 in
   let anf_ifthenelse_7 = if anf_app_4 then 1 else let anf_app_5 = ( - ) n 1 in
   let anf_app_6 = meven anf_app_5 in
   anf_app_6 in
   anf_ifthenelse_7 
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
  | (h :: tl) -> let anf_app_0 = length tl in
   let anf_app_1 = ( + ) 1 anf_app_0 in
   anf_app_1
  let rec ll_0 acc xs  = match xs with
  | [] -> acc
  | (h :: tl) -> let anf_app_0 = ( + ) acc 1 in
   let anf_app_1 = ll_0 anf_app_0 tl in
   anf_app_1
  let  length_tail  = let anf_app_0 = ll_0 0 in
   anf_app_0
  let rec map f xs  = match xs with
  | [] -> []
  | (a :: []) -> let anf_app_0 = f a in
   let anf_app_1 = ( :: ) anf_app_0 [] in
   anf_app_1
  | (a :: (b :: [])) -> let anf_app_2 = f a in
   let anf_app_3 = f b in
   let anf_app_4 = ( :: ) anf_app_3 [] in
   let anf_app_5 = ( :: ) anf_app_2 anf_app_4 in
   anf_app_5
  | (a :: (b :: (c :: []))) -> let anf_app_6 = f a in
   let anf_app_7 = f b in
   let anf_app_8 = f c in
   let anf_app_9 = ( :: ) anf_app_8 [] in
   let anf_app_10 = ( :: ) anf_app_7 anf_app_9 in
   let anf_app_11 = ( :: ) anf_app_6 anf_app_10 in
   anf_app_11
  | (a :: (b :: (c :: (d :: tl)))) -> let anf_app_12 = f a in
   let anf_app_13 = f b in
   let anf_app_14 = f c in
   let anf_app_15 = f d in
   let anf_app_16 = map f tl in
   let anf_app_17 = ( :: ) anf_app_15 anf_app_16 in
   let anf_app_18 = ( :: ) anf_app_14 anf_app_17 in
   let anf_app_19 = ( :: ) anf_app_13 anf_app_18 in
   let anf_app_20 = ( :: ) anf_app_12 anf_app_19 in
   anf_app_20
  let rec append xs ys  = match xs with
  | [] -> ys
  | (x :: xs) -> let anf_app_0 = append xs ys in
   let anf_app_1 = ( :: ) x anf_app_0 in
   anf_app_1
  let rec ll_1 xs  = match xs with
  | [] -> []
  | (h :: tl) -> let anf_app_0 = ll_1 tl in
   let anf_app_1 = append h anf_app_0 in
   anf_app_1
  let  concat  = ll_1
  let rec iter f xs  = match xs with
  | [] -> ()
  | (h :: tl) -> let anf_app_0 = f h in
   let () = anf_app_0 in
   let anf_app_1 = iter f tl in
   anf_app_1
  let  ll_2 h a  = let anf_tuple_0 = (h, a) in
   anf_tuple_0
  let rec cartesian xs ys  = match xs with
  | [] -> []
  | (h :: tl) -> let anf_app_0 = ll_2 h in
   let anf_app_1 = map anf_app_0 ys in
   let anf_app_2 = cartesian tl ys in
   let anf_app_3 = append anf_app_1 anf_app_2 in
   anf_app_3
  let  main  = let anf_app_0 = ( :: ) 3 [] in
   let anf_app_1 = ( :: ) 2 anf_app_0 in
   let anf_app_2 = ( :: ) 1 anf_app_1 in
   let anf_app_3 = iter print_int anf_app_2 in
   let () = anf_app_3 in
   let anf_app_4 = ( :: ) 2 [] in
   let anf_app_5 = ( :: ) 1 anf_app_4 in
   let anf_app_6 = ( :: ) 4 [] in
   let anf_app_7 = ( :: ) 3 anf_app_6 in
   let anf_app_8 = ( :: ) 2 anf_app_7 in
   let anf_app_9 = ( :: ) 1 anf_app_8 in
   let anf_app_10 = cartesian anf_app_5 anf_app_9 in
   let anf_app_11 = length anf_app_10 in
   let anf_app_12 = print_int anf_app_11 in
   let () = anf_app_12 in
   0

  $ dune exec ./anf_demo.exe << EOF
  > let test a1 a2 a3 = a1 + a2 + a3
  > let test_var = test (5 + 5) (6 + 6) (7 + 7)
  > EOF
  let  test a1 a2 a3  = let anf_app_0 = ( + ) a1 a2 in
   let anf_app_1 = ( + ) anf_app_0 a3 in
   anf_app_1
  let  test_var  = let anf_app_0 = ( + ) 5 5 in
   let anf_app_1 = ( + ) 6 6 in
   let anf_app_2 = ( + ) 7 7 in
   let anf_app_3 = test anf_app_0 anf_app_1 anf_app_2 in
   anf_app_3

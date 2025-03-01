  $ ./anf_demo.exe < manytests/do_not_type/001.ml
  let recfac n = let app_0 = (n <= 1) in
  let if_1 = if app_0 then 1 else let app_2 = (n - 1) in
  let app_3 = fac app_2 in
  let app_4 = (n * app_3) in
  app_4 in
  if_1

  $ ./anf_demo.exe < manytests/do_not_type/002if.ml
  let main  = 1

  $ ./anf_demo.exe < manytests/do_not_type/003occurs.ml
  let cc_ll_0 x f = let app_0 = x x f in
  app_0;;
  let cc_ll_1 f x = let app_0 = cc_ll_0 x in
  let app_1 = f app_0 in
  app_1;;
  let cc_ll_2 x f = let app_0 = x x f in
  app_0;;
  let cc_ll_3 f x = let app_0 = cc_ll_2 x in
  let app_1 = f app_0 in
  app_1;;
  let fix f = let app_0 = cc_ll_3 f in
  let app_1 = cc_ll_1 f app_0 in
  app_1

  $ ./anf_demo.exe < manytests/do_not_type/004let_poly.ml
  let cc_ll_0 f = let app_0 = f 1 in
  let app_1 = f true in
  (app_0, app_1);;
  let cc_ll_1 x = x;;
  let temp  = let app_0 = cc_ll_0 cc_ll_1 in
  app_0

  $ ./anf_demo.exe < manytests/do_not_type/015tuples.ml
  other patterns not supported (anf is run after pattern elimination)

PASS
  $ ./anf_demo.exe < manytests/typed/001fac.ml
  let rec fac n = let app_0 = (n <= 1) in
  let if_1 = if app_0 then 1 else let app_2 = (n - 1) in
  let app_3 = fac app_2 in
  let app_4 = (n * app_3) in
  app_4 in
  if_1;;
  let main  = let app_0 = fac 4 in
  let app_1 = print_int app_0 in
  let EVALUATED_0 = app_1 in
  let app_2 = (EVALUATED_0 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  0

PASS
  $ ./anf_demo.exe < manytests/typed/002fac.ml
  let cc_ll_0 n k p = let app_0 = (p * n) in
  let app_1 = k app_0 in
  app_1;;
  let rec fac_cps n k = let app_0 = (n = 1) in
  let if_1 = if app_0 then let app_2 = k 1 in
  app_2 else let app_3 = (n - 1) in
  let app_4 = cc_ll_0 n k in
  let app_5 = fac_cps app_3 app_4 in
  app_5 in
  if_1;;
  let cc_ll_1 print_int = print_int;;
  let main  = let app_0 = fac_cps 4 cc_ll_1 in
  let app_1 = print_int app_0 in
  let EVALUATED_0 = app_1 in
  let app_2 = (EVALUATED_0 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  0

PASS
  $ ./anf_demo.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n = let app_0 = (n = 1) in
  let if_1 = if app_0 then b else let app_2 = (n - 1) in
  let n1 = app_2 in
  let app_3 = (a + b) in
  let ab = app_3 in
  let app_4 = fib_acc b ab n1 in
  app_4 in
  if_1;;
  let rec fib n = let app_0 = (n < 2) in
  let if_1 = if app_0 then n else let app_2 = (n - 1) in
  let app_3 = fib app_2 in
  let app_4 = (n - 2) in
  let app_5 = fib app_4 in
  let app_6 = (app_3 + app_5) in
  app_6 in
  if_1;;
  let main  = let app_0 = fib_acc 0 1 4 in
  let app_1 = print_int app_0 in
  let EVALUATED_0 = app_1 in
  let app_2 = (EVALUATED_0 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  let app_5 = fib 4 in
  let app_6 = print_int app_5 in
  let EVALUATED_1 = app_6 in
  let app_7 = (EVALUATED_1 ( = ) ()) in
  let if_8 = if app_7 then () else let app_9 = RTE_ERROR_MATCH_FAILURE () in
  app_9 in
  let _ = if_8 in
  0

PASS
  $ ./anf_demo.exe < manytests/typed/004manyargs.ml
  let wrap f = let app_0 = (1 = 1) in
  let if_1 = if app_0 then f else f in
  if_1;;
  let test3 a b c = let app_0 = print_int a in
  let a = app_0 in
  let app_1 = print_int b in
  let b = app_1 in
  let app_2 = print_int c in
  let c = app_2 in
  0;;
  let test10 a b c d e f g h i j = let app_0 = (a + b) in
  let app_1 = (app_0 + c) in
  let app_2 = (app_1 + d) in
  let app_3 = (app_2 + e) in
  let app_4 = (app_3 + f) in
  let app_5 = (app_4 + g) in
  let app_6 = (app_5 + h) in
  let app_7 = (app_6 + i) in
  let app_8 = (app_7 + j) in
  app_8;;
  let main  = let app_0 = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
  let rez = app_0 in
  let app_1 = print_int rez in
  let EVALUATED_0 = app_1 in
  let app_2 = (EVALUATED_0 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  let app_5 = wrap test3 1 10 100 in
  let temp2 = app_5 in
  0

PASS
  $ ./anf_demo.exe < manytests/typed/005fix.ml
  let rec fix f x = let app_0 = fix f in
  let app_1 = f app_0 x in
  app_1;;
  let fac self n = let app_0 = (n <= 1) in
  let if_1 = if app_0 then 1 else let app_2 = (n - 1) in
  let app_3 = self app_2 in
  let app_4 = (n * app_3) in
  app_4 in
  if_1;;
  let main  = let app_0 = fix fac 6 in
  let app_1 = print_int app_0 in
  let EVALUATED_0 = app_1 in
  let app_2 = (EVALUATED_0 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  0

PASS
  $ ./anf_demo.exe < manytests/typed/006partial.ml
  let cc_ll_0 foo = let app_0 = (foo + 2) in
  app_0;;
  let cc_ll_1 foo = let app_0 = (foo * 10) in
  app_0;;
  let foo b = let if_0 = if b then cc_ll_0 else cc_ll_1 in
  if_0;;
  let foo x = let app_0 = foo false x in
  let app_1 = foo true app_0 in
  let app_2 = foo false app_1 in
  let app_3 = foo true app_2 in
  app_3;;
  let main  = let app_0 = foo 11 in
  let app_1 = print_int app_0 in
  let EVALUATED_0 = app_1 in
  let app_2 = (EVALUATED_0 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  0

PASS
  $ ./anf_demo.exe < manytests/typed/006partial2.ml
  let foo a b c = let app_0 = print_int a in
  let EVALUATED_0 = app_0 in
  let app_1 = (EVALUATED_0 ( = ) ()) in
  let if_2 = if app_1 then () else let app_3 = RTE_ERROR_MATCH_FAILURE () in
  app_3 in
  let _ = if_2 in
  let app_4 = print_int b in
  let EVALUATED_1 = app_4 in
  let app_5 = (EVALUATED_1 ( = ) ()) in
  let if_6 = if app_5 then () else let app_7 = RTE_ERROR_MATCH_FAILURE () in
  app_7 in
  let _ = if_6 in
  let app_8 = print_int c in
  let EVALUATED_2 = app_8 in
  let app_9 = (EVALUATED_2 ( = ) ()) in
  let if_10 = if app_9 then () else let app_11 = RTE_ERROR_MATCH_FAILURE () in
  app_11 in
  let _ = if_10 in
  let app_12 = (b * c) in
  let app_13 = (a + app_12) in
  app_13;;
  let main  = let app_0 = foo 1 in
  let foo = app_0 in
  let app_1 = foo 2 in
  let foo = app_1 in
  let app_2 = foo 3 in
  let foo = app_2 in
  let app_3 = print_int foo in
  let EVALUATED_3 = app_3 in
  let app_4 = (EVALUATED_3 ( = ) ()) in
  let if_5 = if app_4 then () else let app_6 = RTE_ERROR_MATCH_FAILURE () in
  app_6 in
  let _ = if_5 in
  0

PASS
  $ ./anf_demo.exe < manytests/typed/006partial3.ml
  let cc_ll_0 c = let app_0 = print_int c in
  app_0;;
  let cc_ll_1 b = cc_ll_0;;
  let foo a = cc_ll_1;;
  let main  = let app_0 = foo 4 8 9 in
  let EVALUATED_2 = app_0 in
  let app_1 = (EVALUATED_2 ( = ) ()) in
  let if_2 = if app_1 then () else let app_3 = RTE_ERROR_MATCH_FAILURE () in
  app_3 in
  let _ = if_2 in
  0


PASS
  $ ./anf_demo.exe < manytests/typed/007order.ml
  let cc_ll_0 b a _c d __ = let app_0 = (a + b) in
  let app_1 = print_int app_0 in
  let EVALUATED_4 = app_1 in
  let app_2 = (EVALUATED_4 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  let app_5 = print_int __ in
  let EVALUATED_5 = app_5 in
  let app_6 = (EVALUATED_5 ( = ) ()) in
  let if_7 = if app_6 then () else let app_8 = RTE_ERROR_MATCH_FAILURE () in
  app_8 in
  let _ = if_7 in
  let app_9 = (a * b) in
  let app_10 = (app_9 / _c) in
  let app_11 = (app_10 + d) in
  app_11;;
  let cc_ll_1 a b _c P3 = let app_0 = (P3 ( = ) ()) in
  let if_1 = if app_0 then () else let app_2 = RTE_ERROR_MATCH_FAILURE () in
  app_2 in
  let _ = if_1 in
  let app_3 = cc_ll_0 b a _c in
  app_3;;
  let cc_ll_2 a P2 = let app_0 = (P2 ( = ) ()) in
  let if_1 = if app_0 then () else let app_2 = RTE_ERROR_MATCH_FAILURE () in
  app_2 in
  let _ = if_1 in
  let app_3 = cc_ll_1 a in
  app_3;;
  let cc_ll_3 P1 = cc_ll_2;;
  let _start P0 = cc_ll_3;;
  let main  = let app_0 = print_int 1 in
  let app_1 = print_int 2 in
  let app_2 = print_int 4 in
  let app_3 = - 1 in
  let app_4 = print_int app_3 in
  let app_5 = - 555555 in
  let app_6 = _start app_0 app_1 3 app_2 100 1000 app_4 10000 app_5 in
  let app_7 = print_int app_6 in
  app_7

PASS
  $ ./anf_demo.exe < manytests/typed/008ascription.ml
  let addi f g x = let app_0 = g x in
  let app_1 = f x (app_0 : bool) in
  (app_1 : int);;
  let cc_ll_0 x b = let if_0 = if b then let app_1 = (x + 1) in
  app_1 else let app_2 = (x * 2) in
  app_2 in
  if_0;;
  let cc_ll_1 _start = let app_0 = (_start / 2) in
  let app_1 = (app_0 = 0) in
  app_1;;
  let main  = let app_0 = addi cc_ll_0 cc_ll_1 4 in
  let app_1 = print_int app_0 in
  let EVALUATED_0 = app_1 in
  let app_2 = (EVALUATED_0 ( = ) ()) in
  let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
  app_4 in
  let _ = if_3 in
  0
PASS
  $ ./anf_demo.exe < manytests/typed/009let_poly.ml
  let cc_ll_0 x = x;;
  let temp  = let app_0 = cc_ll_0 1 in
  let app_1 = cc_ll_0 true in
  (app_0, app_1)

  $ ./anf_demo.exe < manytests/typed/015tuples.ml
  other patterns not supported (anf is run after pattern elimination)

  $ ./anf_demo.exe < manytests/typed/016lists.ml
  let rec length xs = let app_0 = ([] ( = ) xs) in
  let if_1 = if app_0 then 0 else let app_2 = ([] ( != ) xs) in
  let if_3 = if app_2 then let app_4 = GET_HEAD xs in
  let h = app_4 in
  let app_5 = GET_TALE xs in
  let tl = app_5 in
  let app_6 = length tl in
  let app_7 = (1 + app_6) in
  app_7 else let app_8 = RTE_ERROR_MATCH_FAILURE () in
  app_8 in
  if_3 in
  if_1;;
  let rec cc_ll_0 acc xs = let EVALUATED_1 = xs in
  let app_0 = ([] ( = ) EVALUATED_1) in
  let if_1 = if app_0 then acc else let app_2 = ([] ( != ) EVALUATED_1) in
  let if_3 = if app_2 then let app_4 = GET_HEAD EVALUATED_1 in
  let h = app_4 in
  let app_5 = GET_TALE EVALUATED_1 in
  let tl = app_5 in
  let app_6 = (acc + 1) in
  let app_7 = cc_ll_0 app_6 tl in
  app_7 else let app_8 = RTE_ERROR_MATCH_FAILURE () in
  app_8 in
  if_3 in
  if_1;;
  let length_tail  = let app_0 = cc_ll_0 0 in
  app_0;;
  let rec map f xs = let app_0 = ([] ( = ) xs) in
  let if_1 = if app_0 then [] else let app_2 = ([] ( != ) xs) in
  let app_3 = GET_TALE xs in
  let app_4 = ([] ( = ) app_3) in
  let app_5 = (app_2 ( && ) app_4) in
  let if_6 = if app_5 then let app_7 = GET_HEAD xs in
  let a = app_7 in
  let app_8 = f a in
  app_8 :: [] else let app_9 = ([] ( != ) xs) in
  let app_10 = GET_TALE xs in
  let app_11 = ([] ( != ) app_10) in
  let app_12 = GET_TALE xs in
  let app_13 = GET_TALE app_12 in
  let app_14 = ([] ( = ) app_13) in
  let app_15 = (app_11 ( && ) app_14) in
  let app_16 = (app_9 ( && ) app_15) in
  let if_17 = if app_16 then let app_18 = GET_HEAD xs in
  let a = app_18 in
  let app_19 = GET_TALE xs in
  let app_20 = GET_HEAD app_19 in
  let b = app_20 in
  let app_21 = f a in
  let app_22 = f b in
  app_21 :: app_22 :: [] else let app_23 = ([] ( != ) xs) in
  let app_24 = GET_TALE xs in
  let app_25 = ([] ( != ) app_24) in
  let app_26 = GET_TALE xs in
  let app_27 = GET_TALE app_26 in
  let app_28 = ([] ( != ) app_27) in
  let app_29 = GET_TALE xs in
  let app_30 = GET_TALE app_29 in
  let app_31 = GET_TALE app_30 in
  let app_32 = ([] ( = ) app_31) in
  let app_33 = (app_28 ( && ) app_32) in
  let app_34 = (app_25 ( && ) app_33) in
  let app_35 = (app_23 ( && ) app_34) in
  let if_36 = if app_35 then let app_37 = GET_HEAD xs in
  let a = app_37 in
  let app_38 = GET_TALE xs in
  let app_39 = GET_HEAD app_38 in
  let b = app_39 in
  let app_40 = GET_TALE xs in
  let app_41 = GET_TALE app_40 in
  let app_42 = GET_HEAD app_41 in
  let c = app_42 in
  let app_43 = f a in
  let app_44 = f b in
  let app_45 = f c in
  app_43 :: app_44 :: app_45 :: [] else let app_46 = ([] ( != ) xs) in
  let app_47 = GET_TALE xs in
  let app_48 = ([] ( != ) app_47) in
  let app_49 = GET_TALE xs in
  let app_50 = GET_TALE app_49 in
  let app_51 = ([] ( != ) app_50) in
  let app_52 = GET_TALE xs in
  let app_53 = GET_TALE app_52 in
  let app_54 = GET_TALE app_53 in
  let app_55 = ([] ( != ) app_54) in
  let app_56 = (app_51 ( && ) app_55) in
  let app_57 = (app_48 ( && ) app_56) in
  let app_58 = (app_46 ( && ) app_57) in
  let if_59 = if app_58 then let app_60 = GET_HEAD xs in
  let a = app_60 in
  let app_61 = GET_TALE xs in
  let app_62 = GET_HEAD app_61 in
  let b = app_62 in
  let app_63 = GET_TALE xs in
  let app_64 = GET_TALE app_63 in
  let app_65 = GET_HEAD app_64 in
  let c = app_65 in
  let app_66 = GET_TALE xs in
  let app_67 = GET_TALE app_66 in
  let app_68 = GET_TALE app_67 in
  let app_69 = GET_HEAD app_68 in
  let d = app_69 in
  let app_70 = GET_TALE xs in
  let app_71 = GET_TALE app_70 in
  let app_72 = GET_TALE app_71 in
  let app_73 = GET_TALE app_72 in
  let tl = app_73 in
  let app_74 = f a in
  let app_75 = f b in
  let app_76 = f c in
  let app_77 = f d in
  let app_78 = map f tl in
  app_74 :: app_75 :: app_76 :: app_77 :: app_78 else let app_79 = RTE_ERROR_MATCH_FAILURE () in
  app_79 in
  if_59 in
  if_36 in
  if_17 in
  if_6 in
  if_1;;
  let rec append xs ys = let app_0 = ([] ( = ) xs) in
  let if_1 = if app_0 then ys else let app_2 = ([] ( != ) xs) in
  let if_3 = if app_2 then let app_4 = GET_HEAD xs in
  let x = app_4 in
  let app_5 = GET_TALE xs in
  let xs = app_5 in
  let app_6 = append xs ys in
  x :: app_6 else let app_7 = RTE_ERROR_MATCH_FAILURE () in
  app_7 in
  if_3 in
  if_1;;
  let rec cc_ll_1 xs = let EVALUATED_4 = xs in
  let app_0 = ([] ( = ) EVALUATED_4) in
  let if_1 = if app_0 then [] else let app_2 = ([] ( != ) EVALUATED_4) in
  let if_3 = if app_2 then let app_4 = GET_HEAD EVALUATED_4 in
  let h = app_4 in
  let app_5 = GET_TALE EVALUATED_4 in
  let tl = app_5 in
  let app_6 = cc_ll_1 tl in
  let app_7 = append h app_6 in
  app_7 else let app_8 = RTE_ERROR_MATCH_FAILURE () in
  app_8 in
  if_3 in
  if_1;;
  let concat  = cc_ll_1;;
  let rec iter f xs = let app_0 = ([] ( = ) xs) in
  let if_1 = if app_0 then () else let app_2 = ([] ( != ) xs) in
  let if_3 = if app_2 then let app_4 = GET_HEAD xs in
  let h = app_4 in
  let app_5 = GET_TALE xs in
  let tl = app_5 in
  let app_6 = f h in
  let EVALUATED_7 = app_6 in
  let app_7 = (EVALUATED_7 ( = ) ()) in
  let if_8 = if app_7 then () else let app_9 = RTE_ERROR_MATCH_FAILURE () in
  app_9 in
  let _ = if_8 in
  let app_10 = iter f tl in
  app_10 else let app_11 = RTE_ERROR_MATCH_FAILURE () in
  app_11 in
  if_3 in
  if_1;;
  let cc_ll_2 h a = (h, a);;
  let rec cartesian xs ys = let app_0 = ([] ( = ) xs) in
  let if_1 = if app_0 then [] else let app_2 = ([] ( != ) xs) in
  let if_3 = if app_2 then let app_4 = GET_HEAD xs in
  let h = app_4 in
  let app_5 = GET_TALE xs in
  let tl = app_5 in
  let app_6 = cc_ll_2 h in
  let app_7 = map app_6 ys in
  let app_8 = cartesian tl ys in
  let app_9 = append app_7 app_8 in
  app_9 else let app_10 = RTE_ERROR_MATCH_FAILURE () in
  app_10 in
  if_3 in
  if_1;;
  let main  = let app_0 = iter print_int 1 :: 2 :: 3 :: [] in
  let EVALUATED_8 = app_0 in
  let app_1 = (EVALUATED_8 ( = ) ()) in
  let if_2 = if app_1 then () else let app_3 = RTE_ERROR_MATCH_FAILURE () in
  app_3 in
  let _ = if_2 in
  let app_4 = cartesian 1 :: 2 :: [] 1 :: 2 :: 3 :: 4 :: [] in
  let app_5 = length app_4 in
  let app_6 = print_int app_5 in
  let EVALUATED_9 = app_6 in
  let app_7 = (EVALUATED_9 ( = ) ()) in
  let if_8 = if app_7 then () else let app_9 = RTE_ERROR_MATCH_FAILURE () in
  app_9 in
  let _ = if_8 in
  0

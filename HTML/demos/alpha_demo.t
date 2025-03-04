  $ ./alpha_demo.exe < manytests/do_not_type/001.ml
  Type inference before:
  Typecheck error: Unbound value fac
  
  Type inference after:
  Typecheck error: Unbound value fac

  $ ./alpha_demo.exe < manytests/do_not_type/002if.ml
  Type inference before:
  Typecheck error: This expression has type int but an expression was expected of type bool
  
  Type inference after:
  Typecheck error: This expression has type int but an expression was expected of type bool

  $ ./alpha_demo.exe < manytests/do_not_type/003occurs.ml
  Type inference before:
  Typecheck error: Occurs check failed
  
  Type inference after:
  Typecheck error: Occurs check failed

  $ ./alpha_demo.exe < manytests/do_not_type/004let_poly.ml
  Type inference before:
  Typecheck error: This expression has type bool but an expression was expected of type int
  
  Tuple expressions are not supported in ANF conversion

  $ ./alpha_demo.exe < manytests/do_not_type/015tuples.ml
  Type inference before:
  Typecheck error: Only variables are allowed as left-hand side of `let rec'
  
  Tuple expressions are not supported in ANF conversion

PASS
  $ ./alpha_demo.exe < manytests/typed/001fac.ml
  Type inference before:
  val fac : int -> int
  val main : int
  
  Type inference after:
  val fac : int -> int
  val main : int
  
  let rec fac.1 n = let app_0.l0 = (n <= 1) in
  let if_1.l0 = if app_0.l0 then 1 else let app_2.l2 = (n - 1) in
  let app_3.l7 = fac.0 app_2.l2 in
  let app_4.l9 = (n * app_3.l7) in
  app_4.l9 in
  if_1.l0;;
  let main.1  = let app_0.l10 = fac.1 4 in
  let app_1.l17 = print_int app_0.l10 in
  let EVALUATED_0.l36 = app_1.l17 in
  let app_2.l75 = (EVALUATED_0.l36 ( = ) ()) in
  let if_3.l94 = if app_2.l75 then () else let app_4.l69 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l69 in
  let _.l0 = if_3.l94 in
  0

PASS
  $ ./alpha_demo.exe < manytests/typed/002fac.ml
  Type inference before:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Type inference after:
  val cc_ll_0 : int -> (int -> 'a) -> int -> 'a
  val cc_ll_1 : 'a -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  let cc_ll_0.1 n k p = let app_0.l0 = (p * n) in
  let app_1.l0 = k app_0.l0 in
  app_1.l0;;
  let rec fac_cps.1 n k = let app_0.l12 = (n = 1) in
  let if_1.l0 = if app_0.l12 then let app_2.l33 = k 1 in
  app_2.l33 else let app_3.l11 = (n - 1) in
  let app_4.l35 = cc_ll_0.1 n k in
  let app_5.l14 = fac_cps.0 app_3.l11 app_4.l35 in
  app_5.l14 in
  if_1.l0;;
  let cc_ll_1.1 print_int = print_int;;
  let main.1  = let app_0.l32 = fac_cps.1 4 cc_ll_1.1 in
  let app_1.l42 = print_int app_0.l32 in
  let EVALUATED_0.l78 = app_1.l42 in
  let app_2.l144 = (EVALUATED_0.l78 ( = ) ()) in
  let if_3.l190 = if app_2.l144 then () else let app_4.l141 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l141 in
  let _.l0 = if_3.l190 in
  0

PASS
  $ ./alpha_demo.exe < manytests/typed/003fib.ml
  Type inference before:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  
  Type inference after:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  
  let rec fib_acc.1 a b n = let app_0.l0 = (n = 1) in
  let if_1.l0 = if app_0.l0 then b else let app_2.l4 = (n - 1) in
  let n1.l17 = app_2.l4 in
  let app_3.l28 = (a + b) in
  let ab.l52 = app_3.l28 in
  let app_4.l61 = fib_acc.0 b ab.l52 n1.l17 in
  app_4.l61 in
  if_1.l0;;
  let rec fib.1 n = let app_0.l25 = (n < 2) in
  let if_1.l1 = if app_0.l25 then n else let app_2.l114 = (n - 1) in
  let app_3.l75 = fib.0 app_2.l114 in
  let app_4.l139 = (n - 2) in
  let app_5.l631 = fib.0 app_4.l139 in
  let app_6.l1098 = (app_3.l75 + app_5.l631) in
  app_6.l1098 in
  if_1.l1;;
  let main.1  = let app_0.l46 = fib_acc.1 0 1 4 in
  let app_1.l64 = print_int app_0.l46 in
  let EVALUATED_0.l134 = app_1.l64 in
  let app_2.l298 = (EVALUATED_0.l134 ( = ) ()) in
  let if_3.l472 = if app_2.l298 then () else let app_4.l214 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l214 in
  let _.l665 = if_3.l472 in
  let app_5.l1812 = fib.1 4 in
  let app_6.l3334 = print_int app_5.l1812 in
  let EVALUATED_1.l6075 = app_6.l3334 in
  let app_7.l11379 = (EVALUATED_1.l6075 ( = ) ()) in
  let if_8.l18147 = if app_7.l11379 then () else let app_9.l15066 = RTE_ERROR_MATCH_FAILURE () in
  app_9.l15066 in
  let _.l672 = if_8.l18147 in
  0

PASS
  $ ./alpha_demo.exe < manytests/typed/004manyargs.ml
  Type inference before:
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  
  Type inference after:
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  
  let wrap.1 f = let app_0.l0 = (1 = 1) in
  let if_1.l0 = if app_0.l0 then f else f in
  if_1.l0;;
  let test3.1 a b c = let app_0.l23 = print_int a in
  let a.l12 = app_0.l23 in
  let app_1.l75 = print_int b in
  let b.l30 = app_1.l75 in
  let app_2.l224 = print_int c in
  let c.l0 = app_2.l224 in
  0;;
  let test10.1 a b c d e f g h i j = let app_0.l48 = (a + b) in
  let app_1.l137 = (app_0.l48 + c) in
  let app_2.l392 = (app_1.l137 + d) in
  let app_3.l151 = (app_2.l392 + e) in
  let app_4.l415 = (app_3.l151 + f) in
  let app_5.l1570 = (app_4.l415 + g) in
  let app_6.l936 = (app_5.l1570 + h) in
  let app_7.l1678 = (app_6.l936 + i) in
  let app_8.l2202 = (app_7.l1678 + j) in
  app_8.l2202;;
  let main.1  = let app_0.l67 = wrap.1 test10.1 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
  let rez.l62 = app_0.l67 in
  let app_1.l205 = print_int rez.l62 in
  let EVALUATED_0.l236 = app_1.l205 in
  let app_2.l627 = (EVALUATED_0.l236 ( = ) ()) in
  let if_3.l697 = if app_2.l627 then () else let app_4.l596 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l596 in
  let _.l510 = if_3.l697 in
  let app_5.l2790 = wrap.1 test3.1 1 10 100 in
  let temp2.l0 = app_5.l2790 in
  0

PASS
  $ ./alpha_demo.exe < manytests/typed/005fix.ml
  Type inference before:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  
  Type inference after:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  
  let rec fix.1 f x = let app_0.l0 = fix.0 f in
  let app_1.l0 = f app_0.l0 x in
  app_1.l0;;
  let fac.1 self n = let app_0.l11 = (n <= 1) in
  let if_1.l0 = if app_0.l11 then 1 else let app_2.l41 = (n - 1) in
  let app_3.l17 = self app_2.l41 in
  let app_4.l32 = (n * app_3.l17) in
  app_4.l32 in
  if_1.l0;;
  let main.1  = let app_0.l23 = fix.1 fac.1 6 in
  let app_1.l30 = print_int app_0.l23 in
  let EVALUATED_0.l57 = app_1.l30 in
  let app_2.l118 = (EVALUATED_0.l57 ( = ) ()) in
  let if_3.l141 = if app_2.l118 then () else let app_4.l104 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l104 in
  let _.l0 = if_3.l141 in
  0

PASS
  $ ./alpha_demo.exe < manytests/typed/006partial.ml
  Type inference before:
  val foo : int -> int
  val main : int
  
  Type inference after:
  val cc_ll_0 : int -> int
  val cc_ll_1 : int -> int
  val foo : int -> int
  val main : int
  
  let cc_ll_0.1 foo = let app_0.l0 = (foo + 2) in
  app_0.l0;;
  let cc_ll_1.1 foo = let app_0.l9 = (foo * 10) in
  app_0.l9;;
  let foo.1 b = let if_0.l0 = if b then cc_ll_0.1 else cc_ll_1.1 in
  if_0.l0;;
  let foo.2 x = let app_0.l24 = foo.1 false x in
  let app_1.l43 = foo.1 true app_0.l24 in
  let app_2.l102 = foo.1 false app_1.l43 in
  let app_3.l19 = foo.1 true app_2.l102 in
  app_3.l19;;
  let main.1  = let app_0.l35 = foo.2 11 in
  let app_1.l64 = print_int app_0.l35 in
  let EVALUATED_0.l78 = app_1.l64 in
  let app_2.l177 = (EVALUATED_0.l78 ( = ) ()) in
  let if_3.l190 = if app_2.l177 then () else let app_4.l121 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l121 in
  let _.l0 = if_3.l190 in
  0

PASS
  $ ./alpha_demo.exe < manytests/typed/006partial2.ml
  Type inference before:
  val foo : int -> int -> int -> int
  val main : int
  
  Type inference after:
  val foo : int -> int -> int -> int
  val main : int
  
  let foo.1 a b c = let app_0.l0 = print_int a in
  let EVALUATED_0.l18 = app_0.l0 in
  let app_1.l55 = (EVALUATED_0.l18 ( = ) ()) in
  let if_2.l118 = if app_1.l55 then () else let app_3.l12 = RTE_ERROR_MATCH_FAILURE () in
  app_3.l12 in
  let _.l34770 = if_2.l118 in
  let app_4.l486 = print_int b in
  let EVALUATED_1.l952 = app_4.l486 in
  let app_5.l1850 = (EVALUATED_1.l952 ( = ) ()) in
  let if_6.l3325 = if app_5.l1850 then () else let app_7.l1024 = RTE_ERROR_MATCH_FAILURE () in
  app_7.l1024 in
  let _.l37016 = if_6.l3325 in
  let app_8.l12289 = print_int c in
  let EVALUATED_2.l23554 = app_8.l12289 in
  let app_9.l45059 = (EVALUATED_2.l23554 ( = ) ()) in
  let if_10.l77828 = if app_9.l45059 then () else let app_11.l53244 = RTE_ERROR_MATCH_FAILURE () in
  app_11.l53244 in
  let _.l37086 = if_10.l77828 in
  let app_12.l278524 = (b * c) in
  let app_13.l458735 = (a + app_12.l278524) in
  app_13.l458735;;
  let main.1  = let app_0.l30 = foo.1 1 in
  let foo.l566 = app_0.l30 in
  let app_1.l145 = foo.l566 2 in
  let foo.l721 = app_1.l145 in
  let app_2.l277 = foo.l721 3 in
  let foo.l756 = app_2.l277 in
  let app_3.l1007 = print_int foo.l756 in
  let EVALUATED_3.l1861 = app_3.l1007 in
  let app_4.l3966 = (EVALUATED_3.l1861 ( = ) ()) in
  let if_5.l5393 = if app_4.l3966 then () else let app_6.l4361 = RTE_ERROR_MATCH_FAILURE () in
  app_6.l4361 in
  let _.l37088 = if_5.l5393 in
  0

PASS
  $ ./alpha_demo.exe < manytests/typed/006partial3.ml
  Type inference before:
  val foo : int -> int -> int -> Unit
  val main : int
  
  Type inference after:
  val cc_ll_0 : int -> Unit
  val cc_ll_1 : int -> int -> Unit
  val foo : int -> int -> int -> Unit
  val main : int
  
  let cc_ll_0.1 c = let app_0.l0 = print_int c in
  app_0.l0;;
  let cc_ll_1.1 b = let app_0.l15 = print_int b in
  let EVALUATED_1.l14 = app_0.l15 in
  let app_1.l60 = (EVALUATED_1.l14 ( = ) ()) in
  let if_2.l75 = if app_1.l60 then () else let app_3.l36 = RTE_ERROR_MATCH_FAILURE () in
  app_3.l36 in
  let _.l0 = if_2.l75 in
  cc_ll_0.1;;
  let foo.1 a = let app_0.l33 = print_int a in
  let EVALUATED_0.l24 = app_0.l33 in
  let app_1.l113 = (EVALUATED_0.l24 ( = ) ()) in
  let if_2.l142 = if app_1.l113 then () else let app_3.l76 = RTE_ERROR_MATCH_FAILURE () in
  app_3.l76 in
  let _.l0 = if_2.l142 in
  cc_ll_1.1;;
  let main.1  = let app_0.l45 = foo.1 4 8 9 in
  let EVALUATED_2.l34 = app_0.l45 in
  let app_1.l149 = (EVALUATED_2.l34 ( = ) ()) in
  let if_2.l188 = if app_1.l149 then () else let app_3.l104 = RTE_ERROR_MATCH_FAILURE () in
  app_3.l104 in
  let _.l0 = if_2.l188 in
  0


PASS
  $ ./alpha_demo.exe < manytests/typed/007order.ml
  Type inference before:
  val _start : Unit -> Unit -> int -> Unit -> int -> int -> Unit -> int -> int -> int
  val main : Unit
  
  Type inference after:
  val _start : Unit -> Unit -> int -> Unit -> int -> int -> Unit -> int -> int -> int
  val cc_ll_0 : int -> int -> int -> int -> int -> int
  val cc_ll_1 : int -> int -> int -> Unit -> int -> int -> int
  val cc_ll_2 : int -> Unit -> int -> int -> Unit -> int -> int -> int
  val cc_ll_3 : Unit -> int -> Unit -> int -> int -> Unit -> int -> int -> int
  val main : Unit
  
  let cc_ll_0.1 b a _c d __ = let app_0.l0 = (a + b) in
  let app_1.l14 = print_int app_0.l0 in
  let EVALUATED_4.l43 = app_1.l14 in
  let app_2.l97 = (EVALUATED_4.l43 ( = ) ()) in
  let if_3.l184 = if app_2.l97 then () else let app_4.l32 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l32 in
  let _.l3255 = if_3.l184 in
  let app_5.l700 = print_int __ in
  let EVALUATED_5.l1342 = app_5.l700 in
  let app_6.l2560 = (EVALUATED_5.l1342 ( = ) ()) in
  let if_7.l4355 = if app_6.l2560 then () else let app_8.l2304 = RTE_ERROR_MATCH_FAILURE () in
  app_8.l2304 in
  let _.l3357 = if_7.l4355 in
  let app_9.l15367 = (a * b) in
  let app_10.l28680 = (app_9.l15367 / _c) in
  let app_11.l45067 = (app_10.l28680 + d) in
  app_11.l45067;;
  let cc_ll_1.1 a b _c P3 = let app_0.l36 = (P3 ( = ) ()) in
  let if_1.l19 = if app_0.l36 then () else let app_2.l144 = RTE_ERROR_MATCH_FAILURE () in
  app_2.l144 in
  let _.l3371 = if_1.l19 in
  let app_3.l51 = cc_ll_0.1 b a _c in
  app_3.l51;;
  let cc_ll_2.1 a P2 = let app_0.l67 = (P2 ( = ) ()) in
  let if_1.l40 = if app_0.l67 then () else let app_2.l186 = RTE_ERROR_MATCH_FAILURE () in
  app_2.l186 in
  let _.l3379 = if_1.l40 in
  let app_3.l110 = cc_ll_1.1 a in
  app_3.l110;;
  let cc_ll_3.1 P1 = let app_0.l93 = (P1 ( = ) ()) in
  let if_1.l54 = if app_0.l93 then () else let app_2.l227 = RTE_ERROR_MATCH_FAILURE () in
  app_2.l227 in
  let _.l3380 = if_1.l54 in
  cc_ll_2.1;;
  let _start.1 P0 = let app_0.l114 = (P0 ( = ) ()) in
  let if_1.l63 = if app_0.l114 then () else let app_2.l267 = RTE_ERROR_MATCH_FAILURE () in
  app_2.l267 in
  let _.l3380 = if_1.l63 in
  cc_ll_3.1;;
  let main.1  = let app_0.l131 = print_int 1 in
  let app_1.l146 = print_int 2 in
  let app_2.l334 = print_int 4 in
  let app_3.l289 = - 1 in
  let app_4.l604 = print_int app_3.l289 in
  let app_5.l1565 = - 555555 in
  let app_6.l4081 = _start.1 app_0.l131 app_1.l146 3 app_2.l334 100 1000 app_4.l604 10000 app_5.l1565 in
  let app_7.l1869 = print_int app_6.l4081 in
  app_7.l1869

PASS
  $ ./alpha_demo.exe < manytests/typed/008ascription.ml
  Type inference before:
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int
  
  Type inference after:
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val cc_ll_0 : int -> bool -> int
  val cc_ll_1 : int -> bool
  val main : int
  
  let addi.1 f g x = let app_0.l0 = g x in
  let app_1.l0 = f x (app_0.l0 : bool) in
  (app_1.l0 : int);;
  let cc_ll_0.1 x b = let if_0.l0 = if b then let app_1.l12 = (x + 1) in
  app_1.l12 else let app_2.l32 = (x * 2) in
  app_2.l32 in
  if_0.l0;;
  let cc_ll_1.1 _start = let app_0.l17 = (_start / 2) in
  let app_1.l25 = (app_0.l17 = 0) in
  app_1.l25;;
  let main.1  = let app_0.l26 = addi.1 cc_ll_0.1 cc_ll_1.1 4 in
  let app_1.l44 = print_int app_0.l26 in
  let EVALUATED_0.l78 = app_1.l44 in
  let app_2.l143 = (EVALUATED_0.l78 ( = ) ()) in
  let if_3.l190 = if app_2.l143 then () else let app_4.l122 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l122 in
  let _.l0 = if_3.l190 in
  0
PASS
  $ ./alpha_demo.exe < manytests/typed/009let_poly.ml
  Type inference before:
  val temp : int * bool
  
  Tuple expressions are not supported in ANF conversion

  $ ./alpha_demo.exe < manytests/typed/015tuples.ml
  Type inference before:
  val feven : 'a * int -> int -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : 'a -> 'b * 'a -> 'b -> 'a -> 'b * 'a -> 'b * 'a -> 'b -> 'a -> 'b -> 'a -> 'b * 'a -> 'b
  val fodd : int -> int * 'a -> int -> int
  val main : int
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : int -> int * int -> int
  
  Tuple expressions are not supported in ANF conversion

  $ ./alpha_demo.exe < manytests/typed/016lists.ml
  Type inference before:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> Unit) -> 'a list -> Unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  
  List expressions are not supported in ANF conversion

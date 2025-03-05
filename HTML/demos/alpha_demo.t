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
  
  let rec fac.1 n = let app_0.l1 = (n <= 1) in
  let if_1.l2 = if app_0.l1 then 1 else let app_2.l2 = (n - 1) in
  let app_3.l4 = fac.1 app_2.l2 in
  let app_4.l8 = (n * app_3.l4) in
  app_4.l8 in
  if_1.l2;;
  let main.1  = let app_0.l3 = fac.1 4 in
  let app_1.l4 = print_int app_0.l3 in
  let EVALUATED_0.l8 = app_1.l4 in
  let app_2.l18 = (EVALUATED_0.l8 ( = ) ()) in
  let if_3.l32 = if app_2.l18 then () else let app_4.l40 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l40 in
  let _.l0 = if_3.l32 in
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
  
  let cc_ll_0.1 n k p = let app_0.l1 = (p * n) in
  let app_1.l2 = k app_0.l1 in
  app_1.l2;;
  let rec fac_cps.1 n k = let app_0.l4 = (n = 1) in
  let if_1.l4 = if app_0.l4 then let app_2.l12 = k 1 in
  app_2.l12 else let app_3.l4 = (n - 1) in
  let app_4.l24 = cc_ll_0.2 n k in
  let app_5.l16 = fac_cps.1 app_3.l4 app_4.l24 in
  app_5.l16 in
  if_1.l4;;
  let cc_ll_1.1 print_int = print_int;;
  let main.1  = let app_0.l7 = fac_cps.1 4 cc_ll_1.1 in
  let app_1.l10 = print_int app_0.l7 in
  let EVALUATED_0.l16 = app_1.l10 in
  let app_2.l36 = (EVALUATED_0.l16 ( = ) ()) in
  let if_3.l64 = if app_2.l36 then () else let app_4.l72 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l72 in
  let _.l0 = if_3.l64 in
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
  
  let rec fib_acc.1 a b n = let app_0.l1 = (n = 1) in
  let if_1.l2 = if app_0.l1 then b else let app_2.l2 = (n - 1) in
  let n1.l4 = app_2.l2 in
  let app_3.l8 = (a + b) in
  let ab.l16 = app_3.l8 in
  let app_4.l32 = fib_acc.1 b ab.l16 n1.l4 in
  app_4.l32 in
  if_1.l2;;
  let rec fib.1 n = let app_0.l4 = (n < 2) in
  let if_1.l6 = if app_0.l4 then n else let app_2.l14 = (n - 1) in
  let app_3.l16 = fib.1 app_2.l14 in
  let app_4.l64 = (n - 2) in
  let app_5.l96 = fib.1 app_4.l64 in
  let app_6.l192 = (app_3.l16 + app_5.l96) in
  app_6.l192 in
  if_1.l6;;
  let main.1  = let app_0.l6 = fib_acc.1 0 1 4 in
  let app_1.l6 = print_int app_0.l6 in
  let EVALUATED_0.l12 = app_1.l6 in
  let app_2.l30 = (EVALUATED_0.l12 ( = ) ()) in
  let if_3.l48 = if app_2.l30 then () else let app_4.l96 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l96 in
  let _.l96 = if_3.l48 in
  let app_5.l224 = fib.1 4 in
  let app_6.l448 = print_int app_5.l224 in
  let EVALUATED_1.l768 = app_6.l448 in
  let app_7.l1536 = (EVALUATED_1.l768 ( = ) ()) in
  let if_8.l3072 = if app_7.l1536 then () else let app_9.l3072 = RTE_ERROR_MATCH_FAILURE () in
  app_9.l3072 in
  let _.l96 = if_8.l3072 in
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
  
  let wrap.1 f = let app_0.l1 = (1 = 1) in
  let if_1.l2 = if app_0.l1 then f else f in
  if_1.l2;;
  let test3.1 a b c = let app_0.l5 = print_int a in
  let a.l0 = app_0.l5 in
  let app_1.l14 = print_int b in
  let b.l0 = app_1.l14 in
  let app_2.l52 = print_int c in
  let c.l0 = app_2.l52 in
  0;;
  let test10.1 a b c d e f g h i j = let app_0.l8 = (a + b) in
  let app_1.l22 = (app_0.l8 + c) in
  let app_2.l76 = (app_1.l22 + d) in
  let app_3.l24 = (app_2.l76 + e) in
  let app_4.l112 = (app_3.l24 + f) in
  let app_5.l352 = (app_4.l112 + g) in
  let app_6.l192 = (app_5.l352 + h) in
  let app_7.l384 = (app_6.l192 + i) in
  let app_8.l768 = (app_7.l384 + j) in
  app_8.l768;;
  let main.1  = let app_0.l10 = wrap.1 test10.1 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
  let rez.l8 = app_0.l10 in
  let app_1.l30 = print_int rez.l8 in
  let EVALUATED_0.l32 = app_1.l30 in
  let app_2.l108 = (EVALUATED_0.l32 ( = ) ()) in
  let if_3.l128 = if app_2.l108 then () else let app_4.l176 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l176 in
  let _.l0 = if_3.l128 in
  let app_5.l608 = wrap.1 test3.1 1 10 100 in
  let temp2.l0 = app_5.l608 in
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
  
  let rec fix.1 f x = let app_0.l1 = fix.1 f in
  let app_1.l2 = f app_0.l1 x in
  app_1.l2;;
  let fac.1 self n = let app_0.l4 = (n <= 1) in
  let if_1.l4 = if app_0.l4 then 1 else let app_2.l12 = (n - 1) in
  let app_3.l8 = self app_2.l12 in
  let app_4.l32 = (n * app_3.l8) in
  app_4.l32 in
  if_1.l4;;
  let main.1  = let app_0.l6 = fix.1 fac.1 6 in
  let app_1.l8 = print_int app_0.l6 in
  let EVALUATED_0.l12 = app_1.l8 in
  let app_2.l28 = (EVALUATED_0.l12 ( = ) ()) in
  let if_3.l48 = if app_2.l28 then () else let app_4.l64 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l64 in
  let _.l0 = if_3.l48 in
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
  
  let cc_ll_0.1 foo = let app_0.l1 = (foo + 2) in
  app_0.l1;;
  let cc_ll_1.1 foo = let app_0.l5 = (foo * 10) in
  app_0.l5;;
  let foo.1 b = let if_0.l3 = if b then cc_ll_0.1 else cc_ll_1.1 in
  if_0.l3;;
  let foo.2 x = let app_0.l9 = foo.1 false x in
  let app_1.l12 = foo.1 true app_0.l9 in
  let app_2.l32 = foo.1 false app_1.l12 in
  let app_3.l32 = foo.1 true app_2.l32 in
  app_3.l32;;
  let main.1  = let app_0.l11 = foo.2 11 in
  let app_1.l16 = print_int app_0.l11 in
  let EVALUATED_0.l16 = app_1.l16 in
  let app_2.l48 = (EVALUATED_0.l16 ( = ) ()) in
  let if_3.l64 = if app_2.l48 then () else let app_4.l64 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l64 in
  let _.l0 = if_3.l64 in
  0

PASS
  $ ./alpha_demo.exe < manytests/typed/006partial2.ml
  Type inference before:
  val foo : int -> int -> int -> int
  val main : int
  
  Type inference after:
  val foo : int -> int -> int -> int
  val main : int
  
  let foo.1 a b c = let app_0.l1 = print_int a in
  let EVALUATED_0.l2 = app_0.l1 in
  let app_1.l4 = (EVALUATED_0.l2 ( = ) ()) in
  let if_2.l8 = if app_1.l4 then () else let app_3.l8 = RTE_ERROR_MATCH_FAILURE () in
  app_3.l8 in
  let _.l481 = if_2.l8 in
  let app_4.l32 = print_int b in
  let EVALUATED_1.l64 = app_4.l32 in
  let app_5.l128 = (EVALUATED_1.l64 ( = ) ()) in
  let if_6.l256 = if app_5.l128 then () else let app_7.l256 = RTE_ERROR_MATCH_FAILURE () in
  app_7.l256 in
  let _.l513 = if_6.l256 in
  let app_8.l1024 = print_int c in
  let EVALUATED_2.l2048 = app_8.l1024 in
  let app_9.l4096 = (EVALUATED_2.l2048 ( = ) ()) in
  let if_10.l8192 = if app_9.l4096 then () else let app_11.l8192 = RTE_ERROR_MATCH_FAILURE () in
  app_11.l8192 in
  let _.l513 = if_10.l8192 in
  let app_12.l32768 = (b * c) in
  let app_13.l65536 = (a + app_12.l32768) in
  app_13.l65536;;
  let main.1  = let app_0.l3 = foo.1 1 in
  let foo.l64 = app_0.l3 in
  let app_1.l12 = foo.l64 2 in
  let foo.l80 = app_1.l12 in
  let app_2.l32 = foo.l80 3 in
  let foo.l84 = app_2.l32 in
  let app_3.l136 = print_int foo.l84 in
  let EVALUATED_3.l256 = app_3.l136 in
  let app_4.l544 = (EVALUATED_3.l256 ( = ) ()) in
  let if_5.l1024 = if app_4.l544 then () else let app_6.l1024 = RTE_ERROR_MATCH_FAILURE () in
  app_6.l1024 in
  let _.l513 = if_5.l1024 in
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
  
  let cc_ll_0.1 c = let app_0.l1 = print_int c in
  app_0.l1;;
  let cc_ll_1.1 b = let app_0.l5 = print_int b in
  let EVALUATED_1.l4 = app_0.l5 in
  let app_1.l16 = (EVALUATED_1.l4 ( = ) ()) in
  let if_2.l32 = if app_1.l16 then () else let app_3.l32 = RTE_ERROR_MATCH_FAILURE () in
  app_3.l32 in
  let _.l0 = if_2.l32 in
  cc_ll_0.1;;
  let foo.1 a = let app_0.l8 = print_int a in
  let EVALUATED_0.l6 = app_0.l8 in
  let app_1.l28 = (EVALUATED_0.l6 ( = ) ()) in
  let if_2.l56 = if app_1.l28 then () else let app_3.l56 = RTE_ERROR_MATCH_FAILURE () in
  app_3.l56 in
  let _.l0 = if_2.l56 in
  cc_ll_1.1;;
  let main.1  = let app_0.l10 = foo.1 4 8 9 in
  let EVALUATED_2.l8 = app_0.l10 in
  let app_1.l36 = (EVALUATED_2.l8 ( = ) ()) in
  let if_2.l72 = if app_1.l36 then () else let app_3.l72 = RTE_ERROR_MATCH_FAILURE () in
  app_3.l72 in
  let _.l0 = if_2.l72 in
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
  
  let cc_ll_0.1 b a _c d __ = let app_0.l1 = (a + b) in
  let app_1.l2 = print_int app_0.l1 in
  let EVALUATED_4.l4 = app_1.l2 in
  let app_2.l8 = (EVALUATED_4.l4 ( = ) ()) in
  let if_3.l16 = if app_2.l8 then () else let app_4.l16 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l16 in
  let _.l32 = if_3.l16 in
  let app_5.l64 = print_int __ in
  let EVALUATED_5.l128 = app_5.l64 in
  let app_6.l256 = (EVALUATED_5.l128 ( = ) ()) in
  let if_7.l512 = if app_6.l256 then () else let app_8.l512 = RTE_ERROR_MATCH_FAILURE () in
  app_8.l512 in
  let _.l32 = if_7.l512 in
  let app_9.l2048 = (a * b) in
  let app_10.l4096 = (app_9.l2048 / _c) in
  let app_11.l8192 = (app_10.l4096 + d) in
  app_11.l8192;;
  let cc_ll_1.1 a b _c P3 = let app_0.l7 = (P3 ( = ) ()) in
  let if_1.l10 = if app_0.l7 then () else let app_2.l22 = RTE_ERROR_MATCH_FAILURE () in
  app_2.l22 in
  let _.l32 = if_1.l10 in
  let app_3.l32 = cc_ll_0.1 b a _c in
  app_3.l32;;
  let cc_ll_2.1 a P2 = let app_0.l12 = (P2 ( = ) ()) in
  let if_1.l18 = if app_0.l12 then () else let app_2.l34 = RTE_ERROR_MATCH_FAILURE () in
  app_2.l34 in
  let _.l32 = if_1.l18 in
  let app_3.l56 = cc_ll_1.1 a in
  app_3.l56;;
  let cc_ll_3.1 P1 = let app_0.l16 = (P1 ( = ) ()) in
  let if_1.l24 = if app_0.l16 then () else let app_2.l44 = RTE_ERROR_MATCH_FAILURE () in
  app_2.l44 in
  let _.l32 = if_1.l24 in
  cc_ll_2.1;;
  let _start.1 P0 = let app_0.l19 = (P0 ( = ) ()) in
  let if_1.l28 = if app_0.l19 then () else let app_2.l52 = RTE_ERROR_MATCH_FAILURE () in
  app_2.l52 in
  let _.l32 = if_1.l28 in
  cc_ll_3.1;;
  let main.1  = let app_0.l21 = print_int 1 in
  let app_1.l14 = print_int 2 in
  let app_2.l60 = print_int 4 in
  let app_3.l88 = - 1 in
  let app_4.l112 = print_int app_3.l88 in
  let app_5.l256 = - 555555 in
  let app_6.l640 = _start.1 app_0.l21 app_1.l14 3 app_2.l60 100 1000 app_4.l112 10000 app_5.l256 in
  let app_7.l768 = print_int app_6.l640 in
  app_7.l768

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
  
  let addi.1 f g x = let app_0.l1 = g x in
  let app_1.l2 = f x (app_0.l1 : bool) in
  (app_1.l2 : int);;
  let cc_ll_0.1 x b = let if_0.l2 = if b then let app_1.l8 = (x + 1) in
  app_1.l8 else let app_2.l10 = (x * 2) in
  app_2.l10 in
  if_0.l2;;
  let cc_ll_1.1 _start = let app_0.l6 = (_start / 2) in
  let app_1.l14 = (app_0.l6 = 0) in
  app_1.l14;;
  let main.1  = let app_0.l8 = addi.1 cc_ll_0.1 cc_ll_1.1 4 in
  let app_1.l18 = print_int app_0.l8 in
  let EVALUATED_0.l16 = app_1.l18 in
  let app_2.l34 = (EVALUATED_0.l16 ( = ) ()) in
  let if_3.l64 = if app_2.l34 then () else let app_4.l64 = RTE_ERROR_MATCH_FAILURE () in
  app_4.l64 in
  let _.l0 = if_3.l64 in
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

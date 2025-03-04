  $ ./anf_demo.exe < manytests/do_not_type/001.ml
  Type inference before:
  Typecheck error: Unbound value fac
  
  Type inference after:
  Typecheck error: Unbound value fac

  $ ./anf_demo.exe < manytests/do_not_type/002if.ml
  Type inference before:
  Typecheck error: This expression has type int but an expression was expected of type bool
  
  Type inference after:
  Typecheck error: This expression has type int but an expression was expected of type bool

  $ ./anf_demo.exe < manytests/do_not_type/003occurs.ml
  Type inference before:
  Typecheck error: Occurs check failed
  
  Type inference after:
  Typecheck error: Occurs check failed

  $ ./anf_demo.exe < manytests/do_not_type/004let_poly.ml
  Type inference before:
  Typecheck error: This expression has type bool but an expression was expected of type int
  
  Tuple expressions are not supported in ANF conversion

  $ ./anf_demo.exe < manytests/do_not_type/015tuples.ml
  Type inference before:
  Typecheck error: Only variables are allowed as left-hand side of `let rec'
  
  Tuple expressions are not supported in ANF conversion

PASS
  $ ./anf_demo.exe < manytests/typed/001fac.ml
  Type inference before:
  val fac : int -> int
  val main : int
  
  Type inference after:
  val fac : int -> int
  val main : int
  
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
  Type inference before:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Type inference after:
  val cc_ll_0 : int -> (int -> 'a) -> int -> 'a
  val cc_ll_1 : 'a -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
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
  Type inference before:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  
  Type inference after:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  
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
  Type inference before:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  
  Type inference after:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  
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
  Type inference before:
  val foo : int -> int
  val main : int
  
  Type inference after:
  val cc_ll_0 : int -> int
  val cc_ll_1 : int -> int
  val foo : int -> int
  val main : int
  
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
  Type inference before:
  val foo : int -> int -> int -> int
  val main : int
  
  Type inference after:
  val foo : int -> int -> int -> int
  val main : int
  
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
  Type inference before:
  val foo : int -> int -> int -> Unit
  val main : int
  
  Type inference after:
  val cc_ll_0 : int -> Unit
  val cc_ll_1 : int -> int -> Unit
  val foo : int -> int -> int -> Unit
  val main : int
  
  let cc_ll_0 c = let app_0 = print_int c in
  app_0;;
  let cc_ll_1 b = let app_0 = print_int b in
  let EVALUATED_1 = app_0 in
  let app_1 = (EVALUATED_1 ( = ) ()) in
  let if_2 = if app_1 then () else let app_3 = RTE_ERROR_MATCH_FAILURE () in
  app_3 in
  let _ = if_2 in
  cc_ll_0;;
  let foo a = let app_0 = print_int a in
  let EVALUATED_0 = app_0 in
  let app_1 = (EVALUATED_0 ( = ) ()) in
  let if_2 = if app_1 then () else let app_3 = RTE_ERROR_MATCH_FAILURE () in
  app_3 in
  let _ = if_2 in
  cc_ll_1;;
  let main  = let app_0 = foo 4 8 9 in
  let EVALUATED_2 = app_0 in
  let app_1 = (EVALUATED_2 ( = ) ()) in
  let if_2 = if app_1 then () else let app_3 = RTE_ERROR_MATCH_FAILURE () in
  app_3 in
  let _ = if_2 in
  0


PASS
  $ ./anf_demo.exe < manytests/typed/007order.ml
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
  let cc_ll_3 P1 = let app_0 = (P1 ( = ) ()) in
  let if_1 = if app_0 then () else let app_2 = RTE_ERROR_MATCH_FAILURE () in
  app_2 in
  let _ = if_1 in
  cc_ll_2;;
  let _start P0 = let app_0 = (P0 ( = ) ()) in
  let if_1 = if app_0 then () else let app_2 = RTE_ERROR_MATCH_FAILURE () in
  app_2 in
  let _ = if_1 in
  cc_ll_3;;
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
  Type inference before:
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int
  
  Type inference after:
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val cc_ll_0 : int -> bool -> int
  val cc_ll_1 : int -> bool
  val main : int
  
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
  Type inference before:
  val temp : int * bool
  
  Tuple expressions are not supported in ANF conversion

  $ ./anf_demo.exe < manytests/typed/015tuples.ml
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

  $ ./anf_demo.exe < manytests/typed/016lists.ml
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

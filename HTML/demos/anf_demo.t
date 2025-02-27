  $ ./anf_demo.exe < manytests/do_not_type/001.ml
  let recfac n =
       let app_0 = (n <= 1) in
       let if_1 = if app_0 then
                  1
                  else
                  let app_2 = (n - 1) in
                  let app_3 = fac app_2 in
                  let app_4 = (n * app_3) in
                  app_4 in
       if_1

  $ ./anf_demo.exe < manytests/do_not_type/002if.ml
  let main  =
       let if_0 = if true then
                  1
                  else
                  false in
       if_0

  $ ./anf_demo.exe < manytests/do_not_type/003occurs.ml
  let cc_ll_0 x f =
       let app_0 = x x f in
       app_0;;
  let cc_ll_1 f x =
       let app_0 = cc_ll_0 x in
       let app_1 = f app_0 in
       app_1;;let cc_ll_2 x f =
                   let app_0 = x x f in
                   app_0;;
  let cc_ll_3 f x =
       let app_0 = cc_ll_2 x in
       let app_1 = f app_0 in
       app_1;;
  let fix f =
       let app_0 = cc_ll_3 f in
       let app_1 = cc_ll_1 f app_0 in
       app_1

  $ ./anf_demo.exe < manytests/do_not_type/004let_poly.ml
  let cc_ll_0 f =
       let app_0 = f 1 in
       let app_1 = f true in
       (app_0, app_1);;let cc_ll_1 x =
                            x;;
  let temp  =
       let app_0 = cc_ll_0 cc_ll_1 in
       app_0

  $ ./anf_demo.exe < manytests/do_not_type/015tuples.ml
  let rec (a, b)  =
           (a, b)

PASS
  $ ./anf_demo.exe < manytests/typed/001fac.ml
  let rec fac n =
           let app_0 = (n <= 1) in
           let if_1 = if app_0 then
                      1
                      else
                      let app_2 = (n - 1) in
                      let app_3 = fac app_2 in
                      let app_4 = (n * app_3) in
                      app_4 in
           if_1;;
  let main  =
       let app_0 = fac 4 in
       let app_1 = print_int app_0 in
       let () = app_1 in
       0

PASS
  $ ./anf_demo.exe < manytests/typed/002fac.ml
  let cc_ll_0 n k p =
       let app_0 = (p * n) in
       let app_1 = k app_0 in
       app_1;;
  let rec fac_cps n k =
           let app_0 = (n = 1) in
           let if_1 = if app_0 then
                      let app_2 = k 1 in
                      app_2
                      else
                      let app_3 = (n - 1) in
                      let app_4 = cc_ll_0 n k in
                      let app_5 = fac_cps app_3 app_4 in
                      app_5 in
           if_1;;let cc_ll_1 print_int =
                      print_int;;
  let main  =
       let app_0 = fac_cps 4 cc_ll_1 in
       let app_1 = print_int app_0 in
       let () = app_1 in
       0

PASS
  $ ./anf_demo.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n =
           let app_0 = (n = 1) in
           let if_1 = if app_0 then
                      b
                      else
                      let app_2 = (n - 1) in
                      let n1 = app_2 in
                      let app_3 = (a + b) in
                      let ab = app_3 in
                      let app_4 = fib_acc b ab n1 in
                      app_4 in
           if_1;;
  let rec fib n =
           let app_0 = (n < 2) in
           let if_1 = if app_0 then
                      n
                      else
                      let app_2 = (n - 1) in
                      let app_3 = fib app_2 in
                      let app_4 = (n - 2) in
                      let app_5 = fib app_4 in
                      let app_6 = (app_3 + app_5) in
                      app_6 in
           if_1;;
  let main  =
       let app_0 = fib_acc 0 1 4 in
       let app_1 = print_int app_0 in
       let () = app_1 in
       let app_2 = fib 4 in
       let app_3 = print_int app_2 in
       let () = app_3 in
       0

PASS
  $ ./anf_demo.exe < manytests/typed/004manyargs.ml
  let wrap f =
       let app_0 = (1 = 1) in
       let if_1 = if app_0 then
                  f
                  else
                  f in
       if_1;;
  let test3 a b c =
       let app_0 = print_int a in
       let a = app_0 in
       let app_1 = print_int b in
       let b = app_1 in
       let app_2 = print_int c in
       let c = app_2 in
       0;;
  let test10 a b c d e f g h i j =
       let app_0 = (a + b) in
       let app_1 = (app_0 + c) in
       let app_2 = (app_1 + d) in
       let app_3 = (app_2 + e) in
       let app_4 = (app_3 + f) in
       let app_5 = (app_4 + g) in
       let app_6 = (app_5 + h) in
       let app_7 = (app_6 + i) in
       let app_8 = (app_7 + j) in
       app_8;;
  let main  =
       let app_0 = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
       let rez = app_0 in
       let app_1 = print_int rez in
       let () = app_1 in
       let app_2 = wrap test3 1 10 100 in
       let temp2 = app_2 in
       0

PASS
  $ ./anf_demo.exe < manytests/typed/005fix.ml
  let rec fix f x =
           let app_0 = fix f in
           let app_1 = f app_0 x in
           app_1;;
  let fac self n =
       let app_0 = (n <= 1) in
       let if_1 = if app_0 then
                  1
                  else
                  let app_2 = (n - 1) in
                  let app_3 = self app_2 in
                  let app_4 = (n * app_3) in
                  app_4 in
       if_1;;
  let main  =
       let app_0 = fix fac 6 in
       let app_1 = print_int app_0 in
       let () = app_1 in
       0

PASS
  $ ./anf_demo.exe < manytests/typed/006partial.ml
  let cc_ll_0 foo =
       let app_0 = (foo + 2) in
       app_0;;let cc_ll_1 foo =
                   let app_0 = (foo * 10) in
                   app_0;;
  let foo b =
       let if_0 = if b then
                  cc_ll_0
                  else
                  cc_ll_1 in
       if_0;;
  let foo x =
       let app_0 = foo false x in
       let app_1 = foo true app_0 in
       let app_2 = foo false app_1 in
       let app_3 = foo true app_2 in
       app_3;;
  let main  =
       let app_0 = foo 11 in
       let app_1 = print_int app_0 in
       let () = app_1 in
       0

PASS
  $ ./anf_demo.exe < manytests/typed/006partial2.ml
  let foo a b c =
       let app_0 = print_int a in
       let () = app_0 in
       let app_1 = print_int b in
       let () = app_1 in
       let app_2 = print_int c in
       let () = app_2 in
       let app_3 = (b * c) in
       let app_4 = (a + app_3) in
       app_4;;
  let main  =
       let app_0 = foo 1 in
       let foo = app_0 in
       let app_1 = foo 2 in
       let foo = app_1 in
       let app_2 = foo 3 in
       let foo = app_2 in
       let app_3 = print_int foo in
       let () = app_3 in
       0

PASS
  $ ./anf_demo.exe < manytests/typed/006partial3.ml
  let cc_ll_0 c =
       let app_0 = print_int c in
       app_0;;
  let cc_ll_1 b =
       let app_0 = print_int b in
       let () = app_0 in
       cc_ll_0;;
  let foo a =
       let app_0 = print_int a in
       let () = app_0 in
       cc_ll_1;;
  let main  =
       let app_0 = foo 4 8 9 in
       let () = app_0 in
       0


PASS
  $ ./anf_demo.exe < manytests/typed/007order.ml
  let _start () () a () b _c () d __ =
       let app_0 = (a + b) in
       let app_1 = print_int app_0 in
       let () = app_1 in
       let app_2 = print_int __ in
       let () = app_2 in
       let app_3 = (a * b) in
       let app_4 = (app_3 / _c) in
       let app_5 = (app_4 + d) in
       app_5;;
  let main  =
       let app_0 = print_int 1 in
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
  let addi f g x =
       let app_0 = g x in
       let app_1 = f x (app_0 : bool) in
       (app_1 : int);;
  let cc_ll_0 x b =
       let if_0 = if b then
                  let app_1 = (x + 1) in
                  app_1
                  else
                  let app_2 = (x * 2) in
                  app_2 in
       if_0;;
  let cc_ll_1 _start =
       let app_0 = (_start / 2) in
       let app_1 = (app_0 = 0) in
       app_1;;
  let main  =
       let app_0 = addi cc_ll_0 cc_ll_1 4 in
       let app_1 = print_int app_0 in
       let () = app_1 in
       0
PASS
  $ ./anf_demo.exe < manytests/typed/009let_poly.ml
  let cc_ll_0 x =
       x;;
  let temp  =
       let app_0 = cc_ll_0 1 in
       let app_1 = cc_ll_0 true in
       (app_0, app_1)

  $ ./anf_demo.exe < manytests/typed/015tuples.ml
  let rec fix f x =
           let app_0 = fix f in
           let app_1 = f app_0 x in
           app_1;;
  let map f p =
       let (a, b) = p in
       let app_0 = f a in
       let app_1 = f b in
       (app_0, app_1);;
  let cc_ll_0 self l li x =
       let app_0 = self l in
       let app_1 = li app_0 x in
       app_1;;
  let cc_ll_1 self l =
       let app_0 = cc_ll_0 self l in
       let app_1 = map app_0 l in
       app_1;;let fixpoly l =
                   let app_0 = fix cc_ll_1 l in
                   app_0;;
  let feven p n =
       let (e, o) = p in
       let app_0 = (n == 0) in
       let if_1 = if app_0 then
                  1
                  else
                  let app_2 = (n - 1) in
                  let app_3 = o app_2 in
                  app_3 in
       if_1;;
  let fodd p n =
       let (e, o) = p in
       let app_0 = (n == 0) in
       let if_1 = if app_0 then
                  0
                  else
                  let app_2 = (n - 1) in
                  let app_3 = e app_2 in
                  app_3 in
       if_1;;let tie  =
                  let app_0 = fixpoly (feven, fodd) in
                  app_0;;
  let rec meven n =
           let app_0 = (n = 0) in
           let if_1 = if app_0 then
                      1
                      else
                      let app_2 = (n - 1) in
                      let app_3 = modd app_2 in
                      app_3 in
           if_1
  and modd n =
       let app_4 = (n = 0) in
       let if_5 = if app_4 then
                  1
                  else
                  let app_6 = (n - 1) in
                  let app_7 = meven app_6 in
                  app_7 in
       if_5;;
  let main  =
       let app_0 = modd 1 in
       let app_1 = print_int app_0 in
       let () = app_1 in
       let app_2 = meven 2 in
       let app_3 = print_int app_2 in
       let () = app_3 in
       let (even, odd) = tie in
       let app_4 = odd 3 in
       let app_5 = print_int app_4 in
       let () = app_5 in
       let app_6 = even 4 in
       let app_7 = print_int app_6 in
       let () = app_7 in
       0

  $ ./anf_demo.exe < manytests/typed/016lists.ml
  Fatal error: exception Failure("not implemented / needed")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Anf__Anf_conv.anf_decl.helper in file "lib/anf/anf_conv.ml", line 109, characters 4-78
  Called from Anf__Anf_conv.anf_decl in file "lib/anf/anf_conv.ml", line 115, characters 17-26
  Called from Anf__Anf_conv.anf_program.(fun) in file "lib/anf/anf_conv.ml", line 132, characters 37-57
  Called from Stdlib__List.map in file "list.ml", line 92, characters 20-23
  Called from Dune__exe__Anf_demo.anf_demo in file "demos/anf_demo.ml", line 5, characters 15-44
  [2]

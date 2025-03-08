
  $ ./anf_demo.exe << EOF
  > let sum = let a x = (fun y -> x + y) 2 in a 1
  let  bin_op x y = (x + y)
  let  lambada x y = ((bin_op x) y)
  let  a x = ((lambada x) 2)
  let  sum  = (a 1)

  $ ./anf_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  let  test2 x y i = (x, y, i)
  let  test1 x y = ((test2 x) y)

  $ ./anf_demo.exe << EOF
  > let odin = 2
  > let vosem = 
  >   let odin = 1 in 
  >   let dva = 2 in
  >   let odin_plus_dva_plus x = x + (fun i -> odin + dva + i) 0 in
  >   odin_plus_dva_plus 5
  > let tri = odin
  let  odin  = 2
  let  odin0  = 1
  let  dva  = 2
  let  bin_op0  = (odin0 + dva)
  let  bin_op i = (bin_op0 + i)
  let  lambada i = (bin_op i)
  let  app  = (lambada 0)
  let  bin_op1 x = (x + app)
  let  odin_plus_dva_plus x = (bin_op1 x)
  let  vosem  = (odin_plus_dva_plus 5)
  let  tri  = odin

  $ ./anf_demo.exe << EOF
  > let b = 6;;
  > let i = 1;;
  > let sem = (fun num1 -> (fun num2 -> num1 + num2)) b i
  let  b  = 6
  let  i  = 1
  let  bin_op num1 num2 = (num1 + num2)
  let  lambada num1 num2 = ((bin_op num1) num2)
  let  sem  = ((lambada b) i)
  $ ./anf_demo.exe << EOF
  > let fac_cps num =
  >   let rec helper num acc =
  >     if num = 0
  >     then acc 1
  >     else helper (num - 1) (fun t -> acc (num * t))
  >   in
  >   helper num (fun x -> x) 
  let  lambada0 x = x
  let  bin_op num t = (num * t)
  let  lambada acc num t = (acc ((bin_op num) t))
  let  bin_op0 num = (num = 0)
  let  app acc num = ((lambada acc) num)
  let  bin_op1 num = (num - 1)
  let rec helper num acc = 
    if (bin_op0 num)
    then (acc 1)
    else ((helper (bin_op1 num)) ((app acc) num))
  let  fac_cps num = ((helper num) lambada0)

  $ ./anf_demo.exe < manytests/typed/001fac.ml
  let  bin_op n = (n <= 1)
  let  bin_op0 n = (n - 1)
  let  app fac n = (fac (bin_op0 n))
  let  bin_op1 fac n = (n * ((app fac) n))
  let rec fac n = 
    if (bin_op n)
    then 1
    else ((bin_op1 fac) n)
  let  app0  = (fac 4)
  let  unit  = (print_int app0)
  let  main  = unit; 0
  $ ./anf_demo.exe < manytests/typed/002fac.ml
  let  bin_op n p = (p * n)
  let  lambada k n p = (k ((bin_op n) p))
  let  bin_op0 n = (n = 1)
  let  app k n = ((lambada k) n)
  let  bin_op1 n = (n - 1)
  let rec fac_cps n k = 
    if (bin_op0 n)
    then (k 1)
    else ((fac_cps (bin_op1 n)) ((app k) n))
  let  lambada0 print_int = print_int
  let  app0  = ((fac_cps 4) lambada0)
  let  unit  = (print_int app0)
  let  main  = unit; 0
  $ ./anf_demo.exe < manytests/typed/003fib.ml
  let  bin_op n = (n - 1)
  let  n1 n = (bin_op n)
  let  bin_op0 a b = (a + b)
  let  ab a b = ((bin_op0 a) b)
  let  bin_op1 n = (n = 1)
  let  app n = (n1 n)
  let  app0 a b = ((ab a) b)
  let rec fib_acc a b n = 
    if (bin_op1 n)
    then b
    else (((fib_acc b) ((app0 a) b)) (app n))
  let  bin_op2 n = (n < 2)
  let  bin_op3 n = (n - 2)
  let  app1 fib n = (fib (bin_op3 n))
  let  bin_op5 n = (n - 1)
  let  bin_op4 fib n = ((fib (bin_op5 n)) + ((app1 fib) n))
  let rec fib n = 
    if (bin_op2 n)
    then n
    else ((bin_op4 fib) n)
  let  app2  = (((fib_acc 0) 1) 4)
  let  unit  = (print_int app2)
  let  app3  = (fib 4)
  let  unit0  = (print_int app3)
  let  main  = unit; unit0; 0
  $ ./anf_demo.exe < manytests/typed/004manyargs.ml
  let  bin_op  = (1 = 1)
  let  wrap f = 
    if bin_op
    then f
    else f
  let  a0 a = (print_int a)
  let  b0 b = (print_int b)
  let  c0 c = (print_int c)
  let  test3 a b c = 0
  let  bin_op8 a b = (a + b)
  let  bin_op7 a b c = (((bin_op8 a) b) + c)
  let  bin_op6 a b c d = ((((bin_op7 a) b) c) + d)
  let  bin_op5 a b c d e = (((((bin_op6 a) b) c) d) + e)
  let  bin_op4 a b c d e f = ((((((bin_op5 a) b) c) d) e) + f)
  let  bin_op3 a b c d e f g = (((((((bin_op4 a) b) c) d) e) f) + g)
  let  bin_op2 a b c d e f g h = ((((((((bin_op3 a) b) c) d) e) f) g) + h)
  let  bin_op1 a b c d e f g h i = (((((((((bin_op2 a) b) c) d) e) f) g) h) + i)
  let  bin_op0 a b c d e f g h i j = ((((((((((bin_op1 a) b) c) d) e) f) g) h) i) + j)
  let  test10 a b c d e f g h i j = ((((((((((bin_op0 a) b) c) d) e) f) g) h) i) j)
  let  rez  = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
  let  unit  = (print_int rez)
  let  temp2  = ((((wrap test3) 1) 10) 100)
  let  main  = unit; 0
  $ ./anf_demo.exe < manytests/typed/005fix.ml
  let  app f fix = (fix f)
  let rec fix f x = ((f ((app f) fix)) x)
  let  bin_op n = (n <= 1)
  let  bin_op0 n = (n - 1)
  let  app0 n self = (self (bin_op0 n))
  let  bin_op1 n self = (n * ((app0 n) self))
  let  fac self n = 
    if (bin_op n)
    then 1
    else ((bin_op1 n) self)
  let  app1  = ((fix fac) 6)
  let  unit  = (print_int app1)
  let  main  = unit; 0
  $ ./anf_demo.exe < manytests/typed/006partial.ml
  let  bin_op foo = (foo + 2)
  let  lambada foo = (bin_op foo)
  let  bin_op0 foo = (foo * 10)
  let  lambada0 foo = (bin_op0 foo)
  let  foo b = 
    if b
    then lambada
    else lambada0
  let  app0 x = ((foo false) x)
  let  app x = ((foo false) ((foo true) (app0 x)))
  let  foo0 x = ((foo true) (app x))
  let  app1  = (foo0 11)
  let  unit  = (print_int app1)
  let  main  = unit; 0
  $ ./anf_demo.exe < manytests/typed/006partial2.ml
  let  unit a = (print_int a)
  let  unit0 b = (print_int b)
  let  unit1 c = (print_int c)
  let  bin_op0 b c = (b * c)
  let  bin_op a b c = (a + ((bin_op0 b) c))
  let  foo a b c = (unit a); (unit0 b); (unit1 c); (((bin_op a) b) c)
  let  foo0  = (foo 1)
  let  foo1  = (foo0 2)
  let  foo2  = (foo1 3)
  let  unit2  = (print_int foo2)
  let  main  = unit2; 0
  $ ./anf_demo.exe < manytests/typed/006partial3.ml
  let  lambada0 print_int c = (print_int c)
  let  unit b = (print_int b)
  let  lambada b = (unit b); (lambada0 print_int)
  let  unit0 a = (print_int a)
  let  foo a = (unit0 a); lambada
  let  unit1  = (((foo 4) 8) 9)
  let  main  = unit1; 0
  $ ./anf_demo.exe < manytests/typed/007order.ml
  let  bin_op a b = (a + b)
  let  unit a b = (print_int ((bin_op a) b))
  let  unit0 __ = (print_int __)
  let  bin_op2 a b = (a * b)
  let  bin_op1 _c a b = (((bin_op2 a) b) / _c)
  let  bin_op0 _c a b d = ((((bin_op1 _c) a) b) + d)
  let  _start () () a () b _c () d __ = ((unit a) b); (unit0 __); ((((bin_op0 _c) a) b) d)
  let  app0  = (print_int -1)
  let  app1  = (print_int 4)
  let  app2  = (print_int 2)
  let  app3  = (print_int 1)
  let  app  = (((((((((_start app3) app2) 3) app1) 100) 1000) app0) 10000) -555555)
  let  main  = (print_int app)
  $ ./anf_demo.exe < manytests/typed/008ascription.ml
  let  app g x = (g x)
  let  addi f g x = ((f x) ((app g) x))
  let  bin_op x = (x + 1)
  let  bin_op0 x = (x * 2)
  let  lambada x b = 
    if b
    then (bin_op x)
    else (bin_op0 x)
  let  bin_op2 _start = (_start / 2)
  let  bin_op1 _start = ((bin_op2 _start) = 0)
  let  lambada0 _start = (bin_op1 _start)
  let  app0  = (((addi lambada) lambada0) 4)
  let  unit  = (print_int app0)
  let  main  = unit; 0
  $ ./anf_demo.exe < manytests/typed/009let_poly.ml
  let  f x = x
  let  temp  = ((f 1), (f true))

  $ ./anf_demo.exe << EOF
  > let a = 5
  > let sum a = 
  >   let a = a + 3 in a
  > let b = a
  let  a  = 5
  let  bin_op a0 = (a0 + 3)
  let  a1 a0 = (bin_op a0)
  let  sum a0 = (a1 a0)
  let  b  = a

  $ ./anf_demo.exe << EOF
  > let a = 5
  > let sum a = 
  >   let ((), a) = (print_int 4, a) in a
  > let b = a
  let  a  = 5
  let  sum a0 = 
    let ((), a1) = ((print_int 4), a0)
    in a1
  let  b  = a

  $ ./anf_demo.exe << EOF
  > let sum a b = a + b 
  > let add a = match a with
  > | 1 -> sum 1 0
  > | 2 -> sum 2 0
  > | 3 -> sum 3 0
  > | 4 -> sum 4 0
  > | 5 -> sum 5 0
  > | _ -> sum 0 0
  >   
  > let main = 
  >   let () = print_int (add 0) in
  > 0
  let  bin_op a b = (a + b)
  let  sum a b = ((bin_op a) b)
  let  add a = 
    if (a = 1)
    then ((sum 1) 0)
    else 
    if (a = 2)
    then ((sum 2) 0)
    else 
    if (a = 3)
    then ((sum 3) 0)
    else 
    if (a = 4)
    then ((sum 4) 0)
    else 
    if (a = 5)
    then ((sum 5) 0)
    else ((sum 0) 0)
  let  app  = (add 0)
  let  unit  = (print_int app)
  let  main  = unit; 0

  $ ./anf_demo.exe << EOF
  > let bin_op = 1 + 2
  > let bin_op0 = 4 - 1
  > let ogogo = 1 + 2 + 3 + bin_op + bin_op0
  > let main = print_int(ogogo)
  let  bin_op  = (1 + 2)
  let  bin_op0  = bin_op
  let  bin_op1  = (4 - 1)
  let  bin_op00  = bin_op1
  let  bin_op5  = (1 + 2)
  let  bin_op4  = (bin_op5 + 3)
  let  bin_op3  = (bin_op4 + bin_op0)
  let  bin_op2  = (bin_op3 + bin_op00)
  let  ogogo  = bin_op2
  let  main  = (print_int ogogo)

  $ ./anf_demo.exe << EOF
  > let app a b = a + b
  > let app1 a b = a - b
  > let ogogo = app (app1 3 2) (app 1 1)
  > let main = print_int(ogogo)
  let  bin_op a b = (a + b)
  let  app a b = ((bin_op a) b)
  let  bin_op0 a b = (a - b)
  let  app1 a b = ((bin_op0 a) b)
  let  app0  = ((app 1) 1)
  let  app2  = ((app1 3) 2)
  let  ogogo  = ((app app2) app0)
  let  main  = (print_int ogogo)

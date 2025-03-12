
  $ ./anf_demo.exe << EOF
  > let sum a b c d = a + b + c + d
  > let main = print_int(sum 1 2 3 4)
  Types: 
  val main: unit
  val sum: int -> int -> int -> int -> int
  
  let  sum a b c d = 
    let bin_op1 = (a + b)
    in 
    let bin_op0 = (bin_op1 + c)
    in 
    let bin_op = (bin_op0 + d)
    in bin_op
  let  main  = 
    let app = ((((sum 1) 2) 3) 4)
    in (print_int app)

  $ ./anf_demo.exe << EOF
  > let sum = let a x = (fun y -> x + y) 2 in a 1
  Types: 
  val a: int -> int
  val lambada: int -> int -> int
  val sum: int
  
  let  lambada x y = 
    let bin_op = (x + y)
    in bin_op
  let  a x = ((lambada x) 2)
  let  sum  = (a 1)

  $ ./anf_demo.exe << EOF
  > let odin = 2
  > let vosem = 
  >   let odin = 1 in 
  >   let dva = 2 in
  >   let odin_plus_dva_plus = (fun i -> odin + dva + i) 0 in
  >   odin_plus_dva_plus
  > let tri = odin
  Types: 
  val lambada: int -> int -> int -> int
  val odin: int
  val tri: int
  val vosem: int
  
  let  odin  = 2
  let  lambada dva odin0 i = 
    let bin_op0 = (odin0 + dva)
    in 
    let bin_op = (bin_op0 + i)
    in bin_op
  let  vosem  = 
    let odin0 = 1
    in 
    let dva = 2
    in 
    let odin_plus_dva_plus = (((lambada dva) odin0) 0)
    in odin_plus_dva_plus
  let  tri  = odin

  $ ./anf_demo.exe << EOF
  > let b = 6;;
  > let i = 1;;
  > let sem = (fun num1 -> (fun num2 -> num1 + num2)) b i
  Types: 
  val b: int
  val i: int
  val lambada: int -> int -> int
  val sem: int
  
  let  b  = 6
  let  i  = 1
  let  lambada num1 num2 = 
    let bin_op = (num1 + num2)
    in bin_op
  let  sem  = ((lambada b) i)
  $ ./anf_demo.exe << EOF
  > let fac_cps num =
  >   let rec helper num acc =
  >     if num = 0
  >     then acc 1
  >     else helper (num - 1) (fun t -> acc (num * t))
  >   in
  >   helper num (fun x -> x) 
  Types: 
  val fac_cps: int -> int
  val helper: int -> (int -> 'a) -> 'a
  val lambada: (int -> 'a) -> int -> int -> 'a
  val lambada0: 'a -> 'a
  
  let  lambada0 x = x
  let  lambada acc num t = 
    let bin_op = (num * t)
    in (acc bin_op)
  let rec helper num acc = 
    let bin_op = (num = 0)
    in 
    if bin_op
    then (acc 1)
    else 
    let bin_op0 = (num - 1)
    in 
    let app = ((lambada acc) num)
    in ((helper bin_op0) app)
  let  fac_cps num = ((helper num) lambada0)

  $ ./anf_demo.exe < manytests/typed/001fac.ml
  Types: 
  val fac: int -> int
  val main: int
  
  let rec fac n = 
    let bin_op = (n <= 1)
    in 
    if bin_op
    then 1
    else 
    let bin_op1 = (n - 1)
    in 
    let bin_op0 = (n * (fac bin_op1))
    in bin_op0
  let  main  = 
    let app = (fac 4)
    in 
    let () = (print_int app)
    in 0
  $ ./anf_demo.exe < manytests/typed/002fac.ml
  Types: 
  val fac_cps: int -> (int -> 'a) -> 'a
  val lambada: (int -> 'a) -> int -> int -> 'a
  val lambada0: 'a -> 'a
  val main: int
  
  let  lambada k n p = 
    let bin_op = (p * n)
    in (k bin_op)
  let rec fac_cps n k = 
    let bin_op = (n = 1)
    in 
    if bin_op
    then (k 1)
    else 
    let bin_op0 = (n - 1)
    in 
    let app = ((lambada k) n)
    in ((fac_cps bin_op0) app)
  let  lambada0 print_int = print_int
  let  main  = 
    let app = ((fac_cps 4) lambada0)
    in 
    let () = (print_int app)
    in 0
  $ ./anf_demo.exe < manytests/typed/003fib.ml
  Types: 
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  val main: int
  
  let rec fib_acc a b n = 
    let bin_op = (n = 1)
    in 
    if bin_op
    then b
    else 
    let bin_op0 = (n - 1)
    in 
    let bin_op1 = (a + b)
    in 
    let n1 = bin_op0
    in 
    let ab = bin_op1
    in (((fib_acc b) ab) n1)
  let rec fib n = 
    let bin_op = (n < 2)
    in 
    if bin_op
    then n
    else 
    let bin_op1 = (n - 1)
    in 
    let bin_op2 = (n - 2)
    in 
    let bin_op0 = ((fib bin_op1) + (fib bin_op2))
    in bin_op0
  let  main  = 
    let app = (((fib_acc 0) 1) 4)
    in 
    let app0 = (fib 4)
    in 
    let () = (print_int app)
    in 
    let () = (print_int app0)
    in 0
  $ ./anf_demo.exe < manytests/typed/004manyargs.ml
  Types: 
  val main: int
  val test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3: int -> int -> int -> int
  val wrap: 'a -> 'a
  
  let  wrap f = 
    let bin_op = (1 = 1)
    in 
    if bin_op
    then f
    else f
  let  test3 a b c = 
    let a0 = (print_int a)
    in 
    let b0 = (print_int b)
    in 
    let c0 = (print_int c)
    in 0
  let  test10 a b c d e f g h i j = 
    let bin_op7 = (a + b)
    in 
    let bin_op6 = (bin_op7 + c)
    in 
    let bin_op5 = (bin_op6 + d)
    in 
    let bin_op4 = (bin_op5 + e)
    in 
    let bin_op3 = (bin_op4 + f)
    in 
    let bin_op2 = (bin_op3 + g)
    in 
    let bin_op1 = (bin_op2 + h)
    in 
    let bin_op0 = (bin_op1 + i)
    in 
    let bin_op = (bin_op0 + j)
    in bin_op
  let  main  = 
    let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
    in 
    let () = (print_int rez)
    in 
    let temp2 = ((((wrap test3) 1) 10) 100)
    in 0
  $ ./anf_demo.exe < manytests/typed/005fix.ml
  Types: 
  val fac: (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main: int
  
  let rec fix f x = 
    let app = (fix f)
    in ((f app) x)
  let  fac self n = 
    let bin_op = (n <= 1)
    in 
    if bin_op
    then 1
    else 
    let bin_op1 = (n - 1)
    in 
    let bin_op0 = (n * (self bin_op1))
    in bin_op0
  let  main  = 
    let app = ((fix fac) 6)
    in 
    let () = (print_int app)
    in 0
  $ ./anf_demo.exe < manytests/typed/006partial.ml
  Types: 
  val foo: bool -> int -> int
  val foo0: int -> int
  val lambada: int -> int
  val lambada0: int -> int
  val main: int
  
  let  lambada foo = 
    let bin_op = (foo + 2)
    in bin_op
  let  lambada0 foo = 
    let bin_op = (foo * 10)
    in bin_op
  let  foo b = 
    if b
    then lambada
    else lambada0
  let  foo0 x = 
    let app0 = ((foo false) x)
    in 
    let app = ((foo false) ((foo true) app0))
    in ((foo true) app)
  let  main  = 
    let app = (foo0 11)
    in 
    let () = (print_int app)
    in 0
  $ ./anf_demo.exe < manytests/typed/006partial2.ml
  Types: 
  val foo: int -> int -> int -> int
  val main: int
  
  let  foo a b c = 
    let bin_op0 = (b * c)
    in 
    let bin_op = (a + bin_op0)
    in 
    let () = (print_int a)
    in 
    let () = (print_int b)
    in 
    let () = (print_int c)
    in bin_op
  let  main  = 
    let foo0 = (foo 1)
    in 
    let foo1 = (foo0 2)
    in 
    let foo2 = (foo1 3)
    in 
    let () = (print_int foo2)
    in 0
  $ ./anf_demo.exe < manytests/typed/006partial3.ml
  Types: 
  val foo: int -> int -> int -> unit
  val lambada: int -> int -> unit
  val lambada0: ('a -> 'b) -> 'a -> 'b
  val main: int
  
  let  lambada0 print_int c = (print_int c)
  let  lambada b = 
    let () = (print_int b)
    in (lambada0 print_int)
  let  foo a = 
    let () = (print_int a)
    in lambada
  let  main  = 
    let () = (((foo 4) 8) 9)
    in 0
  $ ./anf_demo.exe < manytests/typed/007order.ml
  Types: 
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit
  
  let  _start () () a () b _c () d __ = 
    let bin_op = (a + b)
    in 
    let bin_op2 = (a * b)
    in 
    let bin_op1 = (bin_op2 / _c)
    in 
    let bin_op0 = (bin_op1 + d)
    in 
    let () = (print_int bin_op)
    in 
    let () = (print_int __)
    in bin_op0
  let  main  = 
    let app3 = (print_int 1)
    in 
    let app2 = (print_int 2)
    in 
    let app1 = (print_int 4)
    in 
    let app0 = (print_int -1)
    in 
    let app = (((((((((_start app3) app2) 3) app1) 100) 1000) app0) 10000) -555555)
    in (print_int app)
  $ ./anf_demo.exe < manytests/typed/008ascription.ml
  Types: 
  val addi: ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val lambada: int -> bool -> int
  val lambada0: int -> bool
  val main: int
  
  let  addi f g x = 
    let app = (g x)
    in ((f x) app)
  let  lambada x b = 
    if b
    then 
    let bin_op = (x + 1)
    in bin_op
    else 
    let bin_op0 = (x * 2)
    in bin_op0
  let  lambada0 _start = 
    let bin_op0 = (_start / 2)
    in 
    let bin_op = (bin_op0 = 0)
    in bin_op
  let  main  = 
    let app = (((addi lambada) lambada0) 4)
    in 
    let () = (print_int app)
    in 0

  $ ./anf_demo.exe << EOF
  > let a = 5
  > let sum a = 
  >   let a = a + 3 in a
  > let b = a
  Types: 
  val a: int
  val b: int
  val sum: int -> int
  
  let  a  = 5
  let  sum a0 = 
    let bin_op = (a0 + 3)
    in 
    let a1 = bin_op
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
  Types: 
  val add: int -> int
  val main: int
  val sum: int -> int -> int
  
  let  sum a b = 
    let bin_op = (a + b)
    in bin_op
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
  let  main  = 
    let app = (add 0)
    in 
    let () = (print_int app)
    in 0

  $ ./anf_demo.exe << EOF
  > let bin_op = 1 + 2
  > let bin_op0 = 4 - 1
  > let ogogo = 1 + 2 + 3 + bin_op + bin_op0
  > let main = print_int(ogogo)
  Types: 
  val bin_op: int
  val bin_op0: int
  val main: unit
  val ogogo: int
  
  let  bin_op  = 
    let bin_op0 = (1 + 2)
    in bin_op0
  let  bin_op0  = 
    let bin_op1 = (4 - 1)
    in bin_op1
  let  ogogo  = 
    let bin_op4 = (1 + 2)
    in 
    let bin_op3 = (bin_op4 + 3)
    in 
    let bin_op2 = (bin_op3 + bin_op)
    in 
    let bin_op1 = (bin_op2 + bin_op0)
    in bin_op1
  let  main  = (print_int ogogo)

  $ ./anf_demo.exe << EOF
  > let app a b = a + b
  > let app1 a b = a - b
  > let ogogo = app (app1 3 2) (app 1 1)
  > let main = print_int(ogogo)
  Types: 
  val app: int -> int -> int
  val app1: int -> int -> int
  val main: unit
  val ogogo: int
  
  let  app a b = 
    let bin_op = (a + b)
    in bin_op
  let  app1 a b = 
    let bin_op = (a - b)
    in bin_op
  let  ogogo  = 
    let app2 = ((app1 3) 2)
    in 
    let app0 = ((app 1) 1)
    in ((app app2) app0)
  let  main  = (print_int ogogo)

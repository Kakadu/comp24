
  $ ./lambdalift_demo.exe << EOF
  > let sum a b c d = a + b + c + d
  > let main = print_int(sum 1 2 3 4)
  let  sum a b c d  = (((a + b) + c) + d)
  let  main  = (print_int ((((sum 1) 2) 3) 4))

  $ ./lambdalift_demo.exe << EOF
  > let sum = let a x = (fun y -> x + y) 2 in a 1
  let  lambada x y  = (x + y)
  let  a x  = ((lambada x) 2)
  let  sum  = (a 1)

  $ ./lambdalift_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  let  test2 x y i  = (x, y, i)
  let  test1 (x, y)  = ((test2 x) y)

  $ ./lambdalift_demo.exe << EOF
  > let odin = 2
  > let vosem = 
  >   let odin = 1 in 
  >   let dva = 2 in
  >   let odin_plus_dva_plus x = x + (fun i -> odin + dva + i) 0 in
  >   odin_plus_dva_plus 5
  > let tri = odin
  let  odin  = 2
  let  lambada dva odin0 i  = ((odin0 + dva) + i)
  let  odin_plus_dva_plus dva odin0 x  = (x + (((lambada dva) odin0) 0))
  let  vosem  = 
    let odin0 = 1
    in 
    let dva = 2
    in (((odin_plus_dva_plus dva) odin0) 5)
  let  tri  = odin

  $ ./lambdalift_demo.exe << EOF
  > let b = 6;;
  > let i = 1;;
  > let sem = (fun num1 -> (fun num2 -> num1 + num2)) b i
  let  b  = 6
  let  i  = 1
  let  lambada num1 num2  = (num1 + num2)
  let  sem  = ((lambada b) i)
  $ ./lambdalift_demo.exe << EOF
  > let fac_cps num =
  >   let rec helper num acc =
  >     if num = 0
  >     then acc 1
  >     else helper (num - 1) (fun t -> acc (num * t))
  >   in
  >   helper num (fun x -> x) 
  let  lambada0 x  = x
  let  lambada acc num t  = (acc (num * t))
  let rec helper num acc  = 
    if (num = 0)
    then (acc 1)
    else ((helper (num - 1)) ((lambada acc) num))
  let  fac_cps num  = ((helper num) lambada0)

  $ ./lambdalift_demo.exe < manytests/typed/001fac.ml
  let rec fac n  = 
    if (n <= 1)
    then 1
    else (n * (fac (n - 1)))
  let  main  = 
    let () = (print_int (fac 4))
    in 0
  $ ./lambdalift_demo.exe < manytests/typed/002fac.ml
  let  lambada k n p  = (k (p * n))
  let rec fac_cps n k  = 
    if (n = 1)
    then (k 1)
    else ((fac_cps (n - 1)) ((lambada k) n))
  let  lambada0 print_int  = print_int
  let  main  = 
    let () = (print_int ((fac_cps 4) lambada0))
    in 0
  $ ./lambdalift_demo.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n  = 
    if (n = 1)
    then b
    else 
    let n1 = (n - 1)
    in 
    let ab = (a + b)
    in (((fib_acc b) ab) n1)
  let rec fib n  = 
    if (n < 2)
    then n
    else ((fib (n - 1)) + (fib (n - 2)))
  let  main  = 
    let () = (print_int (((fib_acc 0) 1) 4))
    in 
    let () = (print_int (fib 4))
    in 0
  $ ./lambdalift_demo.exe < manytests/typed/004manyargs.ml
  let  wrap f  = 
    if (1 = 1)
    then f
    else f
  let  test3 a b c  = 
    let a0 = (print_int a)
    in 
    let b0 = (print_int b)
    in 
    let c0 = (print_int c)
    in 0
  let  test10 a b c d e f g h i j  = (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j)
  let  main  = 
    let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
    in 
    let () = (print_int rez)
    in 
    let temp2 = ((((wrap test3) 1) 10) 100)
    in 0
  $ ./lambdalift_demo.exe < manytests/typed/005fix.ml
  let rec fix f x  = ((f (fix f)) x)
  let  fac self n  = 
    if (n <= 1)
    then 1
    else (n * (self (n - 1)))
  let  main  = 
    let () = (print_int ((fix fac) 6))
    in 0
  $ ./lambdalift_demo.exe < manytests/typed/006partial.ml
  let  lambada foo  = (foo + 2)
  let  lambada0 foo  = (foo * 10)
  let  foo b  = 
    if b
    then lambada
    else lambada0
  let  foo0 x  = ((foo true) ((foo false) ((foo true) ((foo false) x))))
  let  main  = 
    let () = (print_int (foo0 11))
    in 0
  $ ./lambdalift_demo.exe < manytests/typed/006partial2.ml
  let  foo a b c  = 
    let () = (print_int a)
    in 
    let () = (print_int b)
    in 
    let () = (print_int c)
    in (a + (b * c))
  let  main  = 
    let foo0 = (foo 1)
    in 
    let foo1 = (foo0 2)
    in 
    let foo2 = (foo1 3)
    in 
    let () = (print_int foo2)
    in 0
  $ ./lambdalift_demo.exe < manytests/typed/006partial3.ml
  let  lambada0 print_int c  = (print_int c)
  let  lambada b  = 
    let () = (print_int b)
    in (lambada0 print_int)
  let  foo a  = 
    let () = (print_int a)
    in lambada
  let  main  = 
    let () = (((foo 4) 8) 9)
    in 0
  $ ./lambdalift_demo.exe < manytests/typed/007order.ml
  let  _start () () a () b _c () d __  = 
    let () = (print_int (a + b))
    in 
    let () = (print_int __)
    in (((a * b) / _c) + d)
  let  main  = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int -1)) 10000) -555555))
  $ ./lambdalift_demo.exe < manytests/typed/008ascription.ml
  let  addi f g x  = ((f x) (g x))
  let  lambada x b  = 
    if b
    then (x + 1)
    else (x * 2)
  let  lambada0 _start  = ((_start / 2) = 0)
  let  main  = 
    let () = (print_int (((addi lambada) lambada0) 4))
    in 0
  $ ./lambdalift_demo.exe < manytests/typed/009let_poly.ml
  let  f x  = x
  let  temp  = ((f 1), (f true))
  $ ./lambdalift_demo.exe < manytests/typed/015tuples.ml
  let rec fix f x  = ((f (fix f)) x)
  let  map f p  = 
    let (a, b) = p
    in ((f a), (f b))
  let  lambada0 self l li x  = ((li (self l)) x)
  let  lambada map self l  = ((map ((lambada0 self) l)) l)
  let  fixpoly l  = ((fix (lambada map)) l)
  let  feven p n  = 
    let (e, o) = p
    in 
    if (n = 0)
    then 1
    else (o (n - 1))
  let  fodd p n  = 
    let (e0, o0) = p
    in 
    if (n = 0)
    then 0
    else (e0 (n - 1))
  let  tie  = (fixpoly (feven, fodd))
  let rec modd meven n  = 
    if (n = 0)
    then 1
    else (meven (n - 1))
  let rec meven n  = 
    if (n = 0)
    then 1
    else ((modd meven) (n - 1))
  let  main  = 
    let () = (print_int ((modd meven) 1))
    in 
    let () = (print_int (meven 2))
    in 
    let (even, odd) = tie
    in 
    let () = (print_int (odd 3))
    in 
    let () = (print_int (even 4))
    in 0

  $ ./lambdalift_demo.exe << EOF
  > let a = 5
  > let sum a = 
  >   let a = a + 3 in a
  > let b = a
  let  a  = 5
  let  sum a0  = 
    let a1 = (a0 + 3)
    in a1
  let  b  = a

  $ ./lambdalift_demo.exe << EOF
  > let a = 5
  > let sum a = 
  >   let ((), a) = (print_int 4, a) in a
  > let b = a
  let  a  = 5
  let  sum a0  = 
    let ((), a1) = ((print_int 4), a0)
    in a1
  let  b  = a


  $ ./lambdalift_demo.exe << EOF
  > let sum e a = 
  >   let (e, a) = (e, a) in
  >   let (e, a) = (e, a) in (e, a)
  let  sum e a  = 
    let (e0, a0) = (e, a)
    in 
    let (e1, a1) = (e0, a0)
    in (e1, a1)
  $ ./lambdalift_demo.exe << EOF
  > let sum = 
  >   let ((), ()) = (print_int 4, print_int 3)  in
  >   0
  let (unit, unit0) = ((print_int 4), (print_int 3))
  let  sum  = unit; unit0; 0

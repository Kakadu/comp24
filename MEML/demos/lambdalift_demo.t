
  $ ./lambdalift_demo.exe << EOF
  > let sum = let a x = (fun y -> x + y) 2 in a 1
  let  lambada0 x y  = (x + y)
  let  a x  = ((lambada0 x) 2)
  let  sum  = (a 1)

  $ ./lambdalift_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  let  test2 x y i  = (x, y, i)
  let  test1 (x, y)  = ((test2 x) y)

  $ ./lambdalift_demo.exe << EOF
  > let vosem = 
  >   let odin = 1 in 
  >   let dva = 2 in
  >   let odin_plus_dva_plus x = x + (fun i -> odin + dva + i) 0 in
  >   odin_plus_dva_plus 5
  let  odin  = 1
  let  dva  = 2
  let  lambada0 i  = ((odin + dva) + i)
  let  odin_plus_dva_plus x  = (x + (lambada0 0))
  let  vosem  = (odin_plus_dva_plus 5)

  $ ./lambdalift_demo.exe << EOF
  > let b = 6;;
  > let i = 1;;
  > let sem = (fun num1 -> (fun num2 -> num1 + num2)) b i
  let  b  = 6
  let  i  = 1
  let  lambada0 num1 num2  = (num1 + num2)
  let  sem  = ((lambada0 b) i)
  $ ./lambdalift_demo.exe << EOF
  > let fac_cps num =
  >   let rec helper num acc =
  >     if num = 0
  >     then acc 1
  >     else helper (num - 1) (fun t -> acc (num * t))
  >   in
  >   helper num (fun x -> x) 
  let  lambada1 x  = x
  let  lambada0 acc num t  = (acc (num * t))
  let rec helper num acc  = 
    if (num = 0)
    then (acc 1)
    else ((helper (num - 1)) ((lambada0 acc) num))
  let  fac_cps num  = ((helper num) lambada1)

  $ ./lambdalift_demo.exe < manytests/typed/001fac.ml
  let rec fac n  = 
    if (n <= 1)
    then 1
    else (n * (fac (n -1)))
  let  unit_0  = (print_int (fac 4))
  let  main  = unit_0; 0
  $ ./lambdalift_demo.exe < manytests/typed/002fac.ml
  let  lambada0 k n p  = (k (p * n))
  let rec fac_cps n k  = 
    if (n = 1)
    then (k 1)
    else ((fac_cps (n -1)) ((lambada0 k) n))
  let  lambada0 print_int  = print_int
  let  unit_0  = (print_int ((fac_cps 4) lambada0))
  let  main  = unit_0; 0
  $ ./lambdalift_demo.exe < manytests/typed/003fib.ml
  let  n1 n  = (n -1)
  let  ab a b  = (a + b)
  let rec fib_acc a b n  = 
    if (n = 1)
    then b
    else (((fib_acc b) ((ab a) b)) (n1 n))
  let rec fib n  = 
    if (n < 2)
    then n
    else ((fib (n - 1)) + (fib (n - 2)))
  let  unit_0  = (print_int (((fib_acc 0) 1) 4))
  let  unit_1  = (print_int (fib 4))
  let  main  = unit_0; unit_1; 0
  $ ./lambdalift_demo.exe < manytests/typed/004manyargs.ml
  let  wrap f  = 
    if (1 = 1)
    then f
    else f
  let  a a  = (print_int a)
  let  b b  = (print_int b)
  let  c c  = (print_int c)
  let  test3 a b c  = 0
  let  test10 a b c d e f g h i j  = ((((((((((a a) + (b b)) + (c c)) + d) + e) + f) + g) + h) + i) + j)
  let  rez  = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
  let  unit_0  = (print_int rez)
  let  temp2  = ((((wrap test3) 1) 10) 100)
  let  main  = unit_0; 0
  $ ./lambdalift_demo.exe < manytests/typed/005fix.ml
  let rec fix f x  = ((f (fix f)) x)
  let  fac self n  = 
    if (n <= 1)
    then 1
    else (n * (self (n -1)))
  let  unit_0  = (print_int ((fix fac) 6))
  let  main  = unit_0; 0
  $ ./lambdalift_demo.exe < manytests/typed/006partial.ml
  let  lambada0 foo  = (foo + 2)
  let  lambada1 foo  = (foo * 10)
  let  foo b  = 
    if b
    then lambada0
    else lambada1
  let  foo x  = ((foo true) ((foo false) ((foo true) ((foo false) x))))
  let  unit_0  = (print_int (foo 11))
  let  main  = unit_0; 0
  $ ./lambdalift_demo.exe < manytests/typed/006partial2.ml
  let  unit_0 a  = (print_int a)
  let  unit_1 b  = (print_int b)
  let  unit_2 c  = (print_int c)
  let  foo a b c  = (unit_0 a); (unit_1 b); (unit_2 c); (a + (b * c))
  let  foo  = (foo 1)
  let  foo  = (foo 2)
  let  foo  = (foo 3)
  let  unit_0  = (print_int foo)
  let  main  = (unit_0 a); 0
  $ ./lambdalift_demo.exe < manytests/typed/006partial3.ml
  let  lambada1 print_int c  = (print_int c)
  let  unit_0 b  = (print_int b)
  let  lambada0 b  = (unit_0 b); (lambada1 print_int)
  let  unit_0 a  = (print_int a)
  let  foo a  = (unit_0 a); lambada0
  let  unit_0  = (((foo 4) 8) 9)
  let  main  = (unit_0 a); 0
  $ ./lambdalift_demo.exe < manytests/typed/007order.ml
  let  unit_0 a b  = (print_int (a + b))
  let  unit_1 __  = (print_int __)
  let  _start () () a () b _c () d __  = ((unit_0 a) b); (unit_1 __); (((a * b) / _c) + d)
  let  main  = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int -1)) 10000) -555555))
  $ ./lambdalift_demo.exe < manytests/typed/008ascription.ml
  let  addi f g x  = ((f x) (g x))
  let  lambada0 x b  = 
    if b
    then (x + 1)
    else (x * 2)
  let  lambada1 _start  = ((_start / 2) = 0)
  let  unit_0  = (print_int (((addi lambada0) lambada1) 4))
  let  main  = unit_0; 0
  $ ./lambdalift_demo.exe < manytests/typed/009let_poly.ml
  let  f x  = x
  let  temp  = ((f 1), (f true))
  $ ./lambdalift_demo.exe < manytests/typed/015tuples.ml
  let rec fix f x  = ((f (fix f)) x)
  let  a, b p  = p    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  let  map f p  = ((f (a p)), (f (b p)))
  let  lambada1 self l li x  = ((li (self l)) x)
  let  lambada0 map self l  = ((map ((lambada1 self) l)) l)
  let  fixpoly l  = ((fix (lambada0 map)) l)
  let  e, o p  = p     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  let  feven p n  = 
    if (n = 0)
    then 1
    else ((o p) (n - 1))
  let  e, o p  = p
  let  fodd p n  = 
    if (n = 0)
    then 0
    else ((e p) (n - 1))
  let  tie  = (fixpoly (feven, fodd))
  let  modd meven n  = 
    if (n = 0)
    then 1
    else (meven (n - 1))and rec meven n  =  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (n = 0)
    then 1
    else ((modd meven) (n - 1))
  let  unit_0  = (print_int ((modd meven) 1))
  let  unit_1  = (print_int (meven 2))
  let  even, odd  = tie
  let  unit_2  = (print_int (odd 3))
  let  unit_3  = (print_int (even 4))
  let  main  = unit_0; unit_1; unit_2; unit_3; 0
  $ ./lambdalift_demo.exe < manytests/typed/016lists.ml
  let rec length tl xs  = (match xs with !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  | (h :: tl) -> (1 + (length tl))
  | [] -> 0)
  let rec helper tl acc xs  = (match xs with
  | (h :: tl) -> ((helper (acc + 1)) tl)
  | [] -> acc)
  let  length_tail  = ((helper tl) 0)
  let rec map a b c d tl f xs  = (match xs with
  | (a :: (b :: (c :: (d :: tl)))) -> [(f a); (f b); (f c); (f d); ((map f) tl); ]
  | (a :: (b :: (c :: []))) -> [(f a); (f b); (f c); ]
  | (a :: (b :: [])) -> [(f a); (f b); ]
  | (a :: []) -> [(f a); ]
  | [] -> [])
  let rec append x xs ys  = (match xs with
  | (x :: xs) -> [x; ((append xs) ys); ]
  | [] -> ys)
  let rec helper h tl xs  = (match xs with
  | (h :: tl) -> (((append x) h) ((helper tl) tl))
  | [] -> [])
  let  concat  = ((helper h) tl)
  let  unit_0 f h  = (f h)
  let rec iter () tl f xs  = (match xs with
  | (h :: tl) -> ((unit_0 f) h); ((iter f) tl)
  | [] -> ())
  let  lambada0 h a  = (h, a)
  let rec cartesian h tl xs ys  = (match xs with
  | (h :: tl) -> (((append x) (((((((map a) b) c) d) tl) (lambada0 h)) ys)) ((cartesian tl) ys))
  | [] -> [])
  let  unit_0  = ((((iter ()) tl) print_int) [1; 2; 3; ])
  let  unit_1  = (print_int ((length tl) ((((cartesian h) tl) [1; 2; ]) [1; 2; 3; 4; ])))
  let  main  = ((unit_0 f) h); unit_1; 0

  $ ./lambdalift_demo.exe << EOF
  > let sum = 
  >   let () = print_int 2 in let () = print_int 2 in 0
  let  unit_0  = (print_int 2)
  let  unit_1  = (print_int 2)
  let  sum  = unit_0; unit_1; 0

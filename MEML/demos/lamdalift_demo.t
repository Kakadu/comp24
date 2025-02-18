
  $ ./lamdalift_demo.exe << EOF
  > let sum x = let num2 y = x + y in test2
  let  lambada0 test2 x y  = (x + y)
  let  sum x  = test2

  $ ./lamdalift_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  let  lambada0 x y i  = (x, y, i)
  let  test1 (x, y)  = ((lambada0 x) y)

  $ ./lamdalift_demo.exe << EOF
  > let vosem = 
  >   let odin = 1 in 
  >   let dva = 2 in
  >   let odin_plus_dva_plus x = x + (fun i -> odin + dva) 0 in
  >   odin_plus_dva_plus 5
  let  lambada1 dva odin i  = (odin + dva)
  let  lambada0 dva odin x  = (x + (((lambada1 dva) odin) 0))
  let  vosem  = 
    let  odin = 1
    in 
    let  dva = 2
    in (((lambada0 dva) odin) 5)

  $ ./lamdalift_demo.exe << EOF
  > let b = 6;;
  > let lambada0 = 1;;
  > let i = 1;;
  > let lambada1 = 2;;
  > let lambada3 = 3;;
  > let sem = (fun num1 -> (fun num2 -> num1 + num2)) b i
  let  b  = 6
  let  lambada0  = 1
  let  i  = 1
  let  lambada1  = 2
  let  lambada3  = 3
  let  lambada2 num1 num2  = (num1 + num2)
  let  sem  = ((lambada2 b) i)

  $ ./lamdalift_demo.exe << EOF
  > let rec fac_cps num =
  >   let helper num acc =
  >     if num = 0
  >     then acc 1
  >     else fac_cps (num - 1) (fun t -> acc (num * t))
  >   in
  >   helper num (fun x -> x) 
  let  lambada1 acc num t  = (acc (num * t))
  let  lambada0 fac_cps num acc  = 
    if (num = 0)
    then (acc 1)
    else ((fac_cps (num - 1)) ((lambada1 acc) num))
  let  lambada2 x  = x
  let rec fac_cps num  = (((lambada0 fac_cps) num) lambada2)

  $ ./lamdalift_demo.exe < manytests/typed/001fac.ml
  let rec fac n  = 
    if (n <= 1)
    then 1
    else (n * (fac (n -1)))
  let  main  = 
    let  () = (print_int (fac 4))
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/002fac.ml
  let  lambada0 k n p  = (k (p * n))
  let rec fac_cps n k  = 
    if (n = 1)
    then (k 1)
    else ((fac_cps (n -1)) ((lambada0 k) n))
  let  lambada1 print_int  = print_int
  let  main  = 
    let  () = (print_int ((fac_cps 4) lambada1))
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n  = 
    if (n = 1)
    then b
    else 
    let  n1 = (n -1)
    in 
    let  ab = (a + b)
    in (((fib_acc b) ab) n1)
  let rec fib n  = 
    if (n < 2)
    then n
    else ((fib (n - 1)) + (fib (n - 2)))
  let  main  = 
    let  () = (print_int (((fib_acc 0) 1) 4))
    in 
    let  () = (print_int (fib 4))
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/004manyargs.ml
  let  wrap f  = 
    if (1 = 1)
    then f
    else f
  let  test3 a b c  = 
    let  a = (print_int a)
    in 
    let  b = (print_int b)
    in 
    let  c = (print_int c)
    in 0
  let  test10 a b c d e f g h i j  = (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j)
  let  main  = 
    let  rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
    in 
    let  () = (print_int rez)
    in 
    let  temp2 = ((((wrap test3) 1) 10) 100)
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/005fix.ml
  let rec fix f x  = ((f (fix f)) x)
  let  fac self n  = 
    if (n <= 1)
    then 1
    else (n * (self (n -1)))
  let  main  = 
    let  () = (print_int ((fix fac) 6))
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/006partial.ml
  let  lambada0 foo  = (foo 2)
  let  lambada1 foo  = (foo * 10)
  let  foo b  = 
    if b
    then lambada0
    else lambada1
  let  foo x  = ((foo true) ((foo false) ((foo true) ((foo false) x))))
  let  main  = 
    let  () = (print_int (foo 11))
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/006partial2.ml
  let  foo a b c  = 
    let  () = (print_int a)
    in 
    let  () = (print_int b)
    in 
    let  () = (print_int c)
    in (a + (b * c))
  let  main  = 
    let  foo = (foo 1)
    in 
    let  foo = (foo 2)
    in 
    let  foo = (foo 3)
    in 
    let  () = (print_int foo)
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/006partial3.ml
  let  lambada1 print_int c  = (print_int c)
  let  lambada0 print_int b  = 
    let  () = (print_int b)
    in (lambada1 print_int)
  let  foo a  = 
    let  () = (print_int a)
    in (lambada0 print_int)
  let  main  = 
    let  () = (((foo 4) 8) 9)
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/007order.ml
  let  _start () () a () b _ c () d _ _  = 
    let  () = (print_int (a + b))
    in 
    let  () = (print_int __)
    in (((a * b) / _c) + d)
  let  main  = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int -1)) 10000) -555555))
  $ ./lamdalift_demo.exe < manytests/typed/008ascription.ml
  let  addi f g x  = ((f x) (g x))
  let  lambada0 x b  = 
    if b
    then (x 1)
    else (x * 2)
  let  lambada1 _start _ start  = ((_start / 2) = 0)
  let  main  = 
    let  () = (print_int (((addi lambada0) (lambada1 _start)) 4))
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/009let_poly.ml
  let  lambada0 x  = x
  let  temp  = ((lambada0 1), (lambada0 true))
  $ ./lamdalift_demo.exe < manytests/typed/015tuples.ml
  let rec fix f x  = ((f (fix f)) x)
  let  map f p  = 
    let  a, b = p
    in ((f a), (f b))
  let  lambada1 l self li x  = ((li (self l)) x)
  let  lambada0 map self l  = ((map ((lambada1 l) self)) l)
  let  fixpoly l  = ((fix (lambada0 map)) l)
  let  feven p n  = 
    let  e, o = p
    in 
    if (n = 0)
    then 1
    else (o (n - 1))
  let  fodd p n  = 
    let  e, o = p
    in 
    if (n = 0)
    then 0
    else (e (n - 1))
  let  tie  = (fixpoly (feven, fodd))
  let rec meven n  = 
    if (n = 0)
    then 1
    else (modd (n - 1))and  modd n  = 
    if (n = 0)
    then 1
    else (meven (n - 1))
  let  main  = 
    let  () = (print_int (modd 1))
    in 
    let  () = (print_int (meven 2))
    in 
    let  even, odd = tie
    in 
    let  () = (print_int (odd 3))
    in 
    let  () = (print_int (even 4))
    in 0
  $ ./lamdalift_demo.exe < manytests/typed/016lists.ml
  let rec length xs  = (match xs with
  | [] -> 0
  | (h :: tl) -> (1 + (length tl))
  let rec lambada0 acc xs  = (match xs with
  | [] -> acc
  | (h :: tl) -> ((lambada0 (acc + 1)) tl)
  let  length_tail  = (lambada0 0)
  let rec map f xs  = (match xs with
  | [] -> []
  | (a :: []) -> [(f a); ]
  | (a :: (b :: [])) -> [(f a); (f b); ]
  | (a :: (b :: (c :: []))) -> [(f a); (f b); (f c); ]
  | (a :: (b :: (c :: (d :: tl)))) -> [(f a); (f b); (f c); (f d); ((map f) tl); ]
  let rec append xs ys  = (match xs with
  | [] -> ys
  | (x :: xs) -> [x; ((append xs) ys); ]
  let rec lambada1 append xs  = (match xs with
  | [] -> []
  | (h :: tl) -> ((append h) ((lambada1 append) tl))
  let  concat  = (lambada1 append)
  let rec iter f xs  = (match xs with
  | [] -> ()
  | (h :: tl) -> 
    let  () = (f h)
    in ((iter f) tl)
  let  lambada2 h a  = (h, a)
  let rec cartesian xs ys  = (match xs with
  | [] -> []
  | (h :: tl) -> ((append ((map (lambada2 h)) ys)) ((cartesian tl) ys))
  let  main  = 
    let  () = ((iter print_int) [1; 2; 3; ])
    in 
    let  () = (print_int (length ((cartesian [1; 2; ]) [1; 2; 3; 4; ])))
    in 0


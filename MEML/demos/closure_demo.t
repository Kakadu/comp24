
  $ ./closure_demo.exe << EOF
  > let sum = let a x = (fun y -> x + y) 2 in a 1
  let  sum  = 
    let  lambada0 x y  = (x + y)
    in 
    let  a x  = ((lambada0 x) 2)
    in (a 1)

  $ ./closure_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  let  test1 (x, y)  = 
    let  test2 i  = (x, y, i)
    in test2

  $ ./closure_demo.exe << EOF
  > let vosem = 
  >   let odin = 1 in 
  >   let dva = 2 in
  >   let odin_plus_dva_plus x = x + (fun i -> odin + dva + i) 0 in
  >   odin_plus_dva_plus 5
  let  vosem  = 
    let  odin  = 1
    in 
    let  dva  = 2
    in 
    let  lambada0 i  = ((odin + dva) + i)
    in 
    let  odin_plus_dva_plus x  = (x + (lambada0 0))
    in (odin_plus_dva_plus 5)

  $ ./closure_demo.exe << EOF
  > let b = 6;;
  > let i = 1;;
  > let sem = (fun num1 -> (fun num2 -> num1 + num2)) b i
  let  b  = 6
  let  i  = 1
  let  sem  = 
    let  lambada0 num1 num2  = (num1 + num2)
    in ((lambada0 b) i)
  $ ./closure_demo.exe << EOF
  > let fac_cps num =
  >   let rec helper num acc =
  >     if num = 0
  >     then acc 1
  >     else helper (num - 1) (fun t -> acc (num * t))
  >   in
  >   helper num (fun x -> x) 
  let  fac_cps num  = 
    let  lambada1 x  = x
    in 
    let  lambada0 acc num t  = (acc (num * t))
    in 
    let rec helper num acc  = 
    if (num = 0)
    then (acc 1)
    else ((helper (num - 1)) ((lambada0 acc) num))
    in ((helper num) lambada1)

  $ ./closure_demo.exe < manytests/typed/001fac.ml
  let rec fac n  = 
    if (n <= 1)
    then 1
    else (n * (fac (n -1)))
  let  main  = 
    let  ()  = (print_int (fac 4))
    in 0
  $ ./closure_demo.exe < manytests/typed/002fac.ml
  let rec fac_cps n k  = 
    let  lambada0 k n p  = (k (p * n))
    in 
    if (n = 1)
    then (k 1)
    else ((fac_cps (n -1)) ((lambada0 k) n))
  let  main  = 
    let  lambada0 print_int  = print_int
    in 
    let  ()  = (print_int ((fac_cps 4) lambada0))
    in 0
  $ ./closure_demo.exe < manytests/typed/003fib.ml
  let rec fib_acc a b n  = 
    if (n = 1)
    then b
    else 
    let  n1  = (n -1)
    in 
    let  ab  = (a + b)
    in (((fib_acc b) ab) n1)
  let rec fib n  = 
    if (n < 2)
    then n
    else ((fib (n - 1)) + (fib (n - 2)))
  let  main  = 
    let  ()  = (print_int (((fib_acc 0) 1) 4))
    in 
    let  ()  = (print_int (fib 4))
    in 0
  $ ./closure_demo.exe < manytests/typed/004manyargs.ml
  let  wrap f  = 
    if (1 = 1)
    then f
    else f
  let  test3 a b c  = 
    let  a  = (print_int a)
    in 
    let  b  = (print_int b)
    in 
    let  c  = (print_int c)
    in 0
  let  test10 a b c d e f g h i j  = (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j)
  let  main  = 
    let  rez  = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
    in 
    let  ()  = (print_int rez)
    in 
    let  temp2  = ((((wrap test3) 1) 10) 100)
    in 0
  $ ./closure_demo.exe < manytests/typed/005fix.ml
  let rec fix f x  = ((f (fix f)) x)
  let  fac self n  = 
    if (n <= 1)
    then 1
    else (n * (self (n -1)))
  let  main  = 
    let  ()  = (print_int ((fix fac) 6))
    in 0
  $ ./closure_demo.exe < manytests/typed/006partial.ml
  let  foo b  = 
    let  lambada0 foo  = (foo + 2)
    in 
    let  lambada1 foo  = (foo * 10)
    in 
    if b
    then lambada0
    else lambada1
  let  foo x  = ((foo true) ((foo false) ((foo true) ((foo false) x))))
  let  main  = 
    let  ()  = (print_int (foo 11))
    in 0
  $ ./closure_demo.exe < manytests/typed/006partial2.ml
  let  foo a b c  = 
    let  ()  = (print_int a)
    in 
    let  ()  = (print_int b)
    in 
    let  ()  = (print_int c)
    in (a + (b * c))
  let  main  = 
    let  foo  = (foo 1)
    in 
    let  foo  = (foo 2)
    in 
    let  foo  = (foo 3)
    in 
    let  ()  = (print_int foo)
    in 0
  $ ./closure_demo.exe < manytests/typed/006partial3.ml
  let  foo a  = 
    let  lambada1 print_int c  = (print_int c)
    in 
    let  lambada0 b  = 
    let  ()  = (print_int b)
    in (lambada1 print_int)
    in 
    let  ()  = (print_int a)
    in lambada0
  let  main  = 
    let  ()  = (((foo 4) 8) 9)
    in 0
  $ ./closure_demo.exe < manytests/typed/007order.ml
  let  _start () () a () b _c () d __  = 
    let  ()  = (print_int (a + b))
    in 
    let  ()  = (print_int __)
    in (((a * b) / _c) + d)
  let  main  = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int -1)) 10000) -555555))
  $ ./closure_demo.exe < manytests/typed/008ascription.ml
  let  addi f g x  = ((f x) (g x))
  let  main  = 
    let  lambada0 x b  = 
    if b
    then (x + 1)
    else (x * 2)
    in 
    let  lambada1 _start  = ((_start / 2) = 0)
    in 
    let  ()  = (print_int (((addi lambada0) lambada1) 4))
    in 0
  $ ./closure_demo.exe < manytests/typed/009let_poly.ml
  let  temp  = 
    let  f x  = x
    in ((f 1), (f true))
  $ ./closure_demo.exe < manytests/typed/015tuples.ml
  let rec fix f x  = ((f (fix f)) x)
  let  map f p  = 
    let (a, b) = p
    in ((f a), (f b))
  let  fixpoly l  = 
    let  lambada1 self l li x  = ((li (self l)) x)
    in 
    let  lambada0 map self l  = ((map ((lambada1 self) l)) l)
    in ((fix (lambada0 map)) l)
  let  feven p n  = 
    let (e, o) = p
    in 
    if (n = 0)
    then 1
    else (o (n - 1))
  let  fodd p n  = 
    let (e, o) = p
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
    let  ()  = (print_int (modd 1))
    in 
    let  ()  = (print_int (meven 2))
    in 
    let (even, odd) = tie
    in 
    let  ()  = (print_int (odd 3))
    in 
    let  ()  = (print_int (even 4))
    in 0
  $ ./closure_demo.exe < manytests/typed/016lists.ml
  let rec length xs  = (match xs with
  | [] -> 0
  | (h :: tl) -> (1 + (length tl)))
  let  length_tail  = 
    let rec helper acc xs  = (match xs with
  | [] -> acc
  | (h :: tl) -> ((helper (acc + 1)) tl))
    in (helper 0)
  let rec map f xs  = (match xs with
  | [] -> []
  | (a :: []) -> [(f a); ]
  | (a :: (b :: [])) -> [(f a); (f b); ]
  | (a :: (b :: (c :: []))) -> [(f a); (f b); (f c); ]
  | (a :: (b :: (c :: (d :: tl)))) -> [(f a); (f b); (f c); (f d); ((map f) tl); ])
  let rec append xs ys  = (match xs with
  | [] -> ys
  | (x :: xs) -> [x; ((append xs) ys); ])
  let  concat  = 
    let rec helper xs  = (match xs with
  | [] -> []
  | (h :: tl) -> ((append h) (helper tl)))
    in helper
  let rec iter f xs  = (match xs with
  | [] -> ()
  | (h :: tl) -> 
    let  ()  = (f h)
    in ((iter f) tl))
  let rec cartesian xs ys  = 
    let  lambada0 h a  = (h, a)
    in (match xs with
  | [] -> []
  | (h :: tl) -> ((append ((map (lambada0 h)) ys)) ((cartesian tl) ys)))
  let  main  = 
    let  ()  = ((iter print_int) [1; 2; 3; ])
    in 
    let  ()  = (print_int (length ((cartesian [1; 2; ]) [1; 2; 3; 4; ])))
    in 0


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
  let  lambada0 i  = ((odin0 + dva) + i)
  let  odin_plus_dva_plus x  = (x + (lambada0 0))
  let  vosem  = (odin_plus_dva_plus 5)
  let  tri  = odin

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
  let  unit  = (print_int (fac 4))
  let  main  = unit; 0
  $ ./lambdalift_demo.exe < manytests/typed/002fac.ml
  let  lambada0 k n p  = (k (p * n))
  let rec fac_cps n k  = 
    if (n = 1)
    then (k 1)
    else ((fac_cps (n -1)) ((lambada0 k) n))
  let  lambada00 print_int  = print_int
  let  unit  = (print_int ((fac_cps 4) lambada00))
  let  main  = unit; 0
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
  let  unit  = (print_int (((fib_acc 0) 1) 4))
  let  unit0  = (print_int (fib 4))
  let  main  = unit; unit0; 0
  $ ./lambdalift_demo.exe < manytests/typed/004manyargs.ml
  let  wrap f  = 
    if (1 = 1)
    then f
    else f
  let  a0 a  = (print_int a)
  let  b0 b  = (print_int b)
  let  c0 c  = (print_int c)
  let  test3 a b c  = 0
  let  test10 a b c d e f g h i j  = (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j)
  let  rez  = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
  let  unit  = (print_int rez)
  let  temp2  = ((((wrap test3) 1) 10) 100)
  let  main  = unit; 0
  $ ./lambdalift_demo.exe < manytests/typed/005fix.ml
  let rec fix f x  = ((f (fix f)) x)
  let  fac self n  = 
    if (n <= 1)
    then 1
    else (n * (self (n -1)))
  let  unit  = (print_int ((fix fac) 6))
  let  main  = unit; 0
  $ ./lambdalift_demo.exe < manytests/typed/006partial.ml
  let  lambada0 foo  = (foo + 2)
  let  lambada1 foo  = (foo * 10)
  let  foo b  = 
    if b
    then lambada0
    else lambada1
  let  foo0 x  = ((foo true) ((foo false) ((foo true) ((foo false) x))))
  let  unit  = (print_int (foo0 11))
  let  main  = unit; 0
  $ ./lambdalift_demo.exe < manytests/typed/006partial2.ml
  let  unit a  = (print_int a)
  let  unit0 b  = (print_int b)
  let  unit1 c  = (print_int c)
  let  foo a b c  = (unit a); (unit0 b); (unit1 c); (a + (b * c))
  let  foo0  = (foo 1)
  let  foo1  = (foo0 2)
  let  foo2  = (foo1 3)
  let  unit2  = (print_int foo2)
  let  main  = unit2; 0
  $ ./lambdalift_demo.exe < manytests/typed/006partial3.ml
  let  lambada1 print_int c  = (print_int c)
  let  unit b  = (print_int b)
  let  lambada0 b  = (unit b); (lambada1 print_int)
  let  unit0 a  = (print_int a)
  let  foo a  = (unit0 a); lambada0
  let  unit1  = (((foo 4) 8) 9)
  let  main  = unit1; 0
  $ ./lambdalift_demo.exe < manytests/typed/007order.ml
  let  unit a b  = (print_int (a + b))
  let  unit0 __  = (print_int __)
  let  _start () () a () b _c () d __  = ((unit a) b); (unit0 __); (((a * b) / _c) + d)
  let  main  = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int -1)) 10000) -555555))
  $ ./lambdalift_demo.exe < manytests/typed/008ascription.ml
  let  addi f g x  = ((f x) (g x))
  let  lambada0 x b  = 
    if b
    then (x + 1)
    else (x * 2)
  let  lambada1 _start  = ((_start / 2) = 0)
  let  unit  = (print_int (((addi lambada0) lambada1) 4))
  let  main  = unit; 0
  $ ./lambdalift_demo.exe < manytests/typed/009let_poly.ml
  let  f x  = x
  let  temp  = ((f 1), (f true))
  $ ./lambdalift_demo.exe < manytests/typed/015tuples.ml
  let rec fix f x  = ((f (fix f)) x)
  let  map f p  = 
    let a, b  = p
    in ((f a), (f b))
  let  lambada1 self l li x  = ((li (self l)) x)
  let  lambada0 map self l  = ((map ((lambada1 self) l)) l)
  let  fixpoly l  = ((fix (lambada0 map)) l)
  let  feven p n  = 
    let e, o  = p
    in 
    if (n = 0)
    then 1
    else (o (n - 1))
  let  fodd p n  = 
    let e0, o0  = p
    in 
    if (n = 0)
    then 0
    else (e0 (n - 1))
  let  tie  = (fixpoly (feven, fodd))
  let  modd meven n  = 
    if (n = 0)
    then 1
    else (meven (n - 1))
  let rec meven n  = 
    if (n = 0)
    then 1
    else ((modd meven) (n - 1))
  let  unit  = (print_int ((modd meven) 1))
  let  unit0  = (print_int (meven 2))
  let  unit1 odd  = (print_int (odd 3))
  let  unit2 even  = (print_int (even 4))
  let  main  = unit; unit0; 
    let even, odd  = tie
    in (unit1 odd); (unit2 even); 0
  $ ./lambdalift_demo.exe < manytests/typed/016lists.ml
  let rec length xs  = (match xs with
  | [] -> 0
  | (h :: tl) -> (1 + (length tl)))
  let rec helper acc xs  = (match xs with
  | [] -> acc
  | (h :: tl) -> ((helper (acc + 1)) tl))
  let  length_tail  = (helper 0)
  let rec map f xs  = (match xs with
  | [] -> []
  | (a :: []) -> [(f a); ]
  | (a :: (b :: [])) -> [(f a); (f b); ]
  | (a :: (b :: (c :: []))) -> [(f a); (f b); (f c); ]
  | (a :: (b :: (c :: (d :: tl)))) -> [(f a); (f b); (f c); (f d); ((map f) tl); ])
  let rec append xs ys  = (match xs with
  | [] -> ys
  | (x :: xs) -> [x; ((append xs) ys); ])
  let rec helper0 xs  = (match xs with
  | [] -> []
  | (h :: tl) -> ((append h) (helper0 tl)))
  let  concat  = helper0
  let  unit f h  = (f h)
  let rec iter f xs  = (match xs with
  | [] -> ()
  | (h :: tl) -> ((unit f) h); ((iter f) tl))
  let  lambada0 h a  = (h, a)
  let rec cartesian xs ys  = (match xs with
  | [] -> []
  | (h :: tl) -> ((append ((map (lambada0 h)) ys)) ((cartesian tl) ys)))
  let  unit0  = ((iter print_int) [1; 2; 3; ])
  let  unit1  = (print_int (length ((cartesian [1; 2; ]) [1; 2; 3; 4; ])))
  let  main  = unit0; unit1; 0

  $ ./lambdalift_demo.exe << EOF
  > let a = 5
  > let sum a = 
  >   let a = a + 3 in a
  > let b = a
  let  a  = 5
  let  a1 a0  = (a0 + 3)
  let  sum a0  = (a1 a0)
  let  b  = a

  $ ./lambdalift_demo.exe << EOF
  > let a = 5
  > let sum a = 
  >   let (), a = (print_int 4, a) in a
  > let b = a
  let  a  = 5
  let  sum a0  = 
    let (), a1  = ((print_int 4), a0)
    in a1
  let  b  = a


  $ ./lambdalift_demo.exe << EOF
  > let sum e a = 
  >   let (e, a) = (e, a) in
  >   let (e, a) = (e, a) in (e, a)
  let  sum e a  = 
    let e0, a0  = (e, a)
    in 
    let e1, a1  = (e0, a0)
    in (e1, a1)
  $ ./lambdalift_demo.exe << EOF
  > let sum = 
  >   let (), () = (print_int 4, print_int 3)  in
  >   0
  let  unit, unit0  = ((print_int 4), (print_int 3))
  let  sum  = unit; unit0; 0

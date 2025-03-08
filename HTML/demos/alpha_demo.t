  $ ./alpha_demo.exe < manytests/do_not_type/001.ml
  ---СС---
  
  let recfac n = if (n <= 1) then 1 else (n * fac (n - 1))
  
  ---Alpha conv.---
  
  let recfac_1 n = if (n <= 1) then 1 else (n * fac (n - 1))

  $ ./alpha_demo.exe < manytests/do_not_type/002if.ml
  ---СС---
  
  let main = if true then 1 else false
  
  ---Alpha conv.---
  
  let main_1 = if true then 1 else false

  $ ./alpha_demo.exe < manytests/do_not_type/003occurs.ml
  ---СС---
  
  let cc_ll_0 x f = ((x x) f);;
  let cc_ll_1 f x = (f (cc_ll_0 x));;
  let cc_ll_2 x f = ((x x) f);;
  let cc_ll_3 f x = (f (cc_ll_2 x));;
  let fix f = ((cc_ll_1 f) (cc_ll_3 f))
  
  ---Alpha conv.---
  
  let cc_ll_0_1 x f = ((x x) f);;
  let cc_ll_1_1 f x = (f (cc_ll_0_1 x));;
  let cc_ll_2_1 x f = ((x x) f);;
  let cc_ll_3_1 f x = (f (cc_ll_2_1 x));;
  let fix_1 f = ((cc_ll_1_1 f) (cc_ll_3_1 f))

  $ ./alpha_demo.exe < manytests/do_not_type/004let_poly.ml
  ---СС---
  
  let cc_ll_0 f = ((f 1), (f true));;
  let cc_ll_1 x = x;;
  let temp = (cc_ll_0 cc_ll_1)
  
  ---Alpha conv.---
  
  let cc_ll_0_1 f = ((f 1), (f true));;
  let cc_ll_1_1 x = x;;
  let temp_1 = (cc_ll_0_1 cc_ll_1_1)

  $ ./alpha_demo.exe < manytests/do_not_type/015tuples.ml
  Tuples and lists are not supported

PASS
  $ ./alpha_demo.exe < manytests/typed/001fac.ml
  ---СС---
  
  let rec fac n = if (n <= 1) then 1 else (n * fac (n - 1));;
  let main = let () = (print_int (fac 4))
  in 0
  
  ---Alpha conv.---
  
  let rec fac_1 n = if (n <= 1) then 1 else (n * fac_1 (n - 1));;
  let main_1 = let () = (print_int (fac_1 4))
  in 0

PASS
  $ ./alpha_demo.exe < manytests/typed/002fac.ml
  ---СС---
  
  let cc_ll_0 n k p = k (p * n);;
  let rec fac_cps n k = if (n = 1) then (k 1) else (fac_cps (n - 1) ((cc_ll_0 n) k));;
  let cc_ll_1 print_int = print_int;;
  let main = let () = (print_int ((fac_cps 4) cc_ll_1))
  in 0
  
  ---Alpha conv.---
  
  let cc_ll_0_1 n k p = k (p * n);;
  let rec fac_cps_1 n k = if (n = 1) then (k 1) else (fac_cps_1 (n - 1) ((cc_ll_0_1 n) k));;
  let cc_ll_1_1 print_int = print_int;;
  let main_1 = let () = (print_int ((fac_cps_1 4) cc_ll_1_1))
  in 0

PASS
  $ ./alpha_demo.exe < manytests/typed/003fib.ml
  ---СС---
  
  let rec fib_acc a b n = if (n = 1) then b else let n1 = (n - 1)
  in let ab = (a + b)
  in (((fib_acc b) ab) n1);;
  let rec fib n = if (n < 2) then n else (fib (n - 1) + fib (n - 2));;
  let main = let () = (print_int (((fib_acc 0) 1) 4))
  in let () = (print_int (fib 4))
  in 0
  
  ---Alpha conv.---
  
  let rec fib_acc_1 a b n = if (n = 1) then b else let n1_l1 = (n - 1)
  in let ab_l1 = (a + b)
  in (((fib_acc_1 b) ab_l1) n1_l1);;
  let rec fib_1 n = if (n < 2) then n else (fib_1 (n - 1) + fib_1 (n - 2));;
  let main_1 = let () = (print_int (((fib_acc_1 0) 1) 4))
  in let () = (print_int (fib_1 4))
  in 0

PASS
  $ ./alpha_demo.exe < manytests/typed/004manyargs.ml
  ---СС---
  
  let wrap f = if (1 = 1) then f else f;;
  let test3 a b c = let a = (print_int a)
  in let b = (print_int b)
  in let c = (print_int c)
  in 0;;
  let test10 a b c d e f g h i j = (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j);;
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
  in let () = (print_int rez)
  in let temp2 = ((((wrap test3) 1) 10) 100)
  in 0
  
  ---Alpha conv.---
  
  let wrap_1 f = if (1 = 1) then f else f;;
  let test3_1 a b c = let a_l1 = (print_int a)
  in let b_l1 = (print_int b)
  in let c_l1 = (print_int c)
  in 0;;
  let test10_1 a b c d e f g h i j = (((((((((a + b) + c) + d) + e) + f) + g) + h) + i) + j);;
  let main_1 = let rez_l1 = (((((((((((wrap_1 test10_1) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000)
  in let () = (print_int rez_l1)
  in let temp2_l1 = ((((wrap_1 test3_1) 1) 10) 100)
  in 0

PASS
  $ ./alpha_demo.exe < manytests/typed/005fix.ml
  ---СС---
  
  let rec fix f x = ((f (fix f)) x);;
  let fac self n = if (n <= 1) then 1 else (n * self (n - 1));;
  let main = let () = (print_int ((fix fac) 6))
  in 0
  
  ---Alpha conv.---
  
  let rec fix_1 f x = ((f (fix_1 f)) x);;
  let fac_1 self n = if (n <= 1) then 1 else (n * self (n - 1));;
  let main_1 = let () = (print_int ((fix_1 fac_1) 6))
  in 0

PASS
  $ ./alpha_demo.exe < manytests/typed/006partial.ml
  ---СС---
  
  let cc_ll_0 foo = (foo + 2);;
  let cc_ll_1 foo = (foo * 10);;
  let foo b = if b then cc_ll_0 else cc_ll_1;;
  let foo x = ((foo true) ((foo false) ((foo true) ((foo false) x))));;
  let main = let () = (print_int (foo 11))
  in 0
  
  ---Alpha conv.---
  
  let cc_ll_0_1 foo = (foo + 2);;
  let cc_ll_1_1 foo = (foo * 10);;
  let foo_1 b = if b then cc_ll_0_1 else cc_ll_1_1;;
  let foo_2 x = ((foo_1 true) ((foo_1 false) ((foo_1 true) ((foo_1 false) x))));;
  let main_1 = let () = (print_int (foo_2 11))
  in 0

PASS
  $ ./alpha_demo.exe < manytests/typed/006partial2.ml
  ---СС---
  
  let foo a b c = let () = (print_int a)
  in let () = (print_int b)
  in let () = (print_int c)
  in (a + (b * c));;
  let main = let foo = (foo 1)
  in let foo = (foo 2)
  in let foo = (foo 3)
  in let () = (print_int foo)
  in 0
  
  ---Alpha conv.---
  
  let foo_1 a b c = let () = (print_int a)
  in let () = (print_int b)
  in let () = (print_int c)
  in (a + (b * c));;
  let main_1 = let foo_l1 = (foo_1 1)
  in let foo_l2 = (foo_l1 2)
  in let foo_l3 = (foo_l2 3)
  in let () = (print_int foo_l3)
  in 0

PASS
  $ ./alpha_demo.exe < manytests/typed/006partial3.ml
  ---СС---
  
  let cc_ll_0 c = (print_int c);;
  let cc_ll_1 b = let () = (print_int b)
  in cc_ll_0;;
  let foo a = let () = (print_int a)
  in cc_ll_1;;
  let main = let () = (((foo 4) 8) 9)
  in 0
  
  ---Alpha conv.---
  
  let cc_ll_0_1 c = (print_int c);;
  let cc_ll_1_1 b = let () = (print_int b)
  in cc_ll_0_1;;
  let foo_1 a = let () = (print_int a)
  in cc_ll_1_1;;
  let main_1 = let () = (((foo_1 4) 8) 9)
  in 0


PASS
  $ ./alpha_demo.exe < manytests/typed/007order.ml
  ---СС---
  
  let _start () () a () b _c () d __ = let () = print_int (a + b)
  in let () = (print_int __)
  in (((a * b) / _c) + d);;
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))
  
  ---Alpha conv.---
  
  let _start_1 () () a () b _c () d __ = let () = print_int (a + b)
  in let () = (print_int __)
  in (((a * b) / _c) + d);;
  let main_1 = (print_int (((((((((_start_1 (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))

PASS
  $ ./alpha_demo.exe < manytests/typed/008ascription.ml
  ---СС---
  
  let addi f g x = (((f x) ((g x) : bool)) : int);;
  let cc_ll_0 x b = if b then (x + 1) else (x * 2);;
  let cc_ll_1 _start = ((_start / 2) = 0);;
  let main = let () = (print_int (((addi cc_ll_0) cc_ll_1) 4))
  in 0
  
  ---Alpha conv.---
  
  let addi_1 f g x = (((f x) ((g x) : bool)) : int);;
  let cc_ll_0_1 x b = if b then (x + 1) else (x * 2);;
  let cc_ll_1_1 _start = ((_start / 2) = 0);;
  let main_1 = let () = (print_int (((addi_1 cc_ll_0_1) cc_ll_1_1) 4))
  in 0
PASS
  $ ./alpha_demo.exe < manytests/typed/009let_poly.ml
  ---СС---
  
  let cc_ll_0 x = x;;
  let temp = let f = cc_ll_0
  in ((f 1), (f true))
  
  ---Alpha conv.---
  
  let cc_ll_0_1 x = x;;
  let temp_1 = let f_l1 = cc_ll_0_1
  in ((f_l1 1), (f_l1 true))

  $ ./alpha_demo.exe < manytests/typed/015tuples.ml
  ---СС---
  
  let rec fix f x = ((f (fix f)) x);;
  let map f p = let (a, b) = p
  in ((f a), (f b));;
  let cc_ll_0 self l li x = ((li (self l)) x);;
  let cc_ll_1 self l = ((map ((cc_ll_0 self) l)) l);;
  let fixpoly l = ((fix cc_ll_1) l);;
  let feven p n = let (e, o) = p
  in if (n == 0) then 1 else o (n - 1);;
  let fodd p n = let (e, o) = p
  in if (n == 0) then 0 else e (n - 1);;
  let tie = (fixpoly (feven, fodd));;
  let rec meven n = if (n = 0) then 1 else modd (n - 1)
  and modd n = if (n = 0) then 1 else meven (n - 1);;
  let main = let () = (print_int (modd 1))
  in let () = (print_int (meven 2))
  in let (even, odd) = tie
  in let () = (print_int (odd 3))
  in let () = (print_int (even 4))
  in 0
  
  ---Alpha conv.---
  
  let rec fix_1 f x = ((f (fix_1 f)) x);;
  let map_1 f p = let (a_l1, b_l1) = p
  in ((f a_l1), (f b_l1));;
  let cc_ll_0_1 self l li x = ((li (self l)) x);;
  let cc_ll_1_1 self l = ((map_1 ((cc_ll_0_1 self) l)) l);;
  let fixpoly_1 l = ((fix_1 cc_ll_1_1) l);;
  let feven_1 p n = let (e_l1, o_l1) = p
  in if (n == 0) then 1 else o_l1 (n - 1);;
  let fodd_1 p n = let (e_l2, o_l2) = p
  in if (n == 0) then 0 else e_l2 (n - 1);;
  let tie_1 = (fixpoly_1 (feven_1, fodd_1));;
  let rec meven_1 n = if (n = 0) then 1 else modd_1 (n - 1)
  and modd_1 n = if (n = 0) then 1 else meven_1 (n - 1);;
  let main_1 = let () = (print_int (modd_1 1))
  in let () = (print_int (meven_1 2))
  in let (even_l1, odd_l1) = tie_1
  in let () = (print_int (odd_l1 3))
  in let () = (print_int (even_l1 4))
  in 0

  $ ./alpha_demo.exe < manytests/typed/016lists.ml
  ---СС---
  
  let rec length xs = match xs with
  | [] -> 0
  | h :: tl -> (1 + (length tl));;
  let rec cc_ll_0 acc xs = match xs with
  | [] -> acc
  | h :: tl -> (cc_ll_0 (acc + 1) tl);;
  let length_tail = let helper = cc_ll_0
  in (helper 0);;
  let rec map f xs = match xs with
  | [] -> []
  | a :: [] -> (f a) :: []
  | a :: b :: [] -> (f a) :: (f b) :: []
  | a :: b :: c :: [] -> (f a) :: (f b) :: (f c) :: []
  | a :: b :: c :: d :: tl -> (f a) :: (f b) :: (f c) :: (f d) :: ((map f) tl);;
  let rec append xs ys = match xs with
  | [] -> ys
  | x :: xs -> x :: ((append xs) ys);;
  let rec cc_ll_1 xs = match xs with
  | [] -> []
  | h :: tl -> ((append h) (cc_ll_1 tl));;
  let concat = let helper = cc_ll_1
  in helper;;
  let rec iter f xs = match xs with
  | [] -> ()
  | h :: tl -> let () = (f h)
  in ((iter f) tl);;
  let cc_ll_2 h a = (h, a);;
  let rec cartesian xs ys = match xs with
  | [] -> []
  | h :: tl -> ((append ((map (cc_ll_2 h)) ys)) ((cartesian tl) ys));;
  let main = let () = ((iter print_int) 1 :: 2 :: 3 :: [])
  in let () = (print_int (length ((cartesian 1 :: 2 :: []) 1 :: 2 :: 3 :: 4 :: [])))
  in 0
  
  ---Alpha conv.---
  
  let rec length_1 xs = match xs with
  | [] -> 0
  | h :: tl -> (1 + (length_1 tl));;
  let rec cc_ll_0_1 acc xs = match xs with
  | [] -> acc
  | h :: tl -> (cc_ll_0_1 (acc + 1) tl);;
  let length_tail_1 = let helper_l1 = cc_ll_0_1
  in (helper_l1 0);;
  let rec map_1 f xs = match xs with
  | [] -> []
  | a :: [] -> (f a) :: []
  | a :: b :: [] -> (f a) :: (f b) :: []
  | a :: b :: c :: [] -> (f a) :: (f b) :: (f c) :: []
  | a :: b :: c :: d :: tl -> (f a) :: (f b) :: (f c) :: (f d) :: ((map_1 f) tl);;
  let rec append_1 xs ys = match xs with
  | [] -> ys
  | x :: xs -> x :: ((append_1 xs) ys);;
  let rec cc_ll_1_1 xs = match xs with
  | [] -> []
  | h :: tl -> ((append_1 h) (cc_ll_1_1 tl));;
  let concat_1 = let helper_l2 = cc_ll_1_1
  in helper_l2;;
  let rec iter_1 f xs = match xs with
  | [] -> ()
  | h :: tl -> let () = (f h)
  in ((iter_1 f) tl);;
  let cc_ll_2_1 h a = (h, a);;
  let rec cartesian_1 xs ys = match xs with
  | [] -> []
  | h :: tl -> ((append_1 ((map_1 (cc_ll_2_1 h)) ys)) ((cartesian_1 tl) ys));;
  let main_1 = let () = ((iter_1 print_int) 1 :: 2 :: 3 :: [])
  in let () = (print_int (length_1 ((cartesian_1 1 :: 2 :: []) 1 :: 2 :: 3 :: 4 :: [])))
  in 0

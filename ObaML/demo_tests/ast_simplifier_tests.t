  $ ./ast_simplifier_tests.exe < manytests/typed/002fac.ml
  let rec fac_cps n k = 
  	if (n  =  1)
  	then (k 1)
  	else ((fac_cps (n  -  1)) (fun p -> (k (p  *  n))));;
  
  let main = 
  	let () = (print_int ((fac_cps 4) (fun print_int -> print_int))) in 0;;
  
  $ ./ast_simplifier_tests.exe < manytests/typed/004manyargs.ml
  let wrap f = 
  	if (1  =  1)
  	then f
  	else f;;
  
  let test3 a b c = 
  	let a = (print_int a) in 
  	let b = (print_int b) in 
  	let c = (print_int c) in 0;;
  
  let test10 a b c d e f g h i j = (((((((((a  +  b)  +  c)  +  d)  +  e)  +  f)  +  g)  +  h)  +  i)  +  j);;
  
  let main = 
  	let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in 
  	let () = (print_int rez) in 
  	let temp2 = ((((wrap test3) 1) 10) 100) in 0;;
  

  $ ./ast_simplifier_tests.exe << EOF
  > let map f p = let (a,b) = p in (f a, f b)
  let map f p = 
  	let #pat#0 = p in 
  	let a = ((#tuple_getter# 0) #pat#0) in 
  	let b = ((#tuple_getter# 1) #pat#0) in ((f a), (f b));;
  
  $ ./ast_simplifier_tests.exe << EOF
  > let c = let (a, (b, c)) = (fun x -> (4, (x, x))) 4 in (a, b, c);;
  let c = 
  	let #pat#0 = ((fun x -> (4, (x, x))) 4) in 
  	let a = ((#tuple_getter# 0) #pat#0) in 
  	let b = ((#tuple_getter# 0) ((#tuple_getter# 1) #pat#0)) in 
  	let c = ((#tuple_getter# 1) ((#tuple_getter# 1) #pat#0)) in (a, b, c);;
  

  $ ./ast_simplifier_tests.exe << EOF
  > let a b = match b with | (5, (b, c), 4) -> (b, c);;
  let a b = 
  	let #pat#0 = b in 
  	if ((((#tuple_getter# 0) #pat#0)  =  5)  &&  (((#tuple_getter# 2) #pat#0)  =  4))
  	then 
  	let b = ((#tuple_getter# 0) ((#tuple_getter# 1) #pat#0)) in 
  	let c = ((#tuple_getter# 1) ((#tuple_getter# 1) #pat#0)) in (b, c)
  	else (#matching_failed# ());;
  

  $ ./ast_simplifier_tests.exe << EOF
  > let a = match 1 :: [] with | 2 :: a -> a | 1 :: [] -> [];;
  let a = 
  	let #pat#0 = (1 :: []) in 
  	if (((#list_length_getter# #pat#0)  >=  1)  &&  ((#list_head_getter# #pat#0)  =  2))
  	then 
  	let a = (#list_tail_getter# #pat#0) in a
  	else 
  	if (((#list_length_getter# #pat#0)  =  1)  &&  ((#list_head_getter# #pat#0)  =  1))
  	then []
  	else (#matching_failed# ());;
  

  $ ./ast_simplifier_tests.exe << EOF
  > let a b = fun x () (y, z) (x1, x2) -> b x;;
  let a b = (fun x () #pat#0 #pat#1 -> 
  	let y = ((#tuple_getter# 0) #pat#0) in 
  	let z = ((#tuple_getter# 1) #pat#0) in 
  	let x1 = ((#tuple_getter# 0) #pat#1) in 
  	let x2 = ((#tuple_getter# 1) #pat#1) in (b x));;
  

  $ ./ast_simplifier_tests.exe < manytests/typed/015tuples.ml
  let rec fix f x = ((f (fix f)) x);;
  
  let map f p = 
  	let #pat#0 = p in 
  	let a = ((#tuple_getter# 0) #pat#0) in 
  	let b = ((#tuple_getter# 1) #pat#0) in ((f a), (f b));;
  
  let fixpoly l = ((fix (fun self l -> ((map (fun li x -> ((li (self l)) x))) l))) l);;
  
  let feven p n = 
  	let #pat#0 = p in 
  	let e = ((#tuple_getter# 0) #pat#0) in 
  	let o = ((#tuple_getter# 1) #pat#0) in 
  	if (n  ==  0)
  	then 1
  	else (o (n  -  1));;
  
  let fodd p n = 
  	let #pat#0 = p in 
  	let e = ((#tuple_getter# 0) #pat#0) in 
  	let o = ((#tuple_getter# 1) #pat#0) in 
  	if (n  ==  0)
  	then 0
  	else (e (n  -  1));;
  
  let tie = (fixpoly (feven, fodd));;
  
  let rec meven n = 
  	if (n  =  0)
  	then 1
  	else (modd (n  -  1))
  and modd n = 
  	if (n  =  0)
  	then 1
  	else (meven (n  -  1));;
  
  let main = 
  	let () = (print_int (modd 1)) in 
  	let () = (print_int (meven 2)) in 
  	let #pat#0 = tie in 
  	let even = ((#tuple_getter# 0) #pat#0) in 
  	let odd = ((#tuple_getter# 1) #pat#0) in 
  	let () = (print_int (odd 3)) in 
  	let () = (print_int (even 4)) in 0;;
  

  $ ./ast_simplifier_tests.exe < manytests/typed/016lists.ml
  let rec length xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then 0
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in (1  +  (length tl))
  	else (#matching_failed# ());;
  
  let length_tail = 
  	let rec helper acc xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then acc
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in ((helper (acc  +  1)) tl)
  	else (#matching_failed# ()) in (helper 0);;
  
  let rec map f xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then []
  	else 
  	if ((#list_length_getter# #pat#0)  =  1)
  	then 
  	let a = (#list_head_getter# #pat#0) in ((f a) :: [])
  	else 
  	if ((#list_length_getter# #pat#0)  =  2)
  	then 
  	let a = (#list_head_getter# #pat#0) in 
  	let b = (#list_head_getter# (#list_tail_getter# #pat#0)) in ((f a) :: ((f b) :: []))
  	else 
  	if ((#list_length_getter# #pat#0)  =  3)
  	then 
  	let a = (#list_head_getter# #pat#0) in 
  	let b = (#list_head_getter# (#list_tail_getter# #pat#0)) in 
  	let c = (#list_head_getter# (#list_tail_getter# (#list_tail_getter# #pat#0))) in ((f a) :: ((f b) :: ((f c) :: [])))
  	else 
  	if ((#list_length_getter# #pat#0)  >=  4)
  	then 
  	let a = (#list_head_getter# #pat#0) in 
  	let b = (#list_head_getter# (#list_tail_getter# #pat#0)) in 
  	let c = (#list_head_getter# (#list_tail_getter# (#list_tail_getter# #pat#0))) in 
  	let d = (#list_head_getter# (#list_tail_getter# (#list_tail_getter# (#list_tail_getter# #pat#0)))) in 
  	let tl = (#list_tail_getter# (#list_tail_getter# (#list_tail_getter# (#list_tail_getter# #pat#0)))) in ((f a) :: ((f b) :: ((f c) :: ((f d) :: ((map f) tl)))))
  	else (#matching_failed# ());;
  
  let rec append xs ys = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then ys
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let x = (#list_head_getter# #pat#0) in 
  	let xs = (#list_tail_getter# #pat#0) in (x :: ((append xs) ys))
  	else (#matching_failed# ());;
  
  let concat = 
  	let rec helper xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then []
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in ((append h) (helper tl))
  	else (#matching_failed# ()) in helper;;
  
  let rec iter f xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then ()
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in 
  	let () = (f h) in ((iter f) tl)
  	else (#matching_failed# ());;
  
  let rec cartesian xs ys = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then []
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in ((append ((map (fun a -> (h, a))) ys)) ((cartesian tl) ys))
  	else (#matching_failed# ());;
  
  let main = 
  	let () = ((iter print_int) (1 :: (2 :: (3 :: [])))) in 
  	let () = (print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: []))))))) in 0;;
  

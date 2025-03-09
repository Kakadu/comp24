  $ ./simplifier_tests.exe < manytests/typed/002fac.ml
  let rec fac_cps n k = 
  	if (n  =  1)
  	then (k 1)
  	else ((fac_cps (n  -  1)) (fun p -> (k (p  *  n))));;
  
  let main = 
  	let () = (print_int ((fac_cps 4) (fun oba0 -> oba0))) in 0;;
  
  $ ./simplifier_tests.exe < manytests/typed/004manyargs.ml
  let wrap f = 
  	if (1  =  1)
  	then f
  	else f;;
  
  let test3 a b c = 
  	let oba0 = (print_int a) in 
  	let oba1 = (print_int b) in 
  	let oba2 = (print_int c) in 0;;
  
  let test10 oba3 oba4 oba5 d e oba6 g h i j = (((((((((oba3  +  oba4)  +  oba5)  +  d)  +  e)  +  oba6)  +  g)  +  h)  +  i)  +  j);;
  
  let main = 
  	let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in 
  	let () = (print_int rez) in 
  	let temp2 = ((((wrap test3) 1) 10) 100) in 0;;
  

  $ ./simplifier_tests.exe << EOF
  > let map f p = let (a,b) = p in (f a, f b)
  let map f p = 
  	let pat0 = p in 
  	let a = ((tuple_getter 0) pat0) in 
  	let b = ((tuple_getter 1) pat0) in ((f a), (f b));;
  
  $ ./simplifier_tests.exe << EOF
  > let c = let (a, (b, c)) = (fun x -> (4, (x, x))) 4 in (a, b, c);;
  let c = 
  	let pat0 = ((fun x -> (4, (x, x))) 4) in 
  	let a = ((tuple_getter 0) pat0) in 
  	let b = ((tuple_getter 0) ((tuple_getter 1) pat0)) in 
  	let oba0 = ((tuple_getter 1) ((tuple_getter 1) pat0)) in (a, b, oba0);;
  

  $ ./simplifier_tests.exe << EOF
  > let a b = match b with | (5, (b, c), 4) -> (b, c);;
  let a b = 
  	let pat0 = b in 
  	if ((((tuple_getter 0) pat0)  =  5)  &&  (((tuple_getter 2) pat0)  =  4))
  	then 
  	let oba0 = ((tuple_getter 0) ((tuple_getter 1) pat0)) in 
  	let c = ((tuple_getter 1) ((tuple_getter 1) pat0)) in (oba0, c)
  	else (matching_failed ());;
  

  $ ./simplifier_tests.exe << EOF
  > let a = match 1 :: [] with | 2 :: a -> a | 1 :: [] -> [];;
  let a = 
  	let pat0 = (1 :: []) in 
  	if (((list_length_getter pat0)  >=  1)  &&  ((list_head_getter pat0)  =  2))
  	then 
  	let oba0 = (list_tail_getter pat0) in oba0
  	else 
  	if (((list_length_getter pat0)  =  1)  &&  ((list_head_getter pat0)  =  1))
  	then []
  	else (matching_failed ());;
  

  $ ./simplifier_tests.exe << EOF
  > let a b = fun x () (y, z) (x1, x2) -> b x;;
  let a b = (fun x () pat0 pat1 -> 
  	let y = ((tuple_getter 0) pat0) in 
  	let z = ((tuple_getter 1) pat0) in 
  	let x1 = ((tuple_getter 0) pat1) in 
  	let x2 = ((tuple_getter 1) pat1) in (b x));;
  

  $ ./simplifier_tests.exe < manytests/typed/015tuples.ml
  let rec fix f x = ((f (fix f)) x);;
  
  let map oba0 p = 
  	let pat0 = p in 
  	let a = ((tuple_getter 0) pat0) in 
  	let b = ((tuple_getter 1) pat0) in ((oba0 a), (oba0 b));;
  
  let fixpoly l = ((fix (fun self oba1 -> ((map (fun li oba2 -> ((li (self oba1)) oba2))) oba1))) l);;
  
  let feven oba3 n = 
  	let pat0 = oba3 in 
  	let e = ((tuple_getter 0) pat0) in 
  	let o = ((tuple_getter 1) pat0) in 
  	if (n  =  0)
  	then 1
  	else (o (n  -  1));;
  
  let fodd oba4 oba5 = 
  	let pat0 = oba4 in 
  	let oba6 = ((tuple_getter 0) pat0) in 
  	let oba7 = ((tuple_getter 1) pat0) in 
  	if (oba5  =  0)
  	then 0
  	else (oba6 (oba5  -  1));;
  
  let tie = (fixpoly (feven, fodd));;
  
  let rec meven oba9 = 
  	if (oba9  =  0)
  	then 1
  	else (modd (oba9  -  1))
  and modd oba8 = 
  	if (oba8  =  0)
  	then 1
  	else (meven (oba8  -  1));;
  
  let main = 
  	let () = (print_int (modd 1)) in 
  	let () = (print_int (meven 2)) in 
  	let pat0 = tie in 
  	let even = ((tuple_getter 0) pat0) in 
  	let odd = ((tuple_getter 1) pat0) in 
  	let () = (print_int (odd 3)) in 
  	let () = (print_int (even 4)) in 0;;
  

  $ ./simplifier_tests.exe < manytests/typed/016lists.ml
  let rec length xs = 
  	let pat0 = xs in 
  	if (pat0  =  [])
  	then 0
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let h = (list_head_getter pat0) in 
  	let tl = (list_tail_getter pat0) in (1  +  (length tl))
  	else (matching_failed ());;
  
  let length_tail = 
  	let rec helper acc oba0 = 
  	let pat0 = oba0 in 
  	if (pat0  =  [])
  	then acc
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let oba1 = (list_head_getter pat0) in 
  	let oba2 = (list_tail_getter pat0) in ((helper (acc  +  1)) oba2)
  	else (matching_failed ()) in (helper 0);;
  
  let rec map f oba3 = 
  	let pat0 = oba3 in 
  	if (pat0  =  [])
  	then []
  	else 
  	if ((list_length_getter pat0)  =  1)
  	then 
  	let a = (list_head_getter pat0) in ((f a) :: [])
  	else 
  	if ((list_length_getter pat0)  =  2)
  	then 
  	let oba4 = (list_head_getter pat0) in 
  	let b = (list_head_getter (list_tail_getter pat0)) in ((f oba4) :: ((f b) :: []))
  	else 
  	if ((list_length_getter pat0)  =  3)
  	then 
  	let oba5 = (list_head_getter pat0) in 
  	let oba6 = (list_head_getter (list_tail_getter pat0)) in 
  	let c = (list_head_getter (list_tail_getter (list_tail_getter pat0))) in ((f oba5) :: ((f oba6) :: ((f c) :: [])))
  	else 
  	if ((list_length_getter pat0)  >=  4)
  	then 
  	let oba7 = (list_head_getter pat0) in 
  	let oba8 = (list_head_getter (list_tail_getter pat0)) in 
  	let oba9 = (list_head_getter (list_tail_getter (list_tail_getter pat0))) in 
  	let d = (list_head_getter (list_tail_getter (list_tail_getter (list_tail_getter pat0)))) in 
  	let oba10 = (list_tail_getter (list_tail_getter (list_tail_getter (list_tail_getter pat0)))) in ((f oba7) :: ((f oba8) :: ((f oba9) :: ((f d) :: ((map f) oba10)))))
  	else (matching_failed ());;
  
  let rec append oba11 ys = 
  	let pat0 = oba11 in 
  	if (pat0  =  [])
  	then ys
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let x = (list_head_getter pat0) in 
  	let oba12 = (list_tail_getter pat0) in (x :: ((append oba12) ys))
  	else (matching_failed ());;
  
  let concat = 
  	let rec oba13 oba14 = 
  	let pat0 = oba14 in 
  	if (pat0  =  [])
  	then []
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let oba15 = (list_head_getter pat0) in 
  	let oba16 = (list_tail_getter pat0) in ((append oba15) (oba13 oba16))
  	else (matching_failed ()) in oba13;;
  
  let rec iter oba17 oba18 = 
  	let pat0 = oba18 in 
  	if (pat0  =  [])
  	then ()
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let oba19 = (list_head_getter pat0) in 
  	let oba20 = (list_tail_getter pat0) in 
  	let () = (oba17 oba19) in ((iter oba17) oba20)
  	else (matching_failed ());;
  
  let rec cartesian oba21 oba22 = 
  	let pat0 = oba21 in 
  	if (pat0  =  [])
  	then []
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let oba23 = (list_head_getter pat0) in 
  	let oba24 = (list_tail_getter pat0) in ((append ((map (fun oba25 -> (oba23, oba25))) oba22)) ((cartesian oba24) oba22))
  	else (matching_failed ());;
  
  let main = 
  	let () = ((iter print_int) (1 :: (2 :: (3 :: [])))) in 
  	let () = (print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: []))))))) in 0;;
  

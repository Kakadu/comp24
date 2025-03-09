  $ ./lambda_lifting_tests.exe < manytests/typed/001fac.ml
  Types:
  val fac : int -> int
  val main : int
  
  Converted structure:
  let rec fac n = 
  	if (n  <=  1)
  	then 1
  	else (n  *  (fac (n  -  1)));;
  
  let main = 
  	let () = (print_int (fac 4)) in 0;;
  
  
  Types after conversions:
  val fac : int -> int
  val main : int

  $ ./lambda_lifting_tests.exe < manytests/typed/002fac.ml
  Types:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Converted structure:
  let oba1 k n p = (k (p  *  n));;
  
  let rec fac_cps n k = 
  	if (n  =  1)
  	then (k 1)
  	else ((fac_cps (n  -  1)) ((oba1 k) n));;
  
  let oba2 oba0 = oba0;;
  
  let main = 
  	let () = (print_int ((fac_cps 4) oba2)) in 0;;
  
  
  Types after conversions:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  val oba1 : (int -> 'a) -> int -> int -> 'a
  val oba2 : 'a -> 'a

  $ ./lambda_lifting_tests.exe << EOF
  > let rec y x = let a = y x in let b = fun x -> a + x in b 5;;
  Types:
  val y : 'a -> int
  
  Converted structure:
  let b a oba0 = (a  +  oba0);;
  
  let rec y x = 
  	let a = (y x) in ((b a) 5);;
  
  
  Types after conversions:
  val b : int -> int -> int
  val y : 'a -> int

  $ ./lambda_lifting_tests.exe << EOF
  > let rec y x = let a = fun z -> y x + z in let b = fun x -> a 5 + x in b 5;;
  Types:
  val y : 'a -> int
  
  Converted structure:
  let a x y z = ((y x)  +  z);;
  
  let b a oba0 = ((a 5)  +  oba0);;
  
  let rec y x = ((b ((a x) y)) 5);;
  
  
  Types after conversions:
  val a : 'a -> ('a -> int) -> int -> int
  val b : (int -> int) -> int -> int
  val y : 'a -> int

  $ ./lambda_lifting_tests.exe << EOF
  > let f x y = 
  >    let x z = y + z in 
  >    let y z = x 1 + z in
  >    x 1 + y 2;;
  Types:
  val f : 'a -> int -> int
  
  Converted structure:
  let oba0 y z = (y  +  z);;
  
  let oba1 oba0 oba2 = ((oba0 1)  +  oba2);;
  
  let f x y = (((oba0 y) 1)  +  ((oba1 (oba0 y)) 2));;
  
  
  Types after conversions:
  val f : 'a -> int -> int
  val oba0 : int -> int -> int
  val oba1 : (int -> int) -> int -> int

  $ ./lambda_lifting_tests.exe << EOF
  > let rev = fun lst ->
  >    let rec helper = fun acc -> (fun lst ->
  >    match lst with
  >      | [] -> acc
  >      | h :: tl -> helper (h :: acc) tl)
  >    in
  >    helper [] lst
  >  let reversed1 = rev (1 :: 2 :: 3 :: 4 :: 5 :: [])
  >  let reversed2 = rev (true :: false :: false :: false :: [])
  Types:
  val rev : 'a list -> 'a list
  val reversed1 : int list
  val reversed2 : bool list
  
  Converted structure:
  let oba1 acc helper oba0 = 
  	let pat0 = oba0 in 
  	if (pat0  =  [])
  	then acc
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let h = (list_head_getter pat0) in 
  	let tl = (list_tail_getter pat0) in ((helper (h :: acc)) tl)
  	else (matching_failed ());;
  
  let rec helper acc = ((oba1 acc) helper);;
  
  let rev lst = ((helper []) lst);;
  
  let reversed1 = (rev (1 :: (2 :: (3 :: (4 :: (5 :: []))))));;
  
  let reversed2 = (rev (true :: (false :: (false :: (false :: [])))));;
  
  
  Types after conversions:
  val helper : 'a list -> 'a list -> 'a list
  val oba1 : 'a list -> ('a list -> 'a list -> 'a list) -> 'a list -> 'a list
  val rev : 'a list -> 'a list
  val reversed1 : int list
  val reversed2 : bool list

  $ ./lambda_lifting_tests.exe < manytests/typed/016lists.ml
  Types:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  
  Converted structure:
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
  
  let rec helper acc oba0 = 
  	let pat0 = oba0 in 
  	if (pat0  =  [])
  	then acc
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let oba1 = (list_head_getter pat0) in 
  	let oba2 = (list_tail_getter pat0) in ((helper (acc  +  1)) oba2)
  	else (matching_failed ());;
  
  let length_tail = (helper 0);;
  
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
  
  let rec oba13 oba14 = 
  	let pat0 = oba14 in 
  	if (pat0  =  [])
  	then []
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let oba15 = (list_head_getter pat0) in 
  	let oba16 = (list_tail_getter pat0) in ((append oba15) (oba13 oba16))
  	else (matching_failed ());;
  
  let concat = oba13;;
  
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
  
  let oba26 oba23 oba25 = (oba23, oba25);;
  
  let rec cartesian oba21 oba22 = 
  	let pat0 = oba21 in 
  	if (pat0  =  [])
  	then []
  	else 
  	if ((list_length_getter pat0)  >=  1)
  	then 
  	let oba23 = (list_head_getter pat0) in 
  	let oba24 = (list_tail_getter pat0) in ((append ((map (oba26 oba23)) oba22)) ((cartesian oba24) oba22))
  	else (matching_failed ());;
  
  let main = 
  	let () = ((iter print_int) (1 :: (2 :: (3 :: [])))) in 
  	let () = (print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: []))))))) in 0;;
  
  
  Types after conversions:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val helper : int -> 'a list -> int
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val oba13 : 'a list list -> 'a list
  val oba26 : 'a -> 'b -> 'a * 'b

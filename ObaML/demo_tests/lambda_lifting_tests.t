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
  let foba0 k n p = (k (p  *  n));;
  
  let rec fac_cps n k = 
  	if (n  =  1)
  	then (k 1)
  	else ((fac_cps (n  -  1)) ((foba0 k) n));;
  
  let foba1 print_int = print_int;;
  
  let main = 
  	let () = (print_int ((fac_cps 4) foba1)) in 0;;
  
  
  Types after conversions:
  val fac_cps : int -> (int -> 'a) -> 'a
  val foba0 : (int -> 'a) -> int -> int -> 'a
  val foba1 : 'a -> 'a
  val main : int

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
  
  let oba1 oba0 z = ((oba0 1)  +  z);;
  
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
  let foba0 acc helper oba0 = 
  	let #pat#0 = oba0 in 
  	if (#pat#0  =  [])
  	then acc
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in ((helper (h :: acc)) tl)
  	else (#matching_failed# ());;
  
  let rec helper acc = ((foba0 acc) helper);;
  
  let rev lst = ((helper []) lst);;
  
  let reversed1 = (rev (1 :: (2 :: (3 :: (4 :: (5 :: []))))));;
  
  let reversed2 = (rev (true :: (false :: (false :: (false :: [])))));;
  
  
  Types after conversions:
  val foba0 : 'a list -> ('a list -> 'a list -> 'a list) -> 'a list -> 'a list
  val helper : 'a list -> 'a list -> 'a list
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
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then 0
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in (1  +  (length tl))
  	else (#matching_failed# ());;
  
  let rec helper acc xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then acc
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in ((helper (acc  +  1)) tl)
  	else (#matching_failed# ());;
  
  let length_tail = (helper 0);;
  
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
  	let oba0 = (#list_tail_getter# #pat#0) in (x :: ((append oba0) ys))
  	else (#matching_failed# ());;
  
  let rec helper xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then []
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in ((append h) (helper tl))
  	else (#matching_failed# ());;
  
  let concat = helper;;
  
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
  
  let foba0 h a = (h, a);;
  
  let rec cartesian xs ys = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then []
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in ((append ((map (foba0 h)) ys)) ((cartesian tl) ys))
  	else (#matching_failed# ());;
  
  let main = 
  	let () = ((iter print_int) (1 :: (2 :: (3 :: [])))) in 
  	let () = (print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: []))))))) in 0;;
  
  
  Types after conversions:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val foba0 : 'a -> 'b -> 'a * 'b
  val helper : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list

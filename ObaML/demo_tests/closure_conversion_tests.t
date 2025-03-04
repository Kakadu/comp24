  $ ./closure_conversion_tests.exe < manytests/typed/001fac.ml
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

  $ ./closure_conversion_tests.exe < manytests/typed/002fac.ml
  Types:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Converted structure:
  let rec fac_cps n k = 
  	if (n  =  1)
  	then (k 1)
  	else ((fac_cps (n  -  1)) (((fun k n p -> (k (p  *  n))) k) n));;
  
  let main = 
  	let () = (print_int ((fac_cps 4) (fun print_int -> print_int))) in 0;;
  
  
  Types after conversions:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ./closure_conversion_tests.exe << EOF
  > let rec y = fun x -> let a = y x in let b = fun x -> a + x in b 5;;
  Types:
  val y : 'a -> int
  
  Converted structure:
  let rec y x = 
  	let a = (y x) in 
  	let b a oba0 = (a  +  oba0) in ((b a) 5);;
  
  
  Types after conversions:
  val y : 'a -> int

  $ ./closure_conversion_tests.exe << EOF
  > let rec y = fun x -> let a = fun z -> y x + z in let b = fun x -> a 5 + x in b 5;;
  Types:
  val y : 'a -> int
  
  Converted structure:
  let rec y x = 
  	let a x y z = ((y x)  +  z) in 
  	let b a oba0 = ((a 5)  +  oba0) in ((b ((a x) y)) 5);;
  
  
  Types after conversions:
  val y : 'a -> int

  $ ./closure_conversion_tests.exe << EOF
  > let f x y = 
  >    let x z = y + z in 
  >    let y z = x 1 + z in
  >    x 1 + y 2;;
  Types:
  val f : 'a -> int -> int
  
  Converted structure:
  let f x y = 
  	let oba0 y z = (y  +  z) in 
  	let oba1 oba0 oba2 = ((oba0 1)  +  oba2) in (((oba0 y) 1)  +  ((oba1 (oba0 y)) 2));;
  
  
  Types after conversions:
  val f : 'a -> int -> int

  $ ./closure_conversion_tests.exe << EOF
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
  let rev lst = 
  	let rec helper acc = (((fun acc helper oba0 -> 
  	let oba1 = oba0 in 
  	if (oba1  =  [])
  	then acc
  	else 
  	if ((#list_length_getter# oba1)  >=  1)
  	then 
  	let h = (#list_head_getter# oba1) in 
  	let tl = (#list_tail_getter# oba1) in ((helper (h :: acc)) tl)
  	else (#matching_failed# ())) acc) helper) in ((helper []) lst);;
  
  let reversed1 = (rev (1 :: (2 :: (3 :: (4 :: (5 :: []))))));;
  
  let reversed2 = (rev (true :: (false :: (false :: (false :: [])))));;
  
  
  Types after conversions:
  val rev : 'a list -> 'a list
  val reversed1 : int list
  val reversed2 : bool list

  $ ./closure_conversion_tests.exe < manytests/typed/016lists.ml
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
  	let oba0 = xs in 
  	if (oba0  =  [])
  	then 0
  	else 
  	if ((#list_length_getter# oba0)  >=  1)
  	then 
  	let h = (#list_head_getter# oba0) in 
  	let tl = (#list_tail_getter# oba0) in (1  +  (length tl))
  	else (#matching_failed# ());;
  
  let length_tail = 
  	let rec helper acc oba1 = 
  	let oba2 = oba1 in 
  	if (oba2  =  [])
  	then acc
  	else 
  	if ((#list_length_getter# oba2)  >=  1)
  	then 
  	let oba3 = (#list_head_getter# oba2) in 
  	let oba4 = (#list_tail_getter# oba2) in ((helper (acc  +  1)) oba4)
  	else (#matching_failed# ()) in (helper 0);;
  
  let rec map f oba5 = 
  	let oba6 = oba5 in 
  	if (oba6  =  [])
  	then []
  	else 
  	if ((#list_length_getter# oba6)  =  1)
  	then 
  	let a = (#list_head_getter# oba6) in ((f a) :: [])
  	else 
  	if ((#list_length_getter# oba6)  =  2)
  	then 
  	let oba7 = (#list_head_getter# oba6) in 
  	let b = (#list_head_getter# (#list_tail_getter# oba6)) in ((f oba7) :: ((f b) :: []))
  	else 
  	if ((#list_length_getter# oba6)  =  3)
  	then 
  	let oba8 = (#list_head_getter# oba6) in 
  	let oba9 = (#list_head_getter# (#list_tail_getter# oba6)) in 
  	let c = (#list_head_getter# (#list_tail_getter# (#list_tail_getter# oba6))) in ((f oba8) :: ((f oba9) :: ((f c) :: [])))
  	else 
  	if ((#list_length_getter# oba6)  >=  4)
  	then 
  	let oba10 = (#list_head_getter# oba6) in 
  	let oba11 = (#list_head_getter# (#list_tail_getter# oba6)) in 
  	let oba12 = (#list_head_getter# (#list_tail_getter# (#list_tail_getter# oba6))) in 
  	let d = (#list_head_getter# (#list_tail_getter# (#list_tail_getter# (#list_tail_getter# oba6)))) in 
  	let oba13 = (#list_tail_getter# (#list_tail_getter# (#list_tail_getter# (#list_tail_getter# oba6)))) in ((f oba10) :: ((f oba11) :: ((f oba12) :: ((f d) :: ((map f) oba13)))))
  	else (#matching_failed# ());;
  
  let rec append oba14 ys = 
  	let oba15 = oba14 in 
  	if (oba15  =  [])
  	then ys
  	else 
  	if ((#list_length_getter# oba15)  >=  1)
  	then 
  	let x = (#list_head_getter# oba15) in 
  	let oba16 = (#list_tail_getter# oba15) in (x :: ((append oba16) ys))
  	else (#matching_failed# ());;
  
  let concat = 
  	let rec oba17 oba18 = 
  	let oba19 = oba18 in 
  	if (oba19  =  [])
  	then []
  	else 
  	if ((#list_length_getter# oba19)  >=  1)
  	then 
  	let oba20 = (#list_head_getter# oba19) in 
  	let oba21 = (#list_tail_getter# oba19) in ((append oba20) (oba17 oba21))
  	else (#matching_failed# ()) in oba17;;
  
  let rec iter oba22 oba23 = 
  	let oba24 = oba23 in 
  	if (oba24  =  [])
  	then ()
  	else 
  	if ((#list_length_getter# oba24)  >=  1)
  	then 
  	let oba25 = (#list_head_getter# oba24) in 
  	let oba26 = (#list_tail_getter# oba24) in 
  	let () = (oba22 oba25) in ((iter oba22) oba26)
  	else (#matching_failed# ());;
  
  let rec cartesian oba27 oba28 = 
  	let oba29 = oba27 in 
  	if (oba29  =  [])
  	then []
  	else 
  	if ((#list_length_getter# oba29)  >=  1)
  	then 
  	let oba30 = (#list_head_getter# oba29) in 
  	let oba31 = (#list_tail_getter# oba29) in ((append ((map ((fun oba30 oba32 -> (oba30, oba32)) oba30)) oba28)) ((cartesian oba31) oba28))
  	else (#matching_failed# ());;
  
  let main = 
  	let () = ((iter print_int) (1 :: (2 :: (3 :: [])))) in 
  	let () = (print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: []))))))) in 0;;
  
  
  Types after conversions:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list

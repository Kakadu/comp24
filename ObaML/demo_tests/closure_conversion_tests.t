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

  $ ./closure_conversion_tests.exe < manytests/typed/003fib.ml
  Types:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  
  Converted structure:
  let rec fib_acc a b n = 
  	if (n  =  1)
  	then b
  	else 
  	let n1 = (n  -  1) in 
  	let ab = (a  +  b) in (((fib_acc b) ab) n1);;
  
  let rec fib n = 
  	if (n  <  2)
  	then n
  	else ((fib (n  -  1))  +  (fib (n  -  2)));;
  
  let main = 
  	let () = (print_int (((fib_acc 0) 1) 4)) in 
  	let () = (print_int (fib 4)) in 0;;
  
  
  Types after conversions:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int

  $ ./closure_conversion_tests.exe < manytests/typed/004manyargs.ml
  Types:
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  
  Converted structure:
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
  
  
  Types after conversions:
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a

  $ ./closure_conversion_tests.exe < manytests/typed/005fix.ml
  Types:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  
  Converted structure:
  let rec fix f x = ((f (fix f)) x);;
  
  let fac self n = 
  	if (n  <=  1)
  	then 1
  	else (n  *  (self (n  -  1)));;
  
  let main = 
  	let () = (print_int ((fix fac) 6)) in 0;;
  
  
  Types after conversions:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int

  $ ./closure_conversion_tests.exe < manytests/typed/006partial.ml
  Types:
  val foo : int -> int
  val main : int
  
  Converted structure:
  let foo b = 
  	if b
  	then (fun foo -> (foo  +  2))
  	else (fun foo -> (foo  *  10));;
  
  let foo x = ((foo true) ((foo false) ((foo true) ((foo false) x))));;
  
  let main = 
  	let () = (print_int (foo 11)) in 0;;
  
  
  Types after conversions:
  val foo : int -> int
  val main : int

  $ ./closure_conversion_tests.exe < manytests/typed/006partial2.ml
  Types:
  val foo : int -> int -> int -> int
  val main : int
  
  Converted structure:
  let foo a b c = 
  	let () = (print_int a) in 
  	let () = (print_int b) in 
  	let () = (print_int c) in (a  +  (b  *  c));;
  
  let main = 
  	let foo = (foo 1) in 
  	let foo = (foo 2) in 
  	let foo = (foo 3) in 
  	let () = (print_int foo) in 0;;
  
  
  Types after conversions:
  val foo : int -> int -> int -> int
  val main : int

  $ ./closure_conversion_tests.exe < manytests/typed/006partial3.ml
  Types:
  val foo : int -> int -> int -> unit
  val main : int
  
  Converted structure:
  let foo a = 
  	let () = (print_int a) in (fun b -> 
  	let () = (print_int b) in (fun c -> (print_int c)));;
  
  let main = 
  	let () = (((foo 4) 8) 9) in 0;;
  
  
  Types after conversions:
  val foo : int -> int -> int -> unit
  val main : int

  $ ./closure_conversion_tests.exe < manytests/typed/007order.ml
  Types:
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  
  Converted structure:
  let _start () () a () b _c () d __ = 
  	let () = (print_int (a  +  b)) in 
  	let () = (print_int __) in (((a  *  b)  /  _c)  +  d);;
  
  let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (( ~- ) 1))) 10000) (( ~- ) 555555)));;
  
  
  Types after conversions:
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ ./closure_conversion_tests.exe < manytests/typed/009let_poly.ml
  Types:
  val temp : int * bool
  
  Converted structure:
  let temp = 
  	let f x = x in ((f 1), (f true));;
  
  
  Types after conversions:
  val temp : int * bool

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
  	let rec helper #list_head_getter# #list_length_getter# #list_tail_getter# #matching_failed# acc xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then acc
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in ((((((helper #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#) (acc  +  1)) tl)
  	else (#matching_failed# ()) in (((((helper #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#) 0);;
  
  let rec map f xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then []
  	else 
  	if ((#list_length_getter# #pat#0)  =  1)
  	then 
  	let a = (#list_head_getter# #pat#0) in (f a) :: []
  	else 
  	if ((#list_length_getter# #pat#0)  =  2)
  	then 
  	let a = (#list_head_getter# #pat#0) in 
  	let b = (#list_head_getter# (#list_tail_getter# #pat#0)) in (f a) :: (f b) :: []
  	else 
  	if ((#list_length_getter# #pat#0)  =  3)
  	then 
  	let a = (#list_head_getter# #pat#0) in 
  	let b = (#list_head_getter# (#list_tail_getter# #pat#0)) in 
  	let c = (#list_head_getter# (#list_tail_getter# (#list_tail_getter# #pat#0))) in (f a) :: (f b) :: (f c) :: []
  	else 
  	if ((#list_length_getter# #pat#0)  >=  4)
  	then 
  	let a = (#list_head_getter# #pat#0) in 
  	let b = (#list_head_getter# (#list_tail_getter# #pat#0)) in 
  	let c = (#list_head_getter# (#list_tail_getter# (#list_tail_getter# #pat#0))) in 
  	let d = (#list_head_getter# (#list_tail_getter# (#list_tail_getter# (#list_tail_getter# #pat#0)))) in 
  	let tl = (#list_tail_getter# (#list_tail_getter# (#list_tail_getter# (#list_tail_getter# #pat#0)))) in (f a) :: (f b) :: (f c) :: (f d) :: ((map f) tl)
  	else (#matching_failed# ());;
  
  let rec append xs ys = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then ys
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let x = (#list_head_getter# #pat#0) in 
  	let xs = (#list_tail_getter# #pat#0) in x :: ((append xs) ys)
  	else (#matching_failed# ());;
  
  let concat = 
  	let rec helper #list_head_getter# #list_length_getter# #list_tail_getter# #matching_failed# xs = 
  	let #pat#0 = xs in 
  	if (#pat#0  =  [])
  	then []
  	else 
  	if ((#list_length_getter# #pat#0)  >=  1)
  	then 
  	let h = (#list_head_getter# #pat#0) in 
  	let tl = (#list_tail_getter# #pat#0) in ((append h) (((((helper #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#) tl))
  	else (#matching_failed# ()) in ((((helper #list_head_getter#) #list_length_getter#) #list_tail_getter#) #matching_failed#);;
  
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
  	let tl = (#list_tail_getter# #pat#0) in ((append ((map ((fun h a -> (h, a)) h)) ys)) ((cartesian tl) ys))
  	else (#matching_failed# ());;
  
  let main = 
  	let () = ((iter print_int) 1 :: 2 :: 3 :: []) in 
  	let () = (print_int (length ((cartesian 1 :: 2 :: []) 1 :: 2 :: 3 :: 4 :: []))) in 0;;
  
  
  Types after conversions:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list

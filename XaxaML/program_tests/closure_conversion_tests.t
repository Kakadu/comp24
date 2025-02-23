  $ ./run_closure_conversion.exe < manytests/typed/001fac.ml
  Types before modifications:
  val fac : int -> int
  val main : int
  Types after modifications:
  val fac : int -> int
  val main : int
  Modified ast:
  let rec fac = (fun n -> if ((<= n) 1)
  then 1
  else ((* n) (fac ((- n) 1))))
  
  let main = let #t = (print_int (fac 4)) in
  if ((= #t) ())
  then 0
  else #match_failure
  $ ./run_closure_conversion.exe < manytests/typed/002fac.ml
  Types before modifications:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  Types after modifications:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  Modified ast:
  let rec fac_cps = (fun n k -> if ((= n) 1)
  then (k 1)
  else ((fac_cps ((- n) 1)) (((fun k n p -> (k ((* p) n))) k) n)))
  
  let main = let #t = (print_int ((fac_cps 4) (fun #0 -> #0))) in
  if ((= #t) ())
  then 0
  else #match_failure

  $ ./run_closure_conversion.exe < manytests/typed/016lists.ml
  Types before modifications:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  Types after modifications:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  Modified ast:
  let rec length = (fun xs -> if ((= xs) [])
  then 0
  else if ((> (#list_length xs)) 0)
  then let h = (#list_hd xs) in
  let tl = (#list_tl xs) in
  ((+ 1) (length tl))
  else #match_failure)
  
  let length_tail = let rec helper = (fun acc xs -> if ((= xs) [])
  then acc
  else if ((> (#list_length xs)) 0)
  then let h = (#list_hd xs) in
  let tl = (#list_tl xs) in
  ((helper ((+ acc) 1)) tl)
  else #match_failure) in
  (helper 0)
  
  let rec map = (fun f xs -> if ((= xs) [])
  then []
  else if ((&& ((> (#list_length xs)) 0)) ((= (#list_tl xs)) []))
  then let a = (#list_hd xs) in
  ((f a)::[])
  else if ((&& ((> (#list_length xs)) 1)) ((= (#list_tl (#list_tl xs))) []))
  then let a = (#list_hd xs) in
  let b = (#list_hd (#list_tl xs)) in
  ((f a)::((f b)::[]))
  else if ((&& ((> (#list_length xs)) 2)) ((= (#list_tl (#list_tl (#list_tl xs)))) []))
  then let a = (#list_hd xs) in
  let b = (#list_hd (#list_tl xs)) in
  let c = (#list_hd (#list_tl (#list_tl xs))) in
  ((f a)::((f b)::((f c)::[])))
  else if ((> (#list_length xs)) 3)
  then let a = (#list_hd xs) in
  let b = (#list_hd (#list_tl xs)) in
  let c = (#list_hd (#list_tl (#list_tl xs))) in
  let d = (#list_hd (#list_tl (#list_tl (#list_tl xs)))) in
  let tl = (#list_tl (#list_tl (#list_tl (#list_tl xs)))) in
  ((f a)::((f b)::((f c)::((f d)::((map f) tl)))))
  else #match_failure)
  
  let rec append = (fun xs ys -> if ((= xs) [])
  then ys
  else if ((> (#list_length xs)) 0)
  then let x = (#list_hd xs) in
  let #0 = (#list_tl xs) in
  (x::((append #0) ys))
  else #match_failure)
  
  let concat = let rec helper = (fun xs -> if ((= xs) [])
  then []
  else if ((> (#list_length xs)) 0)
  then let h = (#list_hd xs) in
  let tl = (#list_tl xs) in
  ((append h) (helper tl))
  else #match_failure) in
  helper
  
  let rec iter = (fun f xs -> if ((= xs) [])
  then ()
  else if ((> (#list_length xs)) 0)
  then let h = (#list_hd xs) in
  let tl = (#list_tl xs) in
  let #t = (f h) in
  if ((= #t) ())
  then ((iter f) tl)
  else #match_failure
  else #match_failure)
  
  let rec cartesian = (fun xs ys -> if ((= xs) [])
  then []
  else if ((> (#list_length xs)) 0)
  then let h = (#list_hd xs) in
  let tl = (#list_tl xs) in
  ((append ((map ((fun h a -> (h, a)) h)) ys)) ((cartesian tl) ys))
  else #match_failure)
  
  let main = let #t = ((iter print_int) (1::(2::(3::[])))) in
  if ((= #t) ())
  then let #1 = (print_int (length ((cartesian (1::(2::[]))) (1::(2::(3::(4::[]))))))) in
  if ((= #1) ())
  then 0
  else #match_failure
  else #match_failure

  $ ./run_closure_conversion.exe << EOF
  > let rec y x =
  >   let a z = y x + z in
  >   let b x = a 5 + x in
  >   b 5
  Types before modifications:
  val y : 'a -> int
  Types after modifications:
  val y : 'a -> int
  Modified ast:
  let rec y = (fun x -> let a = (fun x y z -> ((+ (y x)) z)) in
  let b = (fun a #0 -> ((+ (a 5)) #0)) in
  ((b ((a x) y)) 5))

  $ ./run_closure_conversion.exe << EOF
  > let f a b =
  >   let a x = b + x in
  >   let b x = a 1 + x in
  >   a 1 + b 2;;
  Types before modifications:
  val f : 'a -> int -> int
  Types after modifications:
  val f : 'a -> int -> int
  Modified ast:
  let f = (fun a b -> let #0 = (fun b x -> ((+ b) x)) in
  let #1 = (fun #0 x -> ((+ (#0 1)) x)) in
  ((+ ((#0 b) 1)) ((#1 (#0 b)) 2)))





  $ ./run_lambda_lifting.exe < manytests/typed/001fac.ml
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
  
  let main = let () = (print_int (fac 4)) in
  0

  $ ./run_lambda_lifting.exe < manytests/typed/002fac.ml
  Types before modifications:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  Types after modifications:
  val a1 : (int -> 'a) -> int -> int -> 'a
  val a2 : 'a -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  Modified ast:
  let a1 = (fun k n p -> (k ((* p) n)))
  
  let rec fac_cps = (fun n k -> if ((= n) 1)
  then (k 1)
  else ((fac_cps ((- n) 1)) ((a1 k) n)))
  
  let a2 = (fun a0 -> a0)
  
  let main = let () = (print_int ((fac_cps 4) a2)) in
  0

  $ ./run_lambda_lifting.exe << EOF
  > let f a b = 
  >   let inc x = 
  >     let inc2 y = y + 1 in inc2 x
  >   in
  >   let sum a b = a + b in
  >   inc (sum a b) 
  Types before modifications:
  val f : int -> int -> int
  Types after modifications:
  val a2 : int -> int
  val a3 : int -> int
  val a4 : int -> int -> int
  val f : int -> int -> int
  Modified ast:
  let a2 = (fun y -> ((+ y) 1))
  
  let a3 = (fun x -> (a2 x))
  
  let a4 = (fun a0 a1 -> ((+ a0) a1))
  
  let f = (fun a b -> (a3 ((a4 a) b)))

  $ ./run_lambda_lifting.exe << EOF
  > let f = 
  >   let rec is_even n =
  >   if n = 0 then true
  >   else is_odd (n - 1)
  > 
  >   and is_odd n =
  >   if n = 0 then false
  >   else is_even (n - 1)
  > in (is_even 4, is_odd 5)
  Types before modifications:
  val f : bool * bool
  Types after modifications:
  val a0 : int -> bool
  val a1 : int -> bool
  val f : bool * bool
  Modified ast:
  let rec a0 = (fun n -> if ((= n) 0)
  then false
  else (a1 ((- n) 1)))
  and a1 = (fun n -> if ((= n) 0)
  then true
  else (a0 ((- n) 1)))
  
  let f = ((a1 4), (a0 5))

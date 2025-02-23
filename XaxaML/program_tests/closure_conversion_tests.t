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
  0
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
  0

  $ ./run_closure_conversion.exe << EOF 
  > let f y = let add_2 x = x + 2 in add_2
  Types before modifications:
  val f : 'a -> int -> int
  Types after modifications:
  val f : 'a -> int -> int
  Modified ast:
  let f = (fun y -> let add_2 = (fun x -> ((+ x) 2)) in
  add_2)

  $ ./run_closure_conversion.exe << EOF 
  > let f y = let add_y x = x + y in add_y
  Types before modifications:
  val f : int -> int -> int
  Types after modifications:
  val f : int -> int -> int
  Modified ast:
  let f = (fun y -> let add_y = (fun y x -> ((+ x) y)) in
  (add_y y))

  $ ./run_closure_conversion.exe << EOF 
  > let t =
  >   let x = 1 in
  >   let y = 2 in
  >   let f a =
  >     let g b = a + b + x + y in
  >     g (a * 2)
  >   in
  >   f 10
  Types before modifications:
  val t : int
  Types after modifications:
  val t : int
  Modified ast:
  let t = let x = 1 in
  let y = 2 in
  let f = (fun x y a -> let g = (fun a x y b -> ((+ ((+ ((+ a) b)) x)) y)) in
  ((((g a) x) y) ((* a) 2))) in
  (((f x) y) 10)

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

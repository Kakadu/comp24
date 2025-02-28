  $ ./run_to_anf.exe < manytests/typed/001fac.ml
  Types before modifications:
  val fac : int -> int
  val main : int
  
  Types after modifications:
  val fac : int -> int
  val main : int
  
  Modified ast:
  let rec fac n =
    let #0 = (<=) n 1 in
    if #0
    then 1
    else let #2 = (-) n 1 in
    let #1 = fac #2 in
    (*) n #1
    
  let main =
    let #3 = fac 4 in
    let #t = print_int #3 in
    0

  $ ./run_to_anf.exe < manytests/typed/002fac.ml
  Types before modifications:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Types after modifications:
  val #1 : (int -> 'a) -> int -> int -> 'a
  val #2 : 'a -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Modified ast:
  let #1 k n p =
    let #3 = (*) p n in
    k #3
  let rec fac_cps n k =
    let #4 = (=) n 1 in
    if #4
    then k 1
    else let #5 = (-) n 1 in
    let #6 = #1 k n in
    fac_cps #5 #6
    
  let #2 #0 =
    #0
  let main =
    let #7 = fac_cps 4 #2 in
    let #t = print_int #7 in
    0
# simple
  $ ./run_to_anf.exe << EOF
  > let sum a b = a + b
  > let inc x = sum x 1
  > let t = sum (sum 1 2) (inc 2)
  Types before modifications:
  val inc : int -> int
  val sum : int -> int -> int
  val t : int
  
  Types after modifications:
  val inc : int -> int
  val sum : int -> int -> int
  val t : int
  
  Modified ast:
  let sum a b =
    (+) a b
  let inc x =
    sum x 1
  let t =
    let #0 = sum 1 2 in
    let #1 = inc 2 in
    sum #0 #1
  
  $ ./run_to_anf.exe << EOF
  > let f a b = 
  >   let inc x = 
  >     let inc2 y = y + 1 in inc2 x
  >   in
  >   let sum a b = a + b in
  >   inc (sum a b) 
  Types before modifications:
  val f : int -> int -> int
  
  Types after modifications:
  val #2 : int -> int
  val #3 : int -> int
  val #4 : int -> int -> int
  val f : int -> int -> int
  
  Modified ast:
  let #2 y =
    (+) y 1
  let #3 x =
    #2 x
  let #4 #0 #1 =
    (+) #0 #1
  let f a b =
    let #5 = #4 a b in
    #3 #5

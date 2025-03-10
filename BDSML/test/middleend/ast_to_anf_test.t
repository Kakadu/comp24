Simple test
  $ ./run_to_anf.exe <<- EOF
  > let a = 1 + 2 + 3;;
  Types before middleend:
  val a : int
  
  Types after anf:
  val __var_a : int
  
  let __var_a = (let __anf_0 = ((( + ) 2) 1) in 
   ((( + ) 3) __anf_0));;
Test fun
  $ ./run_to_anf.exe <<- EOF
  > let c = 4;;
  > let f a = fun b -> a + b + c;;
  Types before middleend:
  val c : int
  val f : int -> int -> int
  
  Types after anf:
  val __var_c : int
  val lifted_1 : int -> int -> int
  val lifted_0 : int -> int -> int
  val __var_f : int -> int -> int
  
  let __var_c = 4;;
  let lifted_1 __var_a0 __reserved_1 = (let __var_b = __reserved_1 in 
   (let __anf_0 = ((( + ) __var_b) __var_a0) in 
   ((( + ) __var_c) __anf_0)));;
  let lifted_0 __reserved_0 = (let __var_a = __reserved_0 in 
   (lifted_1 __var_a));;
  let __var_f = lifted_0;;

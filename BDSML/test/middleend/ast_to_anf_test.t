Simple test
  $ ./run_to_anf.exe <<- EOF
  > let a = 1 + 2 + 3;;
  Types before middleend:
  val a : int
  
  Types after anf:
  val __var_a : int
  
  let __var_a = (let __anf_0 = ((( + ) 1) 2) in 
   ((( + ) __anf_0) 3));;
Test fun
  $ ./run_to_anf.exe <<- EOF
  > let c = 4;;
  > let f a = fun b -> a + b + c;;
  Types before middleend:
  val c : int
  val f : int -> int -> int
  
  Types after anf:
  val __var_c : int
  val lifted_0 : int -> int -> int
  val __var_f : int -> int -> int
  
  let __var_c = 4;;
  let lifted_0 __var_a0 __reserved_1 = (let __anf_0 = ((( + ) __var_a0) __reserved_1) in 
   ((( + ) __anf_0) __var_c));;
  let __var_f __reserved_0 = (lifted_0 __reserved_0);;

Simple test
  $ ./run_to_anf.exe <<- EOF
  > let a = 1 + 2 + 3;;
  let __var_a = (let __anf_0 = ((__op_plus 2) 1) in 
   ((__op_plus 3) __anf_0));;
Test fun
  $ ./run_to_anf.exe <<- EOF
  > let c = 4;;
  > let f a = fun b -> a + b + c;;
  let __var_c = 4;;
  let lifted_1 __var_a0 __reserved_1 = (let __anf_0 = ((__op_plus __reserved_1) __var_a0) in 
   ((__op_plus __var_c) __anf_0));;
  let lifted_0 __reserved_0 = (lifted_1 __reserved_0);;

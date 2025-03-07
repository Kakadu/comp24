Test simple fun
  $ ./run_closure_conversion.exe <<- EOF
  > let a = 4;;
  > fun b -> a
  let __var_a = 4;;
  ((fun __var_a0 __reserved_0 -> (let __var_b = __reserved_0 in 
   __var_a0)) __var_a);;
Test fun in fun
  $ ./run_closure_conversion.exe <<- EOF
  > let a = 4;;
  > fun b -> fun c -> a + c + b
  let __var_a = 4;;
  ((fun __var_a0 __reserved_0 -> (let __var_b = __reserved_0 in 
   (((fun __var_a1 __var_b2 __reserved_1 -> (let __var_c = __reserved_1 in 
   ((__op_plus (((__op_plus __var_a1) __var_c))) __var_b2))) __var_a0) __var_b))) __var_a);;
Test fun with let
  $ ./run_closure_conversion.exe <<- EOF
  > let a = 4;;
  > fun b -> let a = 4 in a + b;;
  > fun b -> let a = a in a + b
  let __var_a = 4;;
  (fun __reserved_0 -> (let __var_b = __reserved_0 in 
   (let __var_a0 = 4 in 
   ((__op_plus __var_a0) __var_b))));;
  ((fun __var_a0 __reserved_1 -> (let __var_b = __reserved_1 in 
   (let __var_a1 = __var_a0 in 
   ((__op_plus __var_a1) __var_b)))) __var_a);;
Test let rec
  $ ./run_closure_conversion.exe <<- EOF
  > let a = 4;;
  > fun a -> let rec b = fun c -> c + n + b 1
  > and n = a in b;;
  let __var_a = 4;;
  (fun __reserved_0 -> (let __var_a0 = __reserved_0 in 
   (let rec __var_b = (((fun __var_b0 __var_n1 __reserved_1 -> (let __var_c = __reserved_1 in 
   ((__op_plus (((__op_plus __var_c) __var_n1))) ((__var_b0 1))))) __var_b) __var_n)
   and __var_n = __var_a0 in 
   __var_b)));;

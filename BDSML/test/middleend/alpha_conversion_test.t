Test just var in
  $ ./run_alpha_conversion.exe <<- EOF
  > let v = 4 in let v = true in v
  > EOF
  (let __var_v = 4 in 
   (let __var_v0 = true in 
   __var_v0));;
Test shadow previous var
  $ ./run_alpha_conversion.exe <<- EOF
  > let a = 3;;
  > let v = 4 in let a = true in a
  > EOF
  let __var_a = 3;;
  (let __var_v = 4 in 
   (let __var_a0 = true in 
   __var_a0));;
Test fun shadow
  $ ./run_alpha_conversion.exe <<- EOF
  > let a = 3;;
  > let v = fun a -> a;;
  > EOF
  let __var_a = 3;;
  let __var_v = (fun __reserved_0 -> (let __var_a0 = __reserved_0 in 
   __var_a0));;
Test let rec
  $ ./run_alpha_conversion.exe <<- EOF
  > let a = 3;;
  > let v = let rec a = a and b = a in a;;
  > EOF
  let __var_a = 3;;
  let __var_v = (let rec __var_a0 = __var_a0
   and __var_b = __var_a0 in 
   __var_a0);;
Test operator shadowing
  $ ./run_alpha_conversion.exe <<- EOF
  > let v = let ( + ) = ( - ) in 1 + 2;;
  > EOF
  let __var_v = (let __op_plus0 = __op_minus in 
   ((__op_plus0 1) 2));;
Test redefine
  $ ./run_alpha_conversion.exe <<- EOF
  > let a = 1;;
  > let rec a = 2;;
  > let a = true;;
  > EOF
  let __var_a = 1;;
  let rec __var_a = 2;;
  let __var_a = true;;
Test hard
  $ ./run_alpha_conversion.exe <<- EOF
  > let a, b = 1, 3;;
  > let rec v = let b = 3 in if true then None, 3 else (Some (let a = 2 in a)), b;; 
  > EOF
  let __reserved_0 = (1, 3);;
  let __var_a = ((__get_from_tuple __reserved_0) 0);;
  let __var_b = ((__get_from_tuple __reserved_0) 1);;
  let rec __var_v = (let __var_b0 = 3 in 
   (if true then ((None), 3) else ((Some (let __var_a1 = 2 in 
   __var_a1)), __var_b0)));;

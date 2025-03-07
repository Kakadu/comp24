  $ ./run_lambda_lifting.exe <<- EOF
  > let rec fac n = if n<=1 then 1 else n * fac (n-1)
  let lifted_0 = (fun __reserved_0 -> (let __var_n = __reserved_0 in 
   (if ((( <= ) __var_n) 1) then 1 else ((( * ) __var_n) ((__var_fac (((( - ) __var_n) 1))))))));;
  let rec __var_fac = lifted_0;;

  $ ./run_lambda_lifting.exe <<- EOF
  > let rec is_even n = if n = 0 then true else is_odd (n - 1)
  > and is_odd n = if n = 0 then false else is_even (n - 1)
  let lifted_0 = (fun __reserved_0 -> (let __var_n = __reserved_0 in 
   (if ((( = ) __var_n) 0) then true else (__var_is_odd (((( - ) __var_n) 1))))));;
  let lifted_1 = (fun __reserved_1 -> (let __var_n = __reserved_1 in 
   (if ((( = ) __var_n) 0) then false else (__var_is_even (((( - ) __var_n) 1))))));;
  let rec __var_is_even = lifted_0
   and __var_is_odd = lifted_1;;

  $ ./run_lambda_lifting.exe <<- EOF
  > let f = fun a -> a
  let lifted_0 = (fun __reserved_0 -> (let __var_a = __reserved_0 in 
   __var_a));;
  let __var_f = lifted_0;;

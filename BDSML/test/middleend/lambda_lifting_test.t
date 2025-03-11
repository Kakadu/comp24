  $ ./run_lambda_lifting.exe <<- EOF
  > let rec fac n = if n<=1 then 1 else n * fac (n-1)
  let rec __var_fac = (fun __reserved_0 -> (let __var_n = __reserved_0 in 
   (if ((__op_le __var_n) 1) then 1 else ((__op_mult __var_n) ((__var_fac (((__op_minus __var_n) 1))))))));;

  $ ./run_lambda_lifting.exe <<- EOF
  > let rec is_even n = if n = 0 then true else is_odd (n - 1)
  > and is_odd n = if n = 0 then false else is_even (n - 1)
  let rec __var_is_even = (fun __reserved_0 -> (let __var_n = __reserved_0 in 
   (if ((__op_eq __var_n) 0) then true else (__var_is_odd (((__op_minus __var_n) 1))))))
   and __var_is_odd = (fun __reserved_1 -> (let __var_n = __reserved_1 in 
   (if ((__op_eq __var_n) 0) then false else (__var_is_even (((__op_minus __var_n) 1))))));;

  $ ./run_lambda_lifting.exe <<- EOF
  > let f = fun a -> a
  let __var_f = (fun __reserved_0 -> (let __var_a = __reserved_0 in 
   __var_a));;

  $ ./run_lambda_lifting.exe <<- EOF
  > let f x = let y = x + 1 in fun z -> y + z
  let lifted_0 = (fun __var_y0 __reserved_1 -> (let __var_z = __reserved_1 in 
   ((__op_plus __var_y0) __var_z)));;
  let __var_f = (fun __reserved_0 -> (let __var_x = __reserved_0 in 
   (let __var_y = ((__op_plus __var_x) 1) in 
   (lifted_0 __var_y))));;

  $ ./run_lambda_lifting.exe <<- EOF
  > let a = 4;;
  > let b = 5;;
  > let f b = a + b;;
  let __var_a = 4;;
  let __var_b = 5;;
  let __var_f = (fun __reserved_0 -> (let __var_b0 = __reserved_0 in 
   ((__op_plus __var_a) __var_b0)));;
Test let with let rec in
  $ ./run_lambda_lifting.exe <<- EOF
  > let length_tail =
  > let rec helper acc xs =
  > match xs with
  > | [] -> acc
  > | h::tl -> helper (acc + 1) tl
  > in
  > helper 0
  let rec __var_helper = (fun __reserved_0 __reserved_1 -> (let __var_acc = __reserved_0 in 
   (let __var_xs = __reserved_1 in 
   (let __reserved_2 = __var_xs in 
   (if ((__same_cons __reserved_2) "[]") then (let __nothing = ((__same_cons __reserved_2) (([]))) in 
   __var_acc) else (if ((__op_and (((__same_cons __reserved_2) "::"))) (((__op_and (((__op_and true) true))) true))) then (let __reserved_3 = ((__disassemble "::") __reserved_2) in 
   (let __var_h = ((__get_from_tuple __reserved_3) 0) in 
   (let __var_tl = ((__get_from_tuple __reserved_3) 1) in 
   ((__var_helper (((__op_plus __var_acc) 1))) __var_tl)))) else (__exception "Match_failure")))))));;
  let __var_length_tail = (__var_helper 0);;
Test let with let rec in with several capture
  $ ./run_lambda_lifting.exe <<- EOF
  > let f a =
  > let rec helper acc xs =
  > match xs with
  > | [] -> acc
  > | h::tl -> helper (acc + a) tl
  > in
  > helper 0;;
  let rec __var_helper = (fun __var_a0 __reserved_1 __reserved_2 -> (let __var_acc = __reserved_1 in 
   (let __var_xs = __reserved_2 in 
   (let __reserved_3 = __var_xs in 
   (if ((__same_cons __reserved_3) "[]") then (let __nothing = ((__same_cons __reserved_3) (([]))) in 
   __var_acc) else (if ((__op_and (((__same_cons __reserved_3) "::"))) (((__op_and (((__op_and true) true))) true))) then (let __reserved_4 = ((__disassemble "::") __reserved_3) in 
   (let __var_h = ((__get_from_tuple __reserved_4) 0) in 
   (let __var_tl = ((__get_from_tuple __reserved_4) 1) in 
   (((__var_helper __var_a0) (((__op_plus __var_acc) __var_a0))) __var_tl)))) else (__exception "Match_failure")))))));;
  let __var_f = (fun __reserved_0 -> (let __var_a = __reserved_0 in 
   ((__var_helper __var_a) 0)));;

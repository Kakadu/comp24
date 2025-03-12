Test simple fun
  $ ./run_closure_conversion.exe <<- EOF
  > let a = 4;;
  > fun b -> a
  let __var_a = 4;;
  (fun __reserved_0 -> (let __var_b = __reserved_0 in 
   __var_a));;
Test fun in fun
  $ ./run_closure_conversion.exe <<- EOF
  > let a = 4;;
  > fun b -> fun c -> a + c + b
  let __var_a = 4;;
  (fun __reserved_0 -> (let __var_b = __reserved_0 in 
   ((fun __var_b0 __reserved_1 -> (let __var_c = __reserved_1 in 
   ((op_plus (((op_plus __var_a) __var_c))) __var_b0))) __var_b)));;
Test fun with let
  $ ./run_closure_conversion.exe <<- EOF
  > let a = 4;;
  > fun b -> let a = 4 in a + b;;
  > fun b -> let a = a in a + b
  let __var_a = 4;;
  (fun __reserved_0 -> (let __var_b = __reserved_0 in 
   (let __var_a0 = 4 in 
   ((op_plus __var_a0) __var_b))));;
  (fun __reserved_1 -> (let __var_b = __reserved_1 in 
   (let __var_a1 = __var_a in 
   ((op_plus __var_a1) __var_b))));;
Test let rec
  $ ./run_closure_conversion.exe <<- EOF
  > let a = 4;;
  > fun a -> let rec b = fun c -> c + n + b 1
  > and n = a in b;;
  let __var_a = 4;;
  (fun __reserved_0 -> (let __var_a0 = __reserved_0 in 
   (let rec __var_b = (fun __var_n0 __reserved_1 -> (let __var_c = __reserved_1 in 
   ((op_plus (((op_plus __var_c) ((__var_n0 __var_a0))))) (((__var_b __var_n0) 1)))))
   and __var_n = __var_a0 in 
   (__var_b __var_n))));;
Test construct
  $ ./run_closure_conversion.exe <<- EOF
  > let a = 4;;
  > let b = 5;;
  > let rec f = function | h :: b -> h + f b | [] -> a;;
  let __var_a = 4;;
  let __var_b = 5;;
  let rec __var_f = (fun __reserved_0 -> (if ((op_and (((same_cons __reserved_0) "::"))) (((op_and (((op_and true) true))) true))) then (let __reserved_1 = (disassemble __reserved_0) in 
   (let __var_h = ((get_from_tuple __reserved_1) 0) in 
   (let __var_b0 = ((get_from_tuple __reserved_1) 1) in 
   ((op_plus __var_h) ((__var_f __var_b0)))))) else (if ((same_cons __reserved_0) "[]") then (let __nothing = ((same_cons __reserved_0) 3) in 
   __var_a) else (exception "Match_failure"))));;
Test let with let rec in
  $ ./run_closure_conversion.exe <<- EOF
  > let length_tail =
  > let rec helper acc xs =
  > match xs with
  > | [] -> acc
  > | h::tl -> helper (acc + 1) tl
  > in
  > helper 0
  let __var_length_tail = (let rec __var_helper = (fun __reserved_0 __reserved_1 -> (let __var_acc = __reserved_0 in 
   (let __var_xs = __reserved_1 in 
   (let __reserved_2 = __var_xs in 
   (if ((same_cons __reserved_2) "[]") then (let __nothing = ((same_cons __reserved_2) 3) in 
   __var_acc) else (if ((op_and (((same_cons __reserved_2) "::"))) (((op_and (((op_and true) true))) true))) then (let __reserved_3 = (disassemble __reserved_2) in 
   (let __var_h = ((get_from_tuple __reserved_3) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_3) 1) in 
   ((__var_helper (((op_plus __var_acc) 1))) __var_tl)))) else (exception "Match_failure"))))))) in 
   (__var_helper 0));;
Test let with let rec in with several capture
  $ ./run_closure_conversion.exe <<- EOF
  > let f a =
  > let rec helper acc xs =
  > match xs with
  > | [] -> acc
  > | h::tl -> helper (acc + a) tl
  > in
  > helper 0;;
  let __var_f = (fun __reserved_0 -> (let __var_a = __reserved_0 in 
   (let rec __var_helper = (fun __var_a0 __reserved_1 __reserved_2 -> (let __var_acc = __reserved_1 in 
   (let __var_xs = __reserved_2 in 
   (let __reserved_3 = __var_xs in 
   (if ((same_cons __reserved_3) "[]") then (let __nothing = ((same_cons __reserved_3) 3) in 
   __var_acc) else (if ((op_and (((same_cons __reserved_3) "::"))) (((op_and (((op_and true) true))) true))) then (let __reserved_4 = (disassemble __reserved_3) in 
   (let __var_h = ((get_from_tuple __reserved_4) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_4) 1) in 
   (((__var_helper __var_a0) (((op_plus __var_acc) __var_a0))) __var_tl)))) else (exception "Match_failure"))))))) in 
   ((__var_helper __var_a) 0))));;

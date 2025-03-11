  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/001fac.ml
  let rec __var_fac = (fun __reserved_0 -> (let __var_n = __reserved_0 in 
   (if ((op_le __var_n) 1) then 1 else ((op_mult __var_n) ((__var_fac (((op_minus __var_n) 1))))))));;
  let __var_main = (let __nothing = ((op_eq ((print_int ((__var_fac 4))))) ()) in 
   0);;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/002fac.ml
  let lifted_1 = (fun __var_k0 __var_n1 __reserved_2 -> (let __var_p = __reserved_2 in 
   (__var_k0 (((op_mult __var_p) __var_n1)))));;
  let rec __var_fac_cps = (fun __reserved_0 __reserved_1 -> (let __var_n = __reserved_0 in 
   (let __var_k = __reserved_1 in 
   (if ((op_eq __var_n) 1) then (__var_k 1) else ((__var_fac_cps (((op_minus __var_n) 1))) (((lifted_1 __var_k) __var_n)))))));;
  let lifted_0 = (fun __reserved_3 -> (let print_int0 = __reserved_3 in 
   print_int0));;
  let __var_main = (let __nothing = ((op_eq ((print_int (((__var_fac_cps 4) lifted_0))))) ()) in 
   0);;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/003fib.ml
  let rec __var_fib_acc = (fun __reserved_0 __reserved_1 __reserved_2 -> (let __var_a = __reserved_0 in 
   (let __var_b = __reserved_1 in 
   (let __var_n = __reserved_2 in 
   (if ((op_eq __var_n) 1) then __var_b else (let __var_n1 = ((op_minus __var_n) 1) in 
   (let __var_ab = ((op_plus __var_a) __var_b) in 
   (((__var_fib_acc __var_b) __var_ab) __var_n1))))))));;
  let rec __var_fib = (fun __reserved_3 -> (let __var_n = __reserved_3 in 
   (if ((op_lt __var_n) 2) then __var_n else ((op_plus ((__var_fib (((op_minus __var_n) 1))))) ((__var_fib (((op_minus __var_n) 2))))))));;
  let __var_main = (let __nothing = ((op_eq ((print_int ((((__var_fib_acc 0) 1) 4))))) ()) in 
   (let __nothing0 = ((op_eq ((print_int ((__var_fib 4))))) ()) in 
   0));;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/004manyargs.ml
  let __var_wrap = (fun __reserved_0 -> (let __var_f = __reserved_0 in 
   (if ((op_eq 1) 1) then __var_f else __var_f)));;
  let __var_test3 = (fun __reserved_1 __reserved_2 __reserved_3 -> (let __var_a = __reserved_1 in 
   (let __var_b = __reserved_2 in 
   (let __var_c = __reserved_3 in 
   (let __var_a0 = (print_int __var_a) in 
   (let __var_b1 = (print_int __var_b) in 
   (let __var_c2 = (print_int __var_c) in 
   0)))))));;
  let __var_test10 = (fun __reserved_4 __reserved_5 __reserved_6 __reserved_7 __reserved_8 __reserved_9 __reserved_10 __reserved_11 __reserved_12 __reserved_13 -> (let __var_a = __reserved_4 in 
   (let __var_b = __reserved_5 in 
   (let __var_c = __reserved_6 in 
   (let __var_d = __reserved_7 in 
   (let __var_e = __reserved_8 in 
   (let __var_f = __reserved_9 in 
   (let __var_g = __reserved_10 in 
   (let __var_h = __reserved_11 in 
   (let __var_i = __reserved_12 in 
   (let __var_j = __reserved_13 in 
   ((op_plus (((op_plus (((op_plus (((op_plus (((op_plus (((op_plus (((op_plus (((op_plus (((op_plus __var_a) __var_b))) __var_c))) __var_d))) __var_e))) __var_f))) __var_g))) __var_h))) __var_i))) __var_j))))))))))));;
  let __var_main = (let __var_rez = (((((((((((__var_wrap __var_test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in 
   (let __nothing = ((op_eq ((print_int __var_rez))) ()) in 
   (let __var_temp2 = ((((__var_wrap __var_test3) 1) 10) 100) in 
   0)));;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/005fix.ml
  let rec __var_fix = (fun __reserved_0 __reserved_1 -> (let __var_f = __reserved_0 in 
   (let __var_x = __reserved_1 in 
   ((__var_f ((__var_fix __var_f))) __var_x))));;
  let __var_fac = (fun __reserved_2 __reserved_3 -> (let __var_self = __reserved_2 in 
   (let __var_n = __reserved_3 in 
   (if ((op_le __var_n) 1) then 1 else ((op_mult __var_n) ((__var_self (((op_minus __var_n) 1)))))))));;
  let __var_main = (let __nothing = ((op_eq ((print_int (((__var_fix __var_fac) 6))))) ()) in 
   0);;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/006partial.ml
  let lifted_0 = (fun __reserved_1 -> (let __var_foo = __reserved_1 in 
   ((op_plus __var_foo) 2)));;
  let lifted_1 = (fun __reserved_2 -> (let __var_foo = __reserved_2 in 
   ((op_mult __var_foo) 10)));;
  let __var_foo = (fun __reserved_0 -> (let __var_b = __reserved_0 in 
   (if __var_b then lifted_0 else lifted_1)));;
  let __var_foo0 = (fun __reserved_3 -> (let __var_x = __reserved_3 in 
   ((__var_foo true) (((__var_foo false) (((__var_foo true) (((__var_foo false) __var_x)))))))));;
  let __var_main = (let __nothing = ((op_eq ((print_int ((__var_foo0 11))))) ()) in 
   0);;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/006partial2.ml
  let __var_foo = (fun __reserved_0 __reserved_1 __reserved_2 -> (let __var_a = __reserved_0 in 
   (let __var_b = __reserved_1 in 
   (let __var_c = __reserved_2 in 
   (let __nothing = ((op_eq ((print_int __var_a))) ()) in 
   (let __nothing0 = ((op_eq ((print_int __var_b))) ()) in 
   (let __nothing1 = ((op_eq ((print_int __var_c))) ()) in 
   ((op_plus __var_a) (((op_mult __var_b) __var_c))))))))));;
  let __var_main = (let __var_foo2 = (__var_foo 1) in 
   (let __var_foo3 = (__var_foo2 2) in 
   (let __var_foo4 = (__var_foo3 3) in 
   (let __nothing = ((op_eq ((print_int __var_foo4))) ()) in 
   0))));;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/006partial3.ml
  let lifted_1 = (fun __reserved_2 -> (let __var_c = __reserved_2 in 
   (print_int __var_c)));;
  let lifted_0 = (fun __reserved_1 -> (let __var_b = __reserved_1 in 
   (let __nothing0 = ((op_eq ((print_int __var_b))) ()) in 
   lifted_1)));;
  let __var_foo = (fun __reserved_0 -> (let __var_a = __reserved_0 in 
   (let __nothing = ((op_eq ((print_int __var_a))) ()) in 
   lifted_0)));;
  let __var_main = (let __nothing = ((op_eq ((((__var_foo 4) 8) 9))) ()) in 
   0);;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/007order.ml
  let __var__start = (fun __reserved_0 __reserved_1 __reserved_2 __reserved_3 __reserved_4 __reserved_5 __reserved_6 __reserved_7 __reserved_8 -> (let __nothing = ((op_eq __reserved_0) ()) in 
   (let __nothing0 = ((op_eq __reserved_1) ()) in 
   (let __var_a = __reserved_2 in 
   (let __nothing1 = ((op_eq __reserved_3) ()) in 
   (let __var_b = __reserved_4 in 
   (let __var__c = __reserved_5 in 
   (let __nothing2 = ((op_eq __reserved_6) ()) in 
   (let __var_d = __reserved_7 in 
   (let __var___ = __reserved_8 in 
   (let __nothing3 = ((op_eq ((print_int (((op_plus __var_a) __var_b))))) ()) in 
   (let __nothing4 = ((op_eq ((print_int __var___))) ()) in 
   ((op_plus (((op_div (((op_mult __var_a) __var_b))) __var__c))) __var_d)))))))))))));;
  let __var_main = (print_int ((((((((((__var__start ((print_int 1))) ((print_int 2))) 3) ((print_int 4))) 100) 1000) ((print_int ((op_neg 1))))) 10000) ((op_neg 555555)))));;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/008ascription.ml
  let __var_addi = (fun __reserved_0 __reserved_1 __reserved_2 -> (let __var_f = __reserved_0 in 
   (let __var_g = __reserved_1 in 
   (let __var_x = __reserved_2 in 
   ((__var_f __var_x) ((__var_g __var_x)))))));;
  let lifted_0 = (fun __reserved_3 __reserved_4 -> (let __var_x = __reserved_3 in 
   (let __var_b = __reserved_4 in 
   (if __var_b then ((op_plus __var_x) 1) else ((op_mult __var_x) 2)))));;
  let lifted_1 = (fun __reserved_5 -> (let __var__start = __reserved_5 in 
   ((op_eq (((op_div __var__start) 2))) 0)));;
  let __var_main = (let __nothing = ((op_eq ((print_int ((((__var_addi lifted_0) lifted_1) 4))))) ()) in 
   0);;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/009let_poly.ml
  let lifted_0 = (fun __reserved_0 -> (let __var_x = __reserved_0 in 
   __var_x));;
  let __var_temp = (let __var_f = lifted_0 in 
   ((__var_f 1), (__var_f true)));;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/015tuples.ml
  let rec __var_fix = (fun __reserved_0 __reserved_1 -> (let __var_f = __reserved_0 in 
   (let __var_x = __reserved_1 in 
   ((__var_f ((__var_fix __var_f))) __var_x))));;
  let __var_map = (fun __reserved_2 __reserved_3 -> (let __var_f = __reserved_2 in 
   (let __var_p = __reserved_3 in 
   (let __reserved_4 = __var_p in 
   (let __var_a = ((get_from_tuple __reserved_4) 0) in 
   (let __var_b = ((get_from_tuple __reserved_4) 1) in 
   ((__var_f __var_a), (__var_f __var_b))))))));;
  let lifted_1 = (fun __var_l00 __var_self1 __reserved_8 __reserved_9 -> (let __var_li = __reserved_8 in 
   (let __var_x = __reserved_9 in 
   ((__var_li ((__var_self1 __var_l00))) __var_x))));;
  let lifted_0 = (fun __reserved_6 __reserved_7 -> (let __var_self = __reserved_6 in 
   (let __var_l0 = __reserved_7 in 
   ((__var_map (((lifted_1 __var_l0) __var_self))) __var_l0))));;
  let __var_fixpoly = (fun __reserved_5 -> (let __var_l = __reserved_5 in 
   ((__var_fix lifted_0) __var_l)));;
  let __var_feven = (fun __reserved_10 __reserved_11 -> (let __var_p = __reserved_10 in 
   (let __var_n = __reserved_11 in 
   (let __reserved_12 = __var_p in 
   (let __var_e = ((get_from_tuple __reserved_12) 0) in 
   (let __var_o = ((get_from_tuple __reserved_12) 1) in 
   (if ((op_phys_eq __var_n) 0) then 1 else (__var_o (((op_minus __var_n) 1))))))))));;
  let __var_fodd = (fun __reserved_13 __reserved_14 -> (let __var_p = __reserved_13 in 
   (let __var_n = __reserved_14 in 
   (let __reserved_15 = __var_p in 
   (let __var_e = ((get_from_tuple __reserved_15) 0) in 
   (let __var_o = ((get_from_tuple __reserved_15) 1) in 
   (if ((op_phys_eq __var_n) 0) then 0 else (__var_e (((op_minus __var_n) 1))))))))));;
  let __var_tie = (__var_fixpoly ((__var_feven, __var_fodd)));;
  let rec __var_meven = (fun __reserved_16 -> (let __var_n = __reserved_16 in 
   (if ((op_eq __var_n) 0) then 1 else (__var_modd (((op_minus __var_n) 1))))))
   and __var_modd = (fun __reserved_17 -> (let __var_n = __reserved_17 in 
   (if ((op_eq __var_n) 0) then 1 else (__var_meven (((op_minus __var_n) 1))))));;
  let __var_main = (let __nothing = ((op_eq ((print_int ((__var_modd 1))))) ()) in 
   (let __nothing1 = ((op_eq ((print_int ((__var_meven 2))))) ()) in 
   (let __reserved_18 = __var_tie in 
   (let __var_even = ((get_from_tuple __reserved_18) 0) in 
   (let __var_odd = ((get_from_tuple __reserved_18) 1) in 
   (let __nothing2 = ((op_eq ((print_int ((__var_odd 3))))) ()) in 
   (let __nothing3 = ((op_eq ((print_int ((__var_even 4))))) ()) in 
   0)))))));;
  $ ../middleend/run_lambda_lifting.exe run_to_anf < manytests_link/typed/016lists.ml
  let rec __var_length = (fun __reserved_0 -> (let __var_xs = __reserved_0 in 
   (let __reserved_1 = __var_xs in 
   (if ((same_cons __reserved_1) "[]") then (let __nothing = ((same_cons __reserved_1) (([]))) in 
   0) else (if ((op_and (((same_cons __reserved_1) "::"))) (((op_and (((op_and true) true))) true))) then (let __reserved_2 = ((disassemble "::") __reserved_1) in 
   (let __var_h = ((get_from_tuple __reserved_2) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_2) 1) in 
   ((op_plus 1) ((__var_length __var_tl)))))) else (exception "Match_failure"))))));;
  let rec __var_helper = (fun __reserved_3 __reserved_4 -> (let __var_acc = __reserved_3 in 
   (let __var_xs = __reserved_4 in 
   (let __reserved_5 = __var_xs in 
   (if ((same_cons __reserved_5) "[]") then (let __nothing = ((same_cons __reserved_5) (([]))) in 
   __var_acc) else (if ((op_and (((same_cons __reserved_5) "::"))) (((op_and (((op_and true) true))) true))) then (let __reserved_6 = ((disassemble "::") __reserved_5) in 
   (let __var_h = ((get_from_tuple __reserved_6) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_6) 1) in 
   ((__var_helper (((op_plus __var_acc) 1))) __var_tl)))) else (exception "Match_failure")))))));;
  let __var_length_tail = (__var_helper 0);;
  let rec __var_map = (fun __reserved_7 __reserved_8 -> (let __var_f = __reserved_7 in 
   (let __var_xs = __reserved_8 in 
   (let __reserved_9 = __var_xs in 
   (if ((same_cons __reserved_9) "[]") then (let __nothing = ((same_cons __reserved_9) (([]))) in 
   ([])) else (if ((op_and (((same_cons __reserved_9) "::"))) (((op_and (((op_and true) true))) (((same_cons (((get_from_tuple (((get_cons_param "::") __reserved_9))) 1))) "[]"))))) then (let __reserved_19 = ((disassemble "::") __reserved_9) in 
   (let __var_a = ((get_from_tuple __reserved_19) 0) in 
   (let __nothing = ((same_cons (((get_from_tuple __reserved_19) 1))) (([]))) in 
   ((__var_f __var_a) :: ([]))))) else (if ((op_and (((same_cons __reserved_9) "::"))) (((op_and (((op_and true) true))) (((op_and (((same_cons (((get_from_tuple (((get_cons_param "::") __reserved_9))) 1))) "::"))) (((op_and (((op_and true) true))) (((same_cons (((get_from_tuple (((get_cons_param "::") (((get_from_tuple (((get_cons_param "::") __reserved_9))) 1))))) 1))) "[]"))))))))) then (let __reserved_17 = ((disassemble "::") __reserved_9) in 
   (let __var_a = ((get_from_tuple __reserved_17) 0) in 
   (let __reserved_18 = ((disassemble "::") (((get_from_tuple __reserved_17) 1))) in 
   (let __var_b = ((get_from_tuple __reserved_18) 0) in 
   (let __nothing = ((same_cons (((get_from_tuple __reserved_18) 1))) (([]))) in 
   ((__var_f __var_a) :: ((__var_f __var_b) :: ([])))))))) else (if ((op_and (((same_cons __reserved_9) "::"))) (((op_and (((op_and true) true))) (((op_and (((same_cons (((get_from_tuple (((get_cons_param "::") __reserved_9))) 1))) "::"))) (((op_and (((op_and true) true))) (((op_and (((same_cons (((get_from_tuple (((get_cons_param "::") (((get_from_tuple (((get_cons_param "::") __reserved_9))) 1))))) 1))) "::"))) (((op_and (((op_and true) true))) (((same_cons (((get_from_tuple (((get_cons_param "::") (((get_from_tuple (((get_cons_param "::") (((get_from_tuple (((get_cons_param "::") __reserved_9))) 1))))) 1))))) 1))) "[]"))))))))))))) then (let __reserved_14 = ((disassemble "::") __reserved_9) in 
   (let __var_a = ((get_from_tuple __reserved_14) 0) in 
   (let __reserved_15 = ((disassemble "::") (((get_from_tuple __reserved_14) 1))) in 
   (let __var_b = ((get_from_tuple __reserved_15) 0) in 
   (let __reserved_16 = ((disassemble "::") (((get_from_tuple __reserved_15) 1))) in 
   (let __var_c = ((get_from_tuple __reserved_16) 0) in 
   (let __nothing = ((same_cons (((get_from_tuple __reserved_16) 1))) (([]))) in 
   ((__var_f __var_a) :: ((__var_f __var_b) :: ((__var_f __var_c) :: ([]))))))))))) else (if ((op_and (((same_cons __reserved_9) "::"))) (((op_and (((op_and true) true))) (((op_and (((same_cons (((get_from_tuple (((get_cons_param "::") __reserved_9))) 1))) "::"))) (((op_and (((op_and true) true))) (((op_and (((same_cons (((get_from_tuple (((get_cons_param "::") (((get_from_tuple (((get_cons_param "::") __reserved_9))) 1))))) 1))) "::"))) (((op_and (((op_and true) true))) (((op_and (((same_cons (((get_from_tuple (((get_cons_param "::") (((get_from_tuple (((get_cons_param "::") (((get_from_tuple (((get_cons_param "::") __reserved_9))) 1))))) 1))))) 1))) "::"))) (((op_and (((op_and true) true))) true))))))))))))))) then (let __reserved_10 = ((disassemble "::") __reserved_9) in 
   (let __var_a = ((get_from_tuple __reserved_10) 0) in 
   (let __reserved_11 = ((disassemble "::") (((get_from_tuple __reserved_10) 1))) in 
   (let __var_b = ((get_from_tuple __reserved_11) 0) in 
   (let __reserved_12 = ((disassemble "::") (((get_from_tuple __reserved_11) 1))) in 
   (let __var_c = ((get_from_tuple __reserved_12) 0) in 
   (let __reserved_13 = ((disassemble "::") (((get_from_tuple __reserved_12) 1))) in 
   (let __var_d = ((get_from_tuple __reserved_13) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_13) 1) in 
   ((__var_f __var_a) :: ((__var_f __var_b) :: ((__var_f __var_c) :: ((__var_f __var_d) :: ((__var_map __var_f) __var_tl)))))))))))))) else (exception "Match_failure"))))))))));;
  let rec __var_append = (fun __reserved_20 __reserved_21 -> (let __var_xs = __reserved_20 in 
   (let __var_ys = __reserved_21 in 
   (let __reserved_22 = __var_xs in 
   (if ((same_cons __reserved_22) "[]") then (let __nothing = ((same_cons __reserved_22) (([]))) in 
   __var_ys) else (if ((op_and (((same_cons __reserved_22) "::"))) (((op_and (((op_and true) true))) true))) then (let __reserved_23 = ((disassemble "::") __reserved_22) in 
   (let __var_x = ((get_from_tuple __reserved_23) 0) in 
   (let __var_xs0 = ((get_from_tuple __reserved_23) 1) in 
   (__var_x :: ((__var_append __var_xs0) __var_ys))))) else (exception "Match_failure")))))));;
  let rec __var_helper = (fun __reserved_24 -> (let __var_xs = __reserved_24 in 
   (let __reserved_25 = __var_xs in 
   (if ((same_cons __reserved_25) "[]") then (let __nothing = ((same_cons __reserved_25) (([]))) in 
   ([])) else (if ((op_and (((same_cons __reserved_25) "::"))) (((op_and (((op_and true) true))) true))) then (let __reserved_26 = ((disassemble "::") __reserved_25) in 
   (let __var_h = ((get_from_tuple __reserved_26) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_26) 1) in 
   ((__var_append __var_h) ((__var_helper __var_tl)))))) else (exception "Match_failure"))))));;
  let __var_concat = __var_helper;;
  let rec __var_iter = (fun __reserved_27 __reserved_28 -> (let __var_f = __reserved_27 in 
   (let __var_xs = __reserved_28 in 
   (let __reserved_29 = __var_xs in 
   (if ((same_cons __reserved_29) "[]") then (let __nothing = ((same_cons __reserved_29) (([]))) in 
   ()) else (if ((op_and (((same_cons __reserved_29) "::"))) (((op_and (((op_and true) true))) true))) then (let __reserved_30 = ((disassemble "::") __reserved_29) in 
   (let __var_h = ((get_from_tuple __reserved_30) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_30) 1) in 
   (let __nothing = ((op_eq ((__var_f __var_h))) ()) in 
   ((__var_iter __var_f) __var_tl))))) else (exception "Match_failure")))))));;
  let lifted_0 = (fun __var_h0 __reserved_35 -> (let __var_a = __reserved_35 in 
   (__var_h0, __var_a)));;
  let rec __var_cartesian = (fun __reserved_31 __reserved_32 -> (let __var_xs = __reserved_31 in 
   (let __var_ys = __reserved_32 in 
   (let __reserved_33 = __var_xs in 
   (if ((same_cons __reserved_33) "[]") then (let __nothing = ((same_cons __reserved_33) (([]))) in 
   ([])) else (if ((op_and (((same_cons __reserved_33) "::"))) (((op_and (((op_and true) true))) true))) then (let __reserved_34 = ((disassemble "::") __reserved_33) in 
   (let __var_h = ((get_from_tuple __reserved_34) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_34) 1) in 
   ((__var_append (((__var_map ((lifted_0 __var_h))) __var_ys))) (((__var_cartesian __var_tl) __var_ys)))))) else (exception "Match_failure")))))));;
  let __var_main = (let __nothing = ((op_eq (((__var_iter print_int) ((1 :: (2 :: (3 :: ([])))))))) ()) in 
   (let __nothing1 = ((op_eq ((print_int ((__var_length (((__var_cartesian ((1 :: (2 :: ([]))))) ((1 :: (2 :: (3 :: (4 :: ([]))))))))))))) ()) in 
   0));;

Test do nothing
  $ ./run_remove_patterns.exe <<- EOF
  > let a = 4 in a
  > EOF
  (let __var_a = 4 in 
   __var_a);;
Test let with tuple
  $ ./run_remove_patterns.exe <<- EOF
  > let (a, b, c) = 1, 2, 3 in a
  > EOF
  (let __reserved_0 = (1, 2, 3) in 
   (let __var_a = ((__get_from_tuple __reserved_0) 0) in 
   (let __var_b = ((__get_from_tuple __reserved_0) 1) in 
   (let __var_c = ((__get_from_tuple __reserved_0) 2) in 
   __var_a))));;
Test let and
  $ ./run_remove_patterns.exe <<- EOF
  > let (a, b) = 1, 2 
  > and (c, d) = 4, 5 in a;;
  > let rec aa = 1
  > and bb = 2 in aa
  > EOF
  (let __reserved_0 = (1, 2) in 
   (let __var_a = ((__get_from_tuple __reserved_0) 0) in 
   (let __var_b = ((__get_from_tuple __reserved_0) 1) in 
   (let __reserved_1 = (4, 5) in 
   (let __var_c = ((__get_from_tuple __reserved_1) 0) in 
   (let __var_d = ((__get_from_tuple __reserved_1) 1) in 
   __var_a))))));;
  (let rec __var_aa = 1
   and __var_bb = 2 in 
   __var_aa);;

Test let with patterns
  $ ./run_remove_patterns.exe <<- EOF
  > let [(a, _); (4, c)] = [ (1, 2); (3, 4) ] in a
  > EOF
  (let __reserved_0 = ((__disassemble "::") (((1, 2) :: ((3, 4) :: ([]))))) in 
   (let __reserved_1 = ((__get_from_tuple __reserved_0) 0) in 
   (let __var_a = ((__get_from_tuple __reserved_1) 0) in 
   (let __nothing = ((__get_from_tuple __reserved_1) 1) in 
   (let __reserved_2 = ((__disassemble "::") (((__get_from_tuple __reserved_0) 1))) in 
   (let __reserved_3 = ((__get_from_tuple __reserved_2) 0) in 
   (let __nothing = ((__op_eq (((__get_from_tuple __reserved_3) 0))) 4) in 
   (let __var_c = ((__get_from_tuple __reserved_3) 1) in 
   (let __nothing = ((__same_cons (((__get_from_tuple __reserved_2) 1))) (([]))) in 
   __var_a)))))))));;
Test let fun
  $ ./run_remove_patterns.exe <<- EOF
  > let f (a, b) c = a + b + c in f (1, 2) 4
  > EOF
  (let __var_f = (fun __reserved_0 __reserved_2 -> (let __reserved_1 = __reserved_0 in 
   (let __var_a = ((__get_from_tuple __reserved_1) 0) in 
   (let __var_b = ((__get_from_tuple __reserved_1) 1) in 
   (let __var_c = __reserved_2 in 
   ((__op_plus (((__op_plus __var_a) __var_b))) __var_c)))))) in 
   ((__var_f ((1, 2))) 4));;
Test struct items
  $ ./run_remove_patterns.exe <<- EOF
  > let (a, b) = 1, 2
  > and [c] = [3]
  > and f a = a
  > let rec g a = if a then ff a else a
  > and ff a = not a
  > EOF
  let __reserved_0 = (1, 2);;
  let __var_a = ((__get_from_tuple __reserved_0) 0);;
  let __var_b = ((__get_from_tuple __reserved_0) 1);;
  let __reserved_1 = ((__disassemble "::") ((3 :: ([]))));;
  let __var_c = ((__get_from_tuple __reserved_1) 0);;
  let __nothing = ((__same_cons (((__get_from_tuple __reserved_1) 1))) (([])));;
  let __var_f = (fun __reserved_2 -> (let __var_a = __reserved_2 in 
   __var_a));;
  let rec __var_g = (fun __reserved_3 -> (let __var_a = __reserved_3 in 
   (if __var_a then (__var_ff __var_a) else __var_a)))
   and __var_ff = (fun __reserved_4 -> (let __var_a = __reserved_4 in 
   (not __var_a)));;
Tets fun
  $ ./run_remove_patterns.exe <<- EOF
  > fun (a, b) c -> a + b + c
  > EOF
  (fun __reserved_0 __reserved_2 -> (let __reserved_1 = __reserved_0 in 
   (let __var_a = ((__get_from_tuple __reserved_1) 0) in 
   (let __var_b = ((__get_from_tuple __reserved_1) 1) in 
   (let __var_c = __reserved_2 in 
   ((__op_plus (((__op_plus __var_a) __var_b))) __var_c))))));;
Test easy match
  $ ./run_remove_patterns.exe <<- EOF
  > match 5 with
  > | 4 -> 'n'
  > | 3 -> 'm'
  > EOF
  (let __reserved_0 = 5 in 
   (if ((__op_eq 4) __reserved_0) then (let __nothing = ((__op_eq __reserved_0) 4) in 
   'n') else (if ((__op_eq 3) __reserved_0) then (let __nothing = ((__op_eq __reserved_0) 3) in 
   'm') else (__exception "Match_failure"))));;
Test var match
  $ ./run_remove_patterns.exe <<- EOF
  > match 4 with
  > | 5 -> true
  > | a -> a = 1
  > EOF
  (let __reserved_0 = 4 in 
   (if ((__op_eq 5) __reserved_0) then (let __nothing = ((__op_eq __reserved_0) 5) in 
   true) else (if true then (let __var_a = __reserved_0 in 
   ((__op_eq __var_a) 1)) else (__exception "Match_failure"))));;
Test hard match
  $ ./run_remove_patterns.exe <<- EOF
  > match 3, "biba" with
  > | 3, "boba" -> true
  > | b, a -> (a = "biba") || (b = 4)
  > | _, b -> b = "ocaml"
  > EOF
  (let __reserved_0 = (3, "biba") in 
   (if ((__op_and (((__op_and true) (((__op_eq 3) (((__get_from_tuple __reserved_0) 0))))))) (((__op_eq "boba") (((__get_from_tuple __reserved_0) 1))))) then (let __reserved_3 = __reserved_0 in 
   (let __nothing = ((__op_eq (((__get_from_tuple __reserved_3) 0))) 3) in 
   (let __nothing = ((__op_eq (((__get_from_tuple __reserved_3) 1))) "boba") in 
   true))) else (if ((__op_and (((__op_and true) true))) true) then (let __reserved_2 = __reserved_0 in 
   (let __var_b = ((__get_from_tuple __reserved_2) 0) in 
   (let __var_a = ((__get_from_tuple __reserved_2) 1) in 
   ((__op_or (((__op_eq __var_a) "biba"))) (((__op_eq __var_b) 4)))))) else (if ((__op_and (((__op_and true) true))) true) then (let __reserved_1 = __reserved_0 in 
   (let __nothing = ((__get_from_tuple __reserved_1) 0) in 
   (let __var_b = ((__get_from_tuple __reserved_1) 1) in 
   ((__op_eq __var_b) "ocaml")))) else (__exception "Match_failure")))));;
Test function with constructors
  $ ./run_remove_patterns.exe <<- EOF
  > function | Some (x, y) -> x + 3 | None -> 0 
  > EOF
  (fun __reserved_0 -> (if ((__op_and (((__same_cons __reserved_0) "Some"))) (((__op_and (((__op_and true) true))) true))) then (let __reserved_1 = ((__disassemble "Some") __reserved_0) in 
   (let __var_x = ((__get_from_tuple __reserved_1) 0) in 
   (let __var_y = ((__get_from_tuple __reserved_1) 1) in 
   ((__op_plus __var_x) 3)))) else (if ((__same_cons __reserved_0) "None") then (let __nothing = ((__same_cons __reserved_0) ((None))) in 
   0) else (__exception "Match_failure"))));;
Test or
  $ ./run_remove_patterns.exe <<- EOF
  > function | 1 | 4 -> true
  > EOF
  invalid pattern: BDSML doesn't allow vars in or patter

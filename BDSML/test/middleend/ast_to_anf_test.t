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
Test let with let rec in
  $ ./run_to_anf.exe <<- EOF
  > let length_tail =
  > let rec helper acc xs =
  > match xs with
  > | [] -> acc
  > | h::tl -> helper (acc + 1) tl
  > in
  > helper 0
  Types before middleend:
  val length_tail : 'u list -> int
  
  Types after anf:
  val __var_helper : int -> 's -> int
  val __var_length_tail : 's -> int
  
  let rec __var_helper __reserved_0 __reserved_1 = (let __anf_0 = ((__same_cons __reserved_1) "[]") in 
   (if __anf_0 then (let __nothing = ((__same_cons __reserved_1) (([]))) in 
   __reserved_0) else (let __anf_1 = ((__same_cons __reserved_1) "::") in 
   (let __anf_2 = ((( && ) true) true) in 
   (let __anf_3 = ((( && ) __anf_2) true) in 
   (let __anf_4 = ((( && ) __anf_1) __anf_3) in 
   (if __anf_4 then (let __reserved_3 = ((__disassemble "::") __reserved_1) in 
   (let __var_h = ((__get_from_tuple __reserved_3) 0) in 
   (let __var_tl = ((__get_from_tuple __reserved_3) 1) in 
   (let __anf_5 = ((( + ) __reserved_0) 1) in 
   ((__var_helper __anf_5) __var_tl))))) else (__exception "Match_failure"))))))));;
  let __var_length_tail = (__var_helper 0);;
Test let with let rec in with several capture
  $ ./run_to_anf.exe <<- EOF
  > let f a =
  > let rec helper acc xs =
  > match xs with
  > | [] -> acc
  > | h::tl -> helper (acc + a) tl
  > in
  > helper 0;;
  Types before middleend:
  val f : int -> 'v list -> int
  
  Types after anf:
  val __var_helper : int -> int -> 't -> int
  val __var_f : int -> 't -> int
  
  let rec __var_helper __var_a0 __reserved_1 __reserved_2 = (let __anf_0 = ((__same_cons __reserved_2) "[]") in 
   (if __anf_0 then (let __nothing = ((__same_cons __reserved_2) (([]))) in 
   __reserved_1) else (let __anf_1 = ((__same_cons __reserved_2) "::") in 
   (let __anf_2 = ((( && ) true) true) in 
   (let __anf_3 = ((( && ) __anf_2) true) in 
   (let __anf_4 = ((( && ) __anf_1) __anf_3) in 
   (if __anf_4 then (let __reserved_4 = ((__disassemble "::") __reserved_2) in 
   (let __var_h = ((__get_from_tuple __reserved_4) 0) in 
   (let __var_tl = ((__get_from_tuple __reserved_4) 1) in 
   (let __anf_5 = ((( + ) __reserved_1) __var_a0) in 
   (((__var_helper __var_a0) __anf_5) __var_tl))))) else (__exception "Match_failure"))))))));;
  let __var_f __reserved_0 = ((__var_helper __reserved_0) 0);;

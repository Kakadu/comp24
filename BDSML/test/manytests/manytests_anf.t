  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/001fac.ml
  Types before middleend:
  val fac : int -> int
  val main : int
  
  Types after anf:
  val __var_fac : int -> int
  val __var_main : int
  
  let rec __var_fac __reserved_0 = (let __anf_0 = ((( <= ) __reserved_0) 1) in 
   (if __anf_0 then 1 else (let __anf_1 = ((( - ) __reserved_0) 1) in 
   (let __anf_2 = (__var_fac __anf_1) in 
   ((( * ) __reserved_0) __anf_2)))));;
  let __var_main = (let __anf_3 = (__var_fac 4) in 
   (let __anf_4 = (print_int __anf_3) in 
   (let __nothing = ((( = ) __anf_4) ()) in 
   0)));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/002fac.ml
  Types before middleend:
  val fac_cps : int -> (int -> 'cb) -> 'cb
  val main : int
  
  Types after anf:
  val lifted_1 : (int -> 's) -> int -> int -> 's
  val __var_fac_cps : int -> (int -> 'hb) -> 'hb
  val lifted_0 : 'ib -> 'ib
  val __var_main : int
  
  let lifted_1 __var_k0 __var_n1 __reserved_2 = (let __anf_0 = ((( * ) __reserved_2) __var_n1) in 
   (__var_k0 __anf_0));;
  let rec __var_fac_cps __reserved_0 __reserved_1 = (let __anf_1 = ((( = ) __reserved_0) 1) in 
   (if __anf_1 then (__reserved_1 1) else (let __anf_2 = ((( - ) __reserved_0) 1) in 
   (let __anf_3 = ((lifted_1 __reserved_1) __reserved_0) in 
   ((__var_fac_cps __anf_2) __anf_3)))));;
  let lifted_0 __reserved_3 = __reserved_3;;
  let __var_main = (let __anf_4 = ((__var_fac_cps 4) lifted_0) in 
   (let __anf_5 = (print_int __anf_4) in 
   (let __nothing = ((( = ) __anf_5) ()) in 
   0)));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/003fib.ml
  Types before middleend:
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int
  
  Types after anf:
  val __var_fib_acc : int -> int -> int -> int
  val __var_fib : int -> int
  val __var_main : int
  
  let rec __var_fib_acc __reserved_0 __reserved_1 __reserved_2 = (let __anf_0 = ((( = ) __reserved_2) 1) in 
   (if __anf_0 then __reserved_1 else (let __var_n1 = ((( - ) __reserved_2) 1) in 
   (let __var_ab = ((( + ) __reserved_0) __reserved_1) in 
   (((__var_fib_acc __reserved_1) __var_ab) __var_n1)))));;
  let rec __var_fib __reserved_3 = (let __anf_1 = ((( < ) __reserved_3) 2) in 
   (if __anf_1 then __reserved_3 else (let __anf_2 = ((( - ) __reserved_3) 1) in 
   (let __anf_3 = (__var_fib __anf_2) in 
   (let __anf_4 = ((( - ) __reserved_3) 2) in 
   (let __anf_5 = (__var_fib __anf_4) in 
   ((( + ) __anf_3) __anf_5)))))));;
  let __var_main = (let __anf_6 = (((__var_fib_acc 0) 1) 4) in 
   (let __anf_7 = (print_int __anf_6) in 
   (let __nothing = ((( = ) __anf_7) ()) in 
   (let __anf_8 = (__var_fib 4) in 
   (let __anf_9 = (print_int __anf_8) in 
   (let __nothing0 = ((( = ) __anf_9) ()) in 
   0))))));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/004manyargs.ml
  Types before middleend:
  val wrap : 'n -> 'n
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  
  Types after anf:
  val __var_wrap : 'n -> 'n
  val __var_test3 : int -> int -> int -> int
  val __var_test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val __var_main : int
  
  let __var_wrap __reserved_0 = (let __anf_0 = ((( = ) 1) 1) in 
   (if __anf_0 then __reserved_0 else __reserved_0));;
  let __var_test3 __reserved_1 __reserved_2 __reserved_3 = (let __var_a0 = (print_int __reserved_1) in 
   (let __var_b1 = (print_int __reserved_2) in 
   (let __var_c2 = (print_int __reserved_3) in 
   0)));;
  let __var_test10 __reserved_4 __reserved_5 __reserved_6 __reserved_7 __reserved_8 __reserved_9 __reserved_10 __reserved_11 __reserved_12 __reserved_13 = (let __anf_1 = ((( + ) __reserved_4) __reserved_5) in 
   (let __anf_2 = ((( + ) __anf_1) __reserved_6) in 
   (let __anf_3 = ((( + ) __anf_2) __reserved_7) in 
   (let __anf_4 = ((( + ) __anf_3) __reserved_8) in 
   (let __anf_5 = ((( + ) __anf_4) __reserved_9) in 
   (let __anf_6 = ((( + ) __anf_5) __reserved_10) in 
   (let __anf_7 = ((( + ) __anf_6) __reserved_11) in 
   (let __anf_8 = ((( + ) __anf_7) __reserved_12) in 
   ((( + ) __anf_8) __reserved_13)))))))));;
  let __var_main = (let __var_rez = (((((((((((__var_wrap __var_test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in 
   (let __anf_9 = (print_int __var_rez) in 
   (let __nothing = ((( = ) __anf_9) ()) in 
   (let __var_temp2 = ((((__var_wrap __var_test3) 1) 10) 100) in 
   0))));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/005fix.ml
  Types before middleend:
  val fix : (('p -> 's) -> 'p -> 's) -> 'p -> 's
  val fac : (int -> int) -> int -> int
  val main : int
  
  Types after anf:
  val __var_fix : (('p -> 's) -> 'p -> 's) -> 'p -> 's
  val __var_fac : (int -> int) -> int -> int
  val __var_main : int
  
  let rec __var_fix __reserved_0 __reserved_1 = (let __anf_0 = (__var_fix __reserved_0) in 
   ((__reserved_0 __anf_0) __reserved_1));;
  let __var_fac __reserved_2 __reserved_3 = (let __anf_1 = ((( <= ) __reserved_3) 1) in 
   (if __anf_1 then 1 else (let __anf_2 = ((( - ) __reserved_3) 1) in 
   (let __anf_3 = (__reserved_2 __anf_2) in 
   ((( * ) __reserved_3) __anf_3)))));;
  let __var_main = (let __anf_4 = ((__var_fix __var_fac) 6) in 
   (let __anf_5 = (print_int __anf_4) in 
   (let __nothing = ((( = ) __anf_5) ()) in 
   0)));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/006partial.ml
  Types before middleend:
  val foo : bool -> int -> int
  val foo : int -> int
  val main : int
  
  Types after anf:
  val lifted_0 : int -> int
  val lifted_1 : int -> int
  val __var_foo : bool -> int -> int
  val __var_foo0 : int -> int
  val __var_main : int
  
  let lifted_0 __reserved_1 = ((( + ) __reserved_1) 2);;
  let lifted_1 __reserved_2 = ((( * ) __reserved_2) 10);;
  let __var_foo __reserved_0 = (if __reserved_0 then lifted_0 else lifted_1);;
  let __var_foo0 __reserved_3 = (let __anf_0 = ((__var_foo false) __reserved_3) in 
   (let __anf_1 = ((__var_foo true) __anf_0) in 
   (let __anf_2 = ((__var_foo false) __anf_1) in 
   ((__var_foo true) __anf_2))));;
  let __var_main = (let __anf_3 = (__var_foo0 11) in 
   (let __anf_4 = (print_int __anf_3) in 
   (let __nothing = ((( = ) __anf_4) ()) in 
   0)));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/006partial2.ml
  Types before middleend:
  val foo : int -> int -> int -> int
  val main : int
  
  Types after anf:
  val __var_foo : int -> int -> int -> int
  val __var_main : int
  
  let __var_foo __reserved_0 __reserved_1 __reserved_2 = (let __anf_0 = (print_int __reserved_0) in 
   (let __nothing = ((( = ) __anf_0) ()) in 
   (let __anf_1 = (print_int __reserved_1) in 
   (let __nothing0 = ((( = ) __anf_1) ()) in 
   (let __anf_2 = (print_int __reserved_2) in 
   (let __nothing1 = ((( = ) __anf_2) ()) in 
   (let __anf_3 = ((( * ) __reserved_1) __reserved_2) in 
   ((( + ) __reserved_0) __anf_3))))))));;
  let __var_main = (let __var_foo2 = (__var_foo 1) in 
   (let __var_foo3 = (__var_foo2 2) in 
   (let __var_foo4 = (__var_foo3 3) in 
   (let __anf_4 = (print_int __var_foo4) in 
   (let __nothing = ((( = ) __anf_4) ()) in 
   0)))));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/006partial3.ml
  Types before middleend:
  val foo : int -> int -> int -> unit
  val main : int
  
  Types after anf:
  val lifted_1 : int -> unit
  val lifted_0 : int -> int -> unit
  val __var_foo : int -> int -> int -> unit
  val __var_main : int
  
  let lifted_1 __reserved_2 = (print_int __reserved_2);;
  let lifted_0 __reserved_1 = (let __anf_0 = (print_int __reserved_1) in 
   (let __nothing0 = ((( = ) __anf_0) ()) in 
   lifted_1));;
  let __var_foo __reserved_0 = (let __anf_1 = (print_int __reserved_0) in 
   (let __nothing = ((( = ) __anf_1) ()) in 
   lifted_0));;
  let __var_main = (let __anf_2 = (((__var_foo 4) 8) 9) in 
   (let __nothing = ((( = ) __anf_2) ()) in 
   0));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/007order.ml
  Types before middleend:
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  
  Types after anf:
  val __var__start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val __var_main : unit
  
  let __var__start __reserved_0 __reserved_1 __reserved_2 __reserved_3 __reserved_4 __reserved_5 __reserved_6 __reserved_7 __reserved_8 = (let __nothing = ((( = ) __reserved_0) ()) in 
   (let __nothing0 = ((( = ) __reserved_1) ()) in 
   (let __nothing1 = ((( = ) __reserved_3) ()) in 
   (let __nothing2 = ((( = ) __reserved_6) ()) in 
   (let __anf_0 = ((( + ) __reserved_2) __reserved_4) in 
   (let __anf_1 = (print_int __anf_0) in 
   (let __nothing3 = ((( = ) __anf_1) ()) in 
   (let __anf_2 = (print_int __reserved_8) in 
   (let __nothing4 = ((( = ) __anf_2) ()) in 
   (let __anf_3 = ((( * ) __reserved_2) __reserved_4) in 
   (let __anf_4 = ((( / ) __anf_3) __reserved_5) in 
   ((( + ) __anf_4) __reserved_7))))))))))));;
  let __var_main = (let __anf_5 = (print_int 1) in 
   (let __anf_6 = (print_int 2) in 
   (let __anf_7 = (print_int 4) in 
   (let __anf_8 = (( ~- ) 1) in 
   (let __anf_9 = (print_int __anf_8) in 
   (let __anf_10 = (( ~- ) 555555) in 
   (let __anf_11 = (((((((((__var__start __anf_5) __anf_6) 3) __anf_7) 100) 1000) __anf_9) 10000) __anf_10) in 
   (print_int __anf_11))))))));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/008ascription.ml
  Types before middleend:
  val addi : ('p -> bool -> int) -> ('p -> bool) -> 'p -> int
  val main : int
  
  Types after anf:
  val __var_addi : ('p -> 'q -> 's) -> ('p -> 'q) -> 'p -> 's
  val lifted_0 : int -> bool -> int
  val lifted_1 : int -> bool
  val __var_main : int
  
  let __var_addi __reserved_0 __reserved_1 __reserved_2 = (let __anf_0 = (__reserved_1 __reserved_2) in 
   ((__reserved_0 __reserved_2) __anf_0));;
  let lifted_0 __reserved_3 __reserved_4 = (if __reserved_4 then ((( + ) __reserved_3) 1) else ((( * ) __reserved_3) 2));;
  let lifted_1 __reserved_5 = (let __anf_1 = ((( / ) __reserved_5) 2) in 
   ((( = ) __anf_1) 0));;
  let __var_main = (let __anf_2 = (((__var_addi lifted_0) lifted_1) 4) in 
   (let __anf_3 = (print_int __anf_2) in 
   (let __nothing = ((( = ) __anf_3) ()) in 
   0)));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/009let_poly.ml
  Types before middleend:
  val temp : (int * bool)
  
  Types after anf:
  val lifted_0 : 'n -> 'n
  val __var_temp : (int * bool)
  
  let lifted_0 __reserved_0 = __reserved_0;;
  let __var_temp = (let __anf_0 = (lifted_0 1) in 
   (let __anf_1 = (lifted_0 true) in 
   (__anf_0, __anf_1)));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/015tuples.ml
  Types before middleend:
  val fix : (('p -> 's) -> 'p -> 's) -> 'p -> 's
  val map : ('w -> 'y) -> ('w * 'w) -> ('y * 'y)
  val fixpoly : (('gb -> 'jb * 'gb -> 'jb) -> 'gb -> 'jb * ('gb -> 'jb * 'gb -> 'jb) -> 'gb -> 'jb) -> ('gb -> 'jb * 'gb -> 'jb)
  val feven : ('qb * int -> int) -> int -> int
  val fodd : (int -> int * 'cc) -> int -> int
  val tie : (int -> int * int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int
  
  Types after anf:
  val __var_fix : (('p -> 's) -> 'p -> 's) -> 'p -> 's
  val __var_map : ('gb -> 'hb) -> 'u -> ('hb * 'hb)
  val lifted_1 : 'ib -> ('ib -> 'mb) -> ('mb -> 'lb -> 'ob) -> 'lb -> 'ob
  val lifted_0 : ('qb -> 'tb) -> 'qb -> ('bc -> 'cc * 'bc -> 'cc)
  val __var_fixpoly : 'fc -> ('ic -> 'jc * 'ic -> 'jc)
  val __var_feven : 'mc -> int -> int
  val __var_fodd : 'ed -> int -> int
  val __var_tie : ('ic -> 'jc * 'ic -> 'jc)
  val __var_meven : int -> int
  val __var_modd : int -> int
  val __var_main : int
  
  let rec __var_fix __reserved_0 __reserved_1 = (let __anf_0 = (__var_fix __reserved_0) in 
   ((__reserved_0 __anf_0) __reserved_1));;
  let __var_map __reserved_2 __reserved_3 = (let __var_a = ((get_from_tuple __reserved_3) 0) in 
   (let __var_b = ((get_from_tuple __reserved_3) 1) in 
   (let __anf_1 = (__reserved_2 __var_a) in 
   (let __anf_2 = (__reserved_2 __var_b) in 
   (__anf_1, __anf_2)))));;
  let lifted_1 __var_l00 __var_self1 __reserved_8 __reserved_9 = (let __anf_3 = (__var_self1 __var_l00) in 
   ((__reserved_8 __anf_3) __reserved_9));;
  let lifted_0 __reserved_6 __reserved_7 = (let __anf_4 = ((lifted_1 __reserved_7) __reserved_6) in 
   ((__var_map __anf_4) __reserved_7));;
  let __var_fixpoly __reserved_5 = ((__var_fix lifted_0) __reserved_5);;
  let __var_feven __reserved_10 __reserved_11 = (let __var_e = ((get_from_tuple __reserved_10) 0) in 
   (let __var_o = ((get_from_tuple __reserved_10) 1) in 
   (let __anf_5 = ((( == ) __reserved_11) 0) in 
   (if __anf_5 then 1 else (let __anf_6 = ((( - ) __reserved_11) 1) in 
   (__var_o __anf_6))))));;
  let __var_fodd __reserved_13 __reserved_14 = (let __var_e = ((get_from_tuple __reserved_13) 0) in 
   (let __var_o = ((get_from_tuple __reserved_13) 1) in 
   (let __anf_7 = ((( == ) __reserved_14) 0) in 
   (if __anf_7 then 0 else (let __anf_8 = ((( - ) __reserved_14) 1) in 
   (__var_e __anf_8))))));;
  let __var_tie = (__var_fixpoly ((__var_feven, __var_fodd)));;
  let rec __var_meven __reserved_16 = (let __anf_9 = ((( = ) __reserved_16) 0) in 
   (if __anf_9 then 1 else (let __anf_10 = ((( - ) __reserved_16) 1) in 
   (__var_modd __anf_10))))
   and __var_modd __reserved_17 = (let __anf_11 = ((( = ) __reserved_17) 0) in 
   (if __anf_11 then 1 else (let __anf_12 = ((( - ) __reserved_17) 1) in 
   (__var_meven __anf_12))));;
  let __var_main = (let __anf_13 = (__var_modd 1) in 
   (let __anf_14 = (print_int __anf_13) in 
   (let __nothing = ((( = ) __anf_14) ()) in 
   (let __anf_15 = (__var_meven 2) in 
   (let __anf_16 = (print_int __anf_15) in 
   (let __nothing1 = ((( = ) __anf_16) ()) in 
   (let __var_even = ((get_from_tuple __var_tie) 0) in 
   (let __var_odd = ((get_from_tuple __var_tie) 1) in 
   (let __anf_17 = (__var_odd 3) in 
   (let __anf_18 = (print_int __anf_17) in 
   (let __nothing2 = ((( = ) __anf_18) ()) in 
   (let __anf_19 = (__var_even 4) in 
   (let __anf_20 = (print_int __anf_19) in 
   (let __nothing3 = ((( = ) __anf_20) ()) in 
   0))))))))))))));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/016lists.ml
  Types before middleend:
  val length : 'q list -> int
  val length_tail : 'ab list -> int
  val map : ('nb -> 'pb) -> 'nb list -> 'pb list
  val append : 'uc list -> 'uc list -> 'uc list
  val concat : 'uc list list -> 'uc list
  val iter : ('md -> unit) -> 'md list -> unit
  val cartesian : 'wd list -> 'yd list -> ('wd * 'yd) list
  val main : int
  
  Types after anf:
  Inference error: Type infering error: failed unification of types int and string
  
  let rec __var_length __reserved_0 = (let __anf_0 = ((same_cons __reserved_0) "[]") in 
   (if __anf_0 then (let __nothing = ((same_cons __reserved_0) 3) in 
   0) else (let __anf_1 = ((same_cons __reserved_0) "::") in 
   (let __anf_2 = ((( && ) true) true) in 
   (let __anf_3 = ((( && ) __anf_2) true) in 
   (let __anf_4 = ((( && ) __anf_1) __anf_3) in 
   (if __anf_4 then (let __reserved_2 = (disassemble __reserved_0) in 
   (let __var_h = ((get_from_tuple __reserved_2) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_2) 1) in 
   (let __anf_5 = (__var_length __var_tl) in 
   ((( + ) 1) __anf_5))))) else (exception "Match_failure"))))))));;
  let rec __var_helper __reserved_3 __reserved_4 = (let __anf_6 = ((same_cons __reserved_4) "[]") in 
   (if __anf_6 then (let __nothing = ((same_cons __reserved_4) 3) in 
   __reserved_3) else (let __anf_7 = ((same_cons __reserved_4) "::") in 
   (let __anf_8 = ((( && ) true) true) in 
   (let __anf_9 = ((( && ) __anf_8) true) in 
   (let __anf_10 = ((( && ) __anf_7) __anf_9) in 
   (if __anf_10 then (let __reserved_6 = (disassemble __reserved_4) in 
   (let __var_h = ((get_from_tuple __reserved_6) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_6) 1) in 
   (let __anf_11 = ((( + ) __reserved_3) 1) in 
   ((__var_helper __anf_11) __var_tl))))) else (exception "Match_failure"))))))));;
  let __var_length_tail = (__var_helper 0);;
  let rec __var_map __reserved_7 __reserved_8 = (let __anf_12 = ((same_cons __reserved_8) "[]") in 
   (if __anf_12 then (let __nothing = ((same_cons __reserved_8) 3) in 
   ([])) else (let __anf_13 = ((same_cons __reserved_8) "::") in 
   (let __anf_14 = ((( && ) true) true) in 
   (let __anf_15 = (disassemble __reserved_8) in 
   (let __anf_16 = ((get_from_tuple __anf_15) 1) in 
   (let __anf_17 = ((same_cons __anf_16) "[]") in 
   (let __anf_18 = ((( && ) __anf_14) __anf_17) in 
   (let __anf_19 = ((( && ) __anf_13) __anf_18) in 
   (if __anf_19 then (let __reserved_19 = (disassemble __reserved_8) in 
   (let __var_a = ((get_from_tuple __reserved_19) 0) in 
   (let __anf_20 = ((get_from_tuple __reserved_19) 1) in 
   (let __nothing = ((same_cons __anf_20) 3) in 
   (let __anf_21 = (__reserved_7 __var_a) in 
   (__anf_21 :: ([]))))))) else (let __anf_22 = ((same_cons __reserved_8) "::") in 
   (let __anf_23 = ((( && ) true) true) in 
   (let __anf_24 = (disassemble __reserved_8) in 
   (let __anf_25 = ((get_from_tuple __anf_24) 1) in 
   (let __anf_26 = ((same_cons __anf_25) "::") in 
   (let __anf_27 = ((( && ) true) true) in 
   (let __anf_28 = (disassemble __reserved_8) in 
   (let __anf_29 = ((get_from_tuple __anf_28) 1) in 
   (let __anf_30 = (disassemble __anf_29) in 
   (let __anf_31 = ((get_from_tuple __anf_30) 1) in 
   (let __anf_32 = ((same_cons __anf_31) "[]") in 
   (let __anf_33 = ((( && ) __anf_27) __anf_32) in 
   (let __anf_34 = ((( && ) __anf_26) __anf_33) in 
   (let __anf_35 = ((( && ) __anf_23) __anf_34) in 
   (let __anf_36 = ((( && ) __anf_22) __anf_35) in 
   (if __anf_36 then (let __reserved_17 = (disassemble __reserved_8) in 
   (let __var_a = ((get_from_tuple __reserved_17) 0) in 
   (let __anf_37 = ((get_from_tuple __reserved_17) 1) in 
   (let __reserved_18 = (disassemble __anf_37) in 
   (let __var_b = ((get_from_tuple __reserved_18) 0) in 
   (let __anf_38 = ((get_from_tuple __reserved_18) 1) in 
   (let __nothing = ((same_cons __anf_38) 3) in 
   (let __anf_39 = (__reserved_7 __var_a) in 
   (let __anf_40 = (__reserved_7 __var_b) in 
   (__anf_39 :: (__anf_40 :: ([])))))))))))) else (let __anf_41 = ((same_cons __reserved_8) "::") in 
   (let __anf_42 = ((( && ) true) true) in 
   (let __anf_43 = (disassemble __reserved_8) in 
   (let __anf_44 = ((get_from_tuple __anf_43) 1) in 
   (let __anf_45 = ((same_cons __anf_44) "::") in 
   (let __anf_46 = ((( && ) true) true) in 
   (let __anf_47 = (disassemble __reserved_8) in 
   (let __anf_48 = ((get_from_tuple __anf_47) 1) in 
   (let __anf_49 = (disassemble __anf_48) in 
   (let __anf_50 = ((get_from_tuple __anf_49) 1) in 
   (let __anf_51 = ((same_cons __anf_50) "::") in 
   (let __anf_52 = ((( && ) true) true) in 
   (let __anf_53 = (disassemble __reserved_8) in 
   (let __anf_54 = ((get_from_tuple __anf_53) 1) in 
   (let __anf_55 = (disassemble __anf_54) in 
   (let __anf_56 = ((get_from_tuple __anf_55) 1) in 
   (let __anf_57 = (disassemble __anf_56) in 
   (let __anf_58 = ((get_from_tuple __anf_57) 1) in 
   (let __anf_59 = ((same_cons __anf_58) "[]") in 
   (let __anf_60 = ((( && ) __anf_52) __anf_59) in 
   (let __anf_61 = ((( && ) __anf_51) __anf_60) in 
   (let __anf_62 = ((( && ) __anf_46) __anf_61) in 
   (let __anf_63 = ((( && ) __anf_45) __anf_62) in 
   (let __anf_64 = ((( && ) __anf_42) __anf_63) in 
   (let __anf_65 = ((( && ) __anf_41) __anf_64) in 
   (if __anf_65 then (let __reserved_14 = (disassemble __reserved_8) in 
   (let __var_a = ((get_from_tuple __reserved_14) 0) in 
   (let __anf_66 = ((get_from_tuple __reserved_14) 1) in 
   (let __reserved_15 = (disassemble __anf_66) in 
   (let __var_b = ((get_from_tuple __reserved_15) 0) in 
   (let __anf_67 = ((get_from_tuple __reserved_15) 1) in 
   (let __reserved_16 = (disassemble __anf_67) in 
   (let __var_c = ((get_from_tuple __reserved_16) 0) in 
   (let __anf_68 = ((get_from_tuple __reserved_16) 1) in 
   (let __nothing = ((same_cons __anf_68) 3) in 
   (let __anf_69 = (__reserved_7 __var_a) in 
   (let __anf_70 = (__reserved_7 __var_b) in 
   (let __anf_71 = (__reserved_7 __var_c) in 
   (__anf_69 :: (__anf_70 :: (__anf_71 :: ([]))))))))))))))))) else (let __anf_72 = ((same_cons __reserved_8) "::") in 
   (let __anf_73 = ((( && ) true) true) in 
   (let __anf_74 = (disassemble __reserved_8) in 
   (let __anf_75 = ((get_from_tuple __anf_74) 1) in 
   (let __anf_76 = ((same_cons __anf_75) "::") in 
   (let __anf_77 = ((( && ) true) true) in 
   (let __anf_78 = (disassemble __reserved_8) in 
   (let __anf_79 = ((get_from_tuple __anf_78) 1) in 
   (let __anf_80 = (disassemble __anf_79) in 
   (let __anf_81 = ((get_from_tuple __anf_80) 1) in 
   (let __anf_82 = ((same_cons __anf_81) "::") in 
   (let __anf_83 = ((( && ) true) true) in 
   (let __anf_84 = (disassemble __reserved_8) in 
   (let __anf_85 = ((get_from_tuple __anf_84) 1) in 
   (let __anf_86 = (disassemble __anf_85) in 
   (let __anf_87 = ((get_from_tuple __anf_86) 1) in 
   (let __anf_88 = (disassemble __anf_87) in 
   (let __anf_89 = ((get_from_tuple __anf_88) 1) in 
   (let __anf_90 = ((same_cons __anf_89) "::") in 
   (let __anf_91 = ((( && ) true) true) in 
   (let __anf_92 = ((( && ) __anf_91) true) in 
   (let __anf_93 = ((( && ) __anf_90) __anf_92) in 
   (let __anf_94 = ((( && ) __anf_83) __anf_93) in 
   (let __anf_95 = ((( && ) __anf_82) __anf_94) in 
   (let __anf_96 = ((( && ) __anf_77) __anf_95) in 
   (let __anf_97 = ((( && ) __anf_76) __anf_96) in 
   (let __anf_98 = ((( && ) __anf_73) __anf_97) in 
   (let __anf_99 = ((( && ) __anf_72) __anf_98) in 
   (if __anf_99 then (let __reserved_10 = (disassemble __reserved_8) in 
   (let __var_a = ((get_from_tuple __reserved_10) 0) in 
   (let __anf_100 = ((get_from_tuple __reserved_10) 1) in 
   (let __reserved_11 = (disassemble __anf_100) in 
   (let __var_b = ((get_from_tuple __reserved_11) 0) in 
   (let __anf_101 = ((get_from_tuple __reserved_11) 1) in 
   (let __reserved_12 = (disassemble __anf_101) in 
   (let __var_c = ((get_from_tuple __reserved_12) 0) in 
   (let __anf_102 = ((get_from_tuple __reserved_12) 1) in 
   (let __reserved_13 = (disassemble __anf_102) in 
   (let __var_d = ((get_from_tuple __reserved_13) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_13) 1) in 
   (let __anf_103 = (__reserved_7 __var_a) in 
   (let __anf_104 = (__reserved_7 __var_b) in 
   (let __anf_105 = (__reserved_7 __var_c) in 
   (let __anf_106 = (__reserved_7 __var_d) in 
   (let __anf_107 = ((__var_map __reserved_7) __var_tl) in 
   (__anf_103 :: (__anf_104 :: (__anf_105 :: (__anf_106 :: __anf_107))))))))))))))))))))) else (exception "Match_failure"))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));;
  let rec __var_append __reserved_20 __reserved_21 = (let __anf_108 = ((same_cons __reserved_20) "[]") in 
   (if __anf_108 then (let __nothing = ((same_cons __reserved_20) 3) in 
   __reserved_21) else (let __anf_109 = ((same_cons __reserved_20) "::") in 
   (let __anf_110 = ((( && ) true) true) in 
   (let __anf_111 = ((( && ) __anf_110) true) in 
   (let __anf_112 = ((( && ) __anf_109) __anf_111) in 
   (if __anf_112 then (let __reserved_23 = (disassemble __reserved_20) in 
   (let __var_x = ((get_from_tuple __reserved_23) 0) in 
   (let __var_xs0 = ((get_from_tuple __reserved_23) 1) in 
   (let __anf_113 = ((__var_append __var_xs0) __reserved_21) in 
   (__var_x :: __anf_113))))) else (exception "Match_failure"))))))));;
  let rec __var_helper __reserved_24 = (let __anf_114 = ((same_cons __reserved_24) "[]") in 
   (if __anf_114 then (let __nothing = ((same_cons __reserved_24) 3) in 
   ([])) else (let __anf_115 = ((same_cons __reserved_24) "::") in 
   (let __anf_116 = ((( && ) true) true) in 
   (let __anf_117 = ((( && ) __anf_116) true) in 
   (let __anf_118 = ((( && ) __anf_115) __anf_117) in 
   (if __anf_118 then (let __reserved_26 = (disassemble __reserved_24) in 
   (let __var_h = ((get_from_tuple __reserved_26) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_26) 1) in 
   (let __anf_119 = (__var_helper __var_tl) in 
   ((__var_append __var_h) __anf_119))))) else (exception "Match_failure"))))))));;
  let rec __var_iter __reserved_27 __reserved_28 = (let __anf_120 = ((same_cons __reserved_28) "[]") in 
   (if __anf_120 then (let __nothing = ((same_cons __reserved_28) 3) in 
   ()) else (let __anf_121 = ((same_cons __reserved_28) "::") in 
   (let __anf_122 = ((( && ) true) true) in 
   (let __anf_123 = ((( && ) __anf_122) true) in 
   (let __anf_124 = ((( && ) __anf_121) __anf_123) in 
   (if __anf_124 then (let __reserved_30 = (disassemble __reserved_28) in 
   (let __var_h = ((get_from_tuple __reserved_30) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_30) 1) in 
   (let __anf_125 = (__reserved_27 __var_h) in 
   (let __nothing = ((( = ) __anf_125) ()) in 
   ((__var_iter __reserved_27) __var_tl)))))) else (exception "Match_failure"))))))));;
  let lifted_0 __var_h0 __reserved_35 = (__var_h0, __reserved_35);;
  let rec __var_cartesian __reserved_31 __reserved_32 = (let __anf_126 = ((same_cons __reserved_31) "[]") in 
   (if __anf_126 then (let __nothing = ((same_cons __reserved_31) 3) in 
   ([])) else (let __anf_127 = ((same_cons __reserved_31) "::") in 
   (let __anf_128 = ((( && ) true) true) in 
   (let __anf_129 = ((( && ) __anf_128) true) in 
   (let __anf_130 = ((( && ) __anf_127) __anf_129) in 
   (if __anf_130 then (let __reserved_34 = (disassemble __reserved_31) in 
   (let __var_h = ((get_from_tuple __reserved_34) 0) in 
   (let __var_tl = ((get_from_tuple __reserved_34) 1) in 
   (let __anf_131 = (lifted_0 __var_h) in 
   (let __anf_132 = ((__var_map __anf_131) __reserved_32) in 
   (let __anf_133 = ((__var_cartesian __var_tl) __reserved_32) in 
   ((__var_append __anf_132) __anf_133))))))) else (exception "Match_failure"))))))));;
  let __var_main = (let __anf_134 = ((__var_iter print_int) ((1 :: (2 :: (3 :: ([])))))) in 
   (let __nothing = ((( = ) __anf_134) ()) in 
   (let __anf_135 = ((__var_cartesian ((1 :: (2 :: ([]))))) ((1 :: (2 :: (3 :: (4 :: ([]))))))) in 
   (let __anf_136 = (__var_length __anf_135) in 
   (let __anf_137 = (print_int __anf_136) in 
   (let __nothing1 = ((( = ) __anf_137) ()) in 
   0))))));;

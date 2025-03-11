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
  val fac_cps : int -> (int -> 'eb) -> 'eb
  val main : int
  
  Types after anf:
  val lifted_1 : (int -> 'u) -> int -> int -> 'u
  val __var_fac_cps : int -> (int -> 'jb) -> 'jb
  val lifted_0 : 'kb -> 'kb
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
  val wrap : 'p -> 'p
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  
  Types after anf:
  val __var_wrap : 'p -> 'p
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
  val fix : (('r -> 'u) -> 'r -> 'u) -> 'r -> 'u
  val fac : (int -> int) -> int -> int
  val main : int
  
  Types after anf:
  val __var_fix : (('r -> 'u) -> 'r -> 'u) -> 'r -> 'u
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
  val addi : ('r -> bool -> int) -> ('r -> bool) -> 'r -> int
  val main : int
  
  Types after anf:
  val __var_addi : ('r -> 's -> 'u) -> ('r -> 's) -> 'r -> 'u
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
  val lifted_0 : 'p -> 'p
  val __var_temp : (int * bool)
  
  let lifted_0 __reserved_0 = __reserved_0;;
  let __var_temp = (let __anf_0 = (lifted_0 1) in 
   (let __anf_1 = (lifted_0 true) in 
   (__anf_0, __anf_1)));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/015tuples.ml
  Types before middleend:
  val fix : (('r -> 'u) -> 'r -> 'u) -> 'r -> 'u
  val map : ('y -> 'bb) -> ('y * 'y) -> ('bb * 'bb)
  val fixpoly : (('ib -> 'lb * 'ib -> 'lb) -> 'ib -> 'lb * ('ib -> 'lb * 'ib -> 'lb) -> 'ib -> 'lb) -> ('ib -> 'lb * 'ib -> 'lb)
  val feven : ('sb * int -> int) -> int -> int
  val fodd : (int -> int * 'ec) -> int -> int
  val tie : (int -> int * int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int
  
  Types after anf:
  val __var_fix : (('r -> 'u) -> 'r -> 'u) -> 'r -> 'u
  val __var_map : ('ib -> 'jb) -> 'w -> ('jb * 'jb)
  val lifted_1 : 'kb -> ('kb -> 'ob) -> ('ob -> 'nb -> 'qb) -> 'nb -> 'qb
  val lifted_0 : ('sb -> 'vb) -> 'sb -> ('dc -> 'ec * 'dc -> 'ec)
  val __var_fixpoly : 'hc -> ('kc -> 'lc * 'kc -> 'lc)
  val __var_feven : 'oc -> int -> int
  val __var_fodd : 'gd -> int -> int
  val __var_tie : ('kc -> 'lc * 'kc -> 'lc)
  val __var_meven : int -> int
  val __var_modd : int -> int
  val __var_main : int
  
  let rec __var_fix __reserved_0 __reserved_1 = (let __anf_0 = (__var_fix __reserved_0) in 
   ((__reserved_0 __anf_0) __reserved_1));;
  let __var_map __reserved_2 __reserved_3 = (let __var_a = ((__get_from_tuple __reserved_3) 0) in 
   (let __var_b = ((__get_from_tuple __reserved_3) 1) in 
   (let __anf_1 = (__reserved_2 __var_a) in 
   (let __anf_2 = (__reserved_2 __var_b) in 
   (__anf_1, __anf_2)))));;
  let lifted_1 __var_l00 __var_self1 __reserved_8 __reserved_9 = (let __anf_3 = (__var_self1 __var_l00) in 
   ((__reserved_8 __anf_3) __reserved_9));;
  let lifted_0 __reserved_6 __reserved_7 = (let __anf_4 = ((lifted_1 __reserved_7) __reserved_6) in 
   ((__var_map __anf_4) __reserved_7));;
  let __var_fixpoly __reserved_5 = ((__var_fix lifted_0) __reserved_5);;
  let __var_feven __reserved_10 __reserved_11 = (let __var_e = ((__get_from_tuple __reserved_10) 0) in 
   (let __var_o = ((__get_from_tuple __reserved_10) 1) in 
   (let __anf_5 = ((( == ) __reserved_11) 0) in 
   (if __anf_5 then 1 else (let __anf_6 = ((( - ) __reserved_11) 1) in 
   (__var_o __anf_6))))));;
  let __var_fodd __reserved_13 __reserved_14 = (let __var_e = ((__get_from_tuple __reserved_13) 0) in 
   (let __var_o = ((__get_from_tuple __reserved_13) 1) in 
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
   (let __var_even = ((__get_from_tuple __var_tie) 0) in 
   (let __var_odd = ((__get_from_tuple __var_tie) 1) in 
   (let __anf_17 = (__var_odd 3) in 
   (let __anf_18 = (print_int __anf_17) in 
   (let __nothing2 = ((( = ) __anf_18) ()) in 
   (let __anf_19 = (__var_even 4) in 
   (let __anf_20 = (print_int __anf_19) in 
   (let __nothing3 = ((( = ) __anf_20) ()) in 
   0))))))))))))));;
  $ ../middleend/run_to_anf.exe run_to_anf < manytests_link/typed/016lists.ml
  Types before middleend:
  val length : 's list -> int
  val length_tail : 'cb list -> int
  val map : ('pb -> 'rb) -> 'pb list -> 'rb list
  val append : 'wc list -> 'wc list -> 'wc list
  val concat : 'wc list list -> 'wc list
  val iter : ('od -> unit) -> 'od list -> unit
  val cartesian : 'yd list -> 'be list -> ('yd * 'be) list
  val main : int
  
  invalid previous middleend result in anf: BDSML doesn't support vars in "let rec in"

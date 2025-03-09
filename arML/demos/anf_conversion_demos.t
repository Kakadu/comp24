MANYTESTS
  $ ./start_anf_conversion_demos.exe < manytests/typed/001fac.ml
  ANF 
  let rec fac n = (let i_2 = (( <= ) n 1) in (if i_2 then 1 else (let i_1 = (( - ) n 1) in (let i_0 = (fac i_1) in (( * ) n i_0)))))
  let main = (let i_3 = (fac 4) in (let () = (print_int i_3) in 0))
  
  Types (before anf conversion): 
  val fac : int -> int
  val main : int
  
  Types (after anf conversion): 
  val fac : int -> int
  val main : int

  $ ./start_anf_conversion_demos.exe < manytests/typed/002fac.ml
  ANF 
  let rec fac_cps n k = (let i_3 = (( = ) n 1) in (if i_3 then (k 1) else (let i_2 = (( - ) n 1) in (let i_1 = (ll_1 n k) in (fac_cps i_2 i_1))))) and ll_1 cc_1 cc_0 p = (let i_0 = (( * ) p cc_1) in (cc_0 i_0))
  let ll_0 ac_0 = ac_0
  let main = (let i_4 = (fac_cps 4 ll_0) in (let () = (print_int i_4) in 0))
  
  Types (before anf conversion): 
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Types (after anf conversion): 
  val fac_cps : int -> (int -> 'a) -> 'a
  val ll_1 : int -> (int -> 'a) -> int -> 'a
  val ll_0 : 'a -> 'a
  val main : int

  $ ./start_anf_conversion_demos.exe < manytests/typed/003fib.ml
  ANF 
  let rec fib_acc a b n = (let i_0 = (( = ) n 1) in (if i_0 then b else (let n1 = (( - ) n 1) in (let ab = (( + ) a b) in (fib_acc b ab n1)))))
  let rec fib ac_0 = (let i_5 = (( < ) ac_0 2) in (if i_5 then ac_0 else (let i_4 = (( - ) ac_0 1) in (let i_3 = (fib i_4) in (let i_2 = (( - ) ac_0 2) in (let i_1 = (fib i_2) in (( + ) i_3 i_1)))))))
  let main = (let i_6 = (fib_acc 0 1 4) in (let () = (print_int i_6) in (let i_7 = (fib 4) in (let () = (print_int i_7) in 0))))
  
  Types (before anf conversion): 
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int
  
  Types (after anf conversion): 
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ ./start_anf_conversion_demos.exe < manytests/typed/004manyargs.ml
  ANF 
  let wrap f = (let i_0 = (( = ) 1 1) in (if i_0 then f else f))
  let test3 a b c = (let ac_0 = (print_int a) in (let ac_1 = (print_int b) in (let ac_2 = (print_int c) in 0)))
  let test10 ac_6 ac_5 ac_4 d e ac_3 g h i j = (let i_8 = (( + ) ac_6 ac_5) in (let i_7 = (( + ) i_8 ac_4) in (let i_6 = (( + ) i_7 d) in (let i_5 = (( + ) i_6 e) in (let i_4 = (( + ) i_5 ac_3) in (let i_3 = (( + ) i_4 g) in (let i_2 = (( + ) i_3 h) in (let i_1 = (( + ) i_2 i) in (( + ) i_1 j)))))))))
  let main = (let rez = (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000) in (let () = (print_int rez) in (let temp2 = (wrap test3 1 10 100) in 0)))
  
  Types (before anf conversion): 
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  
  Types (after anf conversion): 
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ ./start_anf_conversion_demos.exe < manytests/typed/005fix.ml
  ANF 
  let rec fix f x = (let i_0 = (fix f) in (f i_0 x))
  let fac self n = (let i_3 = (( <= ) n 1) in (if i_3 then 1 else (let i_2 = (( - ) n 1) in (let i_1 = (self i_2) in (( * ) n i_1)))))
  let main = (let i_4 = (fix fac 6) in (let () = (print_int i_4) in 0))
  
  Types (before anf conversion): 
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int
  
  Types (after anf conversion): 
  val fix : ('a -> 'b -> 'c) -> 'b -> 'c
  val fac : (int -> 'a) -> int -> int
  val main : int

  $ ./start_anf_conversion_demos.exe < manytests/typed/006partial.ml
  ANF 
  let ll_0 foo = (( + ) foo 2)
  let ll_1 ac_0 = (( * ) ac_0 10)
  let ac_1 b = (if b then ll_0 else ll_1)
  let ac_2 x = (let i_2 = (ac_1 false x) in (let i_1 = (ac_1 true i_2) in (let i_0 = (ac_1 false i_1) in (ac_1 true i_0))))
  let main = (let i_3 = (ac_2 11) in (let () = (print_int i_3) in 0))
  
  Types (before anf conversion): 
  val foo : int -> int
  val main : int
  
  Types (after anf conversion): 
  val ll_0 : int -> int
  val ll_1 : int -> int
  val ac_1 : bool -> int -> int
  val ac_2 : int -> int
  val main : int

  $ ./start_anf_conversion_demos.exe < manytests/typed/006partial2.ml
  ANF 
  let foo a b c = (let () = (print_int a) in (let () = (print_int b) in (let () = (print_int c) in (let i_0 = (( * ) b c) in (( + ) a i_0)))))
  let main = (let ac_0 = (foo 1) in (let ac_1 = (ac_0 2) in (let cc_0 = (ac_1 3) in (let () = (print_int cc_0) in 0))))
  
  Types (before anf conversion): 
  val foo : int -> int -> int -> int
  val main : int
  
  Types (after anf conversion): 
  val foo : int -> int -> int -> int
  val main : int

  $ ./start_anf_conversion_demos.exe < manytests/typed/006partial3.ml
  ANF 
  let ll_1 c = (print_int c)
  let ll_0 b = (let () = (print_int b) in ll_1)
  let foo a = (let () = (print_int a) in ll_0)
  let main = (let () = (foo 4 8 9) in 0)
  
  Types (before anf conversion): 
  val foo : int -> int -> int -> unit
  val main : int
  
  Types (after anf conversion): 
  val ll_1 : int -> unit
  val ll_0 : int -> int -> unit
  val foo : int -> int -> int -> unit
  val main : int

  $ ./start_anf_conversion_demos.exe < manytests/typed/007order.ml
  ANF 
  let _start () () a () b _c () d __ = (let i_0 = (( + ) a b) in (let () = (print_int i_0) in (let () = (print_int __) in (let i_2 = (( * ) a b) in (let i_1 = (( / ) i_2 _c) in (( + ) i_1 d))))))
  let main = (let i_9 = (print_int 1) in (let i_8 = (print_int 2) in (let i_7 = (print_int 4) in (let i_6 = (U- 1) in (let i_5 = (print_int i_6) in (let i_4 = (U- 555555) in (let i_3 = (_start i_9 i_8 3 i_7 100 1000 i_5 10000 i_4) in (print_int i_3))))))))
  
  Types (before anf conversion): 
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  
  Types (after anf conversion): 
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ ./start_anf_conversion_demos.exe < manytests/typed/008ascription.ml
  ANF 
  let addi f g x = (let i_0 = ((g x) : bool) in ((f x i_0) : int))
  let ll_0 ac_0 b = (if b then (( + ) ac_0 1) else (( * ) ac_0 2))
  let ll_1 _start = (let i_1 = (( / ) _start 2) in (( = ) i_1 0))
  let main = (let i_2 = (addi ll_0 ll_1 4) in (let () = (print_int i_2) in 0))
  
  Types (before anf conversion): 
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int
  
  Types (after anf conversion): 
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val ll_0 : int -> bool -> int
  val ll_1 : int -> bool
  val main : int

  $ ./start_anf_conversion_demos.exe < manytests/typed/009let_poly.ml
  ANF 
  let ll_0 x = x
  let temp = (let i_0 = (ll_0 1) in (let i_1 = (ll_0 true) in (i_0, i_1)))
  
  Types (before anf conversion): 
  val temp : int * bool
  
  Types (after anf conversion): 
  val ll_0 : 'a -> 'a
  val temp : int * bool

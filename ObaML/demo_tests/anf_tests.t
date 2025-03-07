  $ ./anf_tests.exe < manytests/typed/001fac.ml
  Types:
  val fac : int -> int
  val main : int
  
  Converted structure:
  let rec fac n = 
  	let oba0 = (( <= ) n 1) in 
  	if oba0
  	then 1
  	else 
  	let oba1 = (( - ) n 1) in 
  	let oba2 = (fac oba1) in (( * ) n oba2);;
  
  let main = 
  	let oba3 = (fac 4) in 
  	let () = (print_int oba3) in 0;;
  
  
  Types after conversions:
  val fac : int -> int
  val main : int

  $ ./anf_tests.exe < manytests/typed/002fac.ml
  Types:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Converted structure:
  let oba0 k n p = 
  	let oba2 = (( * ) p n) in (k oba2);;
  
  let rec fac_cps n k = 
  	let oba3 = (( = ) n 1) in 
  	if oba3
  	then (k 1)
  	else 
  	let oba4 = (( - ) n 1) in 
  	let oba5 = (oba0 k n) in (fac_cps oba4 oba5);;
  
  let oba1 print_int = print_int;;
  
  let main = 
  	let oba6 = (fac_cps 4 oba1) in 
  	let () = (print_int oba6) in 0;;
  
  
  Types after conversions:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  val oba0 : (int -> 'a) -> int -> int -> 'a
  val oba1 : 'a -> 'a

  $ ./anf_tests.exe < manytests/typed/003fib.ml
  Types:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  
  Converted structure:
  let rec fib_acc a b n = 
  	let oba1 = (( = ) n 1) in 
  	if oba1
  	then b
  	else 
  	let n1 = (( - ) n 1) in 
  	let ab = (( + ) a b) in (fib_acc b ab n1);;
  
  let rec fib oba0 = 
  	let oba2 = (( < ) oba0 2) in 
  	if oba2
  	then oba0
  	else 
  	let oba3 = (( - ) oba0 1) in 
  	let oba4 = (fib oba3) in 
  	let oba5 = (( - ) oba0 2) in 
  	let oba6 = (fib oba5) in (( + ) oba4 oba6);;
  
  let main = 
  	let oba7 = (fib_acc 0 1 4) in 
  	let () = (print_int oba7) in 
  	let oba8 = (fib 4) in 
  	let () = (print_int oba8) in 0;;
  
  
  Types after conversions:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int

  $ ./anf_tests.exe < manytests/typed/004manyargs.ml
  Types:
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  
  Converted structure:
  let wrap f = 
  	let oba7 = (( = ) 1 1) in 
  	if oba7
  	then f
  	else f;;
  
  let test3 a b c = 
  	let oba0 = (print_int a) in 
  	let oba1 = (print_int b) in 
  	let oba2 = (print_int c) in 0;;
  
  let test10 oba3 oba4 oba5 d e oba6 g h i j = 
  	let oba8 = (( + ) oba3 oba4) in 
  	let oba9 = (( + ) oba8 oba5) in 
  	let oba10 = (( + ) oba9 d) in 
  	let oba11 = (( + ) oba10 e) in 
  	let oba12 = (( + ) oba11 oba6) in 
  	let oba13 = (( + ) oba12 g) in 
  	let oba14 = (( + ) oba13 h) in 
  	let oba15 = (( + ) oba14 i) in (( + ) oba15 j);;
  
  let main = 
  	let rez = (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000) in 
  	let () = (print_int rez) in 
  	let temp2 = (wrap test3 1 10 100) in 0;;
  
  
  Types after conversions:
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a

  $ ./anf_tests.exe < manytests/typed/005fix.ml
  Types:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  
  Converted structure:
  let rec fix f x = 
  	let oba0 = (fix f) in (f oba0 x);;
  
  let fac self n = 
  	let oba1 = (( <= ) n 1) in 
  	if oba1
  	then 1
  	else 
  	let oba2 = (( - ) n 1) in 
  	let oba3 = (self oba2) in (( * ) n oba3);;
  
  let main = 
  	let oba4 = (fix fac 6) in 
  	let () = (print_int oba4) in 0;;
  
  
  Types after conversions:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int

  $ ./anf_tests.exe < manytests/typed/006partial.ml
  Types:
  val foo : int -> int
  val main : int
  
  Converted structure:
  let oba2 oba0 = (( + ) oba0 2);;
  
  let oba3 oba1 = (( * ) oba1 10);;
  
  let foo b = 
  	if b
  	then oba2
  	else oba3;;
  
  let foo x = 
  	let oba4 = (foo false x) in 
  	let oba5 = (foo true oba4) in 
  	let oba6 = (foo false oba5) in (foo true oba6);;
  
  let main = 
  	let oba7 = (foo 11) in 
  	let () = (print_int oba7) in 0;;
  
  
  Types after conversions:
  val foo : int -> int
  val main : int
  val oba2 : int -> int
  val oba3 : int -> int

  $ ./anf_tests.exe < manytests/typed/006partial2.ml
  Types:
  val foo : int -> int -> int -> int
  val main : int
  
  Converted structure:
  let foo a b c = 
  	let () = (print_int a) in 
  	let () = (print_int b) in 
  	let () = (print_int c) in 
  	let oba3 = (( * ) b c) in (( + ) a oba3);;
  
  let main = 
  	let oba0 = (foo 1) in 
  	let oba1 = (oba0 2) in 
  	let oba2 = (oba1 3) in 
  	let () = (print_int oba2) in 0;;
  
  
  Types after conversions:
  val foo : int -> int -> int -> int
  val main : int

  $ ./anf_tests.exe < manytests/typed/006partial3.ml
  Types:
  val foo : int -> int -> int -> unit
  val main : int
  
  Converted structure:
  let oba1 c = (print_int c);;
  
  let oba0 b = 
  	let () = (print_int b) in oba1;;
  
  let foo a = 
  	let () = (print_int a) in oba0;;
  
  let main = 
  	let () = (foo 4 8 9) in 0;;
  
  
  Types after conversions:
  val foo : int -> int -> int -> unit
  val main : int
  val oba0 : int -> int -> unit
  val oba1 : int -> unit

  $ ./anf_tests.exe < manytests/typed/007order.ml
  Types:
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  
  Converted structure:
  let _start () () a () b _c () d __ = 
  	let oba0 = (( + ) a b) in 
  	let () = (print_int oba0) in 
  	let () = (print_int __) in 
  	let oba1 = (( * ) a b) in 
  	let oba2 = (( / ) oba1 _c) in (( + ) oba2 d);;
  
  let main = 
  	let oba3 = (print_int 1) in 
  	let oba4 = (print_int 2) in 
  	let oba5 = (print_int 4) in 
  	let oba6 = (( ~- ) 1) in 
  	let oba7 = (print_int oba6) in 
  	let oba8 = (( ~- ) 555555) in 
  	let oba9 = (_start oba3 oba4 3 oba5 100 1000 oba7 10000 oba8) in (print_int oba9);;
  
  
  Types after conversions:
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ ./anf_tests.exe < manytests/typed/009let_poly.ml
  Types:
  val temp : int * bool
  
  Converted structure:
  let f x = x;;
  
  let temp = 
  	let oba0 = (f 1) in 
  	let oba1 = (f true) in (oba0, oba1);;
  
  
  Types after conversions:
  val f : 'a -> 'a
  val temp : int * bool

  $ ./anf_tests.exe << EOF
  > let rec fix f x = f (fix f) x
  > let helper f p = f p
  > let zu l =
  >  fix (fun self l -> helper (fun li x -> li (self l) x) l) l
  Types:
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val helper : ('a -> 'b) -> 'a -> 'b
  val zu : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  
  Converted structure:
  let rec fix f x = 
  	let oba5 = (fix f) in (f oba5 x);;
  
  let helper oba0 p = (oba0 p);;
  
  let oba4 oba1 self li oba2 = 
  	let oba6 = (self oba1) in (li oba6 oba2);;
  
  let oba3 self oba1 = 
  	let oba7 = (oba4 oba1 self) in (helper oba7 oba1);;
  
  let zu l = (fix oba3 l);;
  
  
  Types after conversions:
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val helper : ('a -> 'b) -> 'a -> 'b
  val oba3 : (('a -> 'b -> 'c) -> 'a) -> ('a -> 'b -> 'c) -> 'b -> 'c
  val oba4 : 'a -> ('a -> 'c) -> ('c -> 'b -> 'd) -> 'b -> 'd
  val zu : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

  $ ./anf_tests.exe << EOF
  > let rec a x = let z = (fun zu -> zu + x) in
  >     let rec a x y = a (x + z 1) y in 
  >       a;;
  Types:
  val a : int -> int -> 'a -> 'b
  
  Converted structure:
  let z x zu = (( + ) zu x);;
  
  let rec oba0 z oba1 y = 
  	let oba2 = (z 1) in 
  	let oba3 = (( + ) oba1 oba2) in (oba0 z oba3 y);;
  
  let rec a x = 
  	let oba4 = (z x) in (oba0 oba4);;
  
  
  Types after conversions:
  val a : int -> int -> 'a -> 'b
  val oba0 : (int -> int) -> int -> 'a -> 'b
  val z : int -> int -> int

  $ ./anf_tests.exe << EOF
  > let f x y = 
  >     let x z = y + z in 
  >     let y z = x 1 + z in
  >       x 1 + y 2;;
  Types:
  val f : 'a -> int -> int
  
  Converted structure:
  let oba0 y z = (( + ) y z);;
  
  let oba1 oba0 oba2 = 
  	let oba3 = (oba0 1) in (( + ) oba3 oba2);;
  
  let f x y = 
  	let oba4 = (oba0 y 1) in 
  	let oba5 = (oba0 y) in 
  	let oba6 = (oba1 oba5 2) in (( + ) oba4 oba6);;
  
  
  Types after conversions:
  val f : 'a -> int -> int
  val oba0 : int -> int -> int
  val oba1 : (int -> int) -> int -> int

  $ ./anf_tests.exe << EOF
  > let a = let x = 1 in
  > let f x = 
  >   let y = x + 1 in
  >   let g y =
  >     let x = y * 2 in
  >     let h x =
  >       let y = x - 3 in
  >       y + x
  >     in
  >     h (x + y)
  >   in
  >   g (y + x)
  > in
  > let x = f 5 in
  > let f x = x * 2 in
  > f x;;
  Types:
  val a : int
  
  Converted structure:
  let h oba3 = 
  	let oba4 = (( - ) oba3 3) in (( + ) oba4 oba3);;
  
  let g oba1 = 
  	let oba2 = (( * ) oba1 2) in 
  	let oba8 = (( + ) oba2 oba1) in (h oba8);;
  
  let f oba0 = 
  	let y = (( + ) oba0 1) in 
  	let oba9 = (( + ) y oba0) in (g oba9);;
  
  let oba6 oba7 = (( * ) oba7 2);;
  
  let a = 
  	let x = 1 in 
  	let oba5 = (f 5) in (oba6 oba5);;
  
  
  Types after conversions:
  val a : int
  val f : int -> int
  val g : int -> int
  val h : int -> int
  val oba6 : int -> int

  $ ./anf_tests.exe << EOF
  > let rec outer n =
  >  let rec middle x =
  >    let rec inner y =
  >      if y = 0 then x
  >      else
  >        let x = x + y in
  >        inner (y - 1)
  >    in
  >    inner x
  >  in
  >  if n = 0 then 0
  >  else
  >    let result = middle n in
  >    let outer = outer (n - 1) in 
  >    result + outer
  > let final_result = outer 5  
  > let outer = fun x -> x * x  
  > let final_transformed = outer final_result  
  > let () = print_int final_transformed
  Types:
  val final_result : int
  val final_transformed : int
  val outer : int -> int
  
  Converted structure:
  let rec inner x y = 
  	let oba3 = (( = ) y 0) in 
  	if oba3
  	then x
  	else 
  	let oba0 = (( + ) x y) in 
  	let oba4 = (( - ) y 1) in (inner x oba4);;
  
  let rec middle x = (inner x x);;
  
  let rec outer n = 
  	let oba5 = (( = ) n 0) in 
  	if oba5
  	then 0
  	else 
  	let result = (middle n) in 
  	let oba6 = (( - ) n 1) in 
  	let oba1 = (outer oba6) in (( + ) result oba1);;
  
  let final_result = (outer 5);;
  
  let outer oba2 = (( * ) oba2 oba2);;
  
  let final_transformed = (outer final_result);;
  
  let () = (print_int final_transformed);;
  
  
  Types after conversions:
  val final_result : int
  val final_transformed : int
  val inner : int -> int -> int
  val middle : int -> int
  val outer : int -> int

  $ ./anf_tests.exe << EOF
  > let rec oba0 n =
  >  let oba1 = n * 2 in
  >  let rec oba2 x =
  >    let oba3 = x + oba1 in
  >    let rec oba4 y =
  >      if y = 0 then oba3
  >      else
  >        let oba5 = oba3 + y in
  >        let rec oba6 z =
  >          let oba7 = z + oba5 in
  >          if z = 1 then oba7
  >          else
  >            let oba8 = oba7 * 2 in
  >            oba6 (z - 1)
  >        in
  >        oba6 y
  >    in
  >    oba4 x
  >  in
  >  if n = 0 then 0
  >  else
  >    let oba9 = oba2 n in
  >    let oba0 = oba0 (n - 1) in  
  >    oba9 + oba0
  > let oba10 = oba0 5 
  > let oba0 = fun x -> x - 7  
  > let oba11 = oba0 oba10  
  > let () = print_int oba11
  Types:
  val oba0 : int -> int
  val oba10 : int
  val oba11 : int
  
  Converted structure:
  let rec oba6 oba5 z = 
  	let oba7 = (( + ) z oba5) in 
  	let oba12 = (( = ) z 1) in 
  	if oba12
  	then oba7
  	else 
  	let oba8 = (( * ) oba7 2) in 
  	let oba13 = (( - ) z 1) in (oba6 oba5 oba13);;
  
  let rec oba4 oba3 y = 
  	let oba14 = (( = ) y 0) in 
  	if oba14
  	then oba3
  	else 
  	let oba5 = (( + ) oba3 y) in (oba6 oba5 y);;
  
  let rec oba2 oba1 x = 
  	let oba3 = (( + ) x oba1) in (oba4 oba3 x);;
  
  let rec oba0 n = 
  	let oba1 = (( * ) n 2) in 
  	let oba15 = (( = ) n 0) in 
  	if oba15
  	then 0
  	else 
  	let oba9 = (oba2 oba1 n) in 
  	let oba16 = (( - ) n 1) in 
  	let oba10 = (oba0 oba16) in (( + ) oba9 oba10);;
  
  let oba10 = (oba0 5);;
  
  let oba0 oba11 = (( - ) oba11 7);;
  
  let oba11 = (oba0 oba10);;
  
  let () = (print_int oba11);;
  
  
  Types after conversions:
  val oba0 : int -> int
  val oba10 : int
  val oba11 : int
  val oba2 : int -> int -> int
  val oba4 : int -> int -> int
  val oba6 : int -> int -> int

  $ ./anf_tests.exe << EOF
  > let foo () =
  >  print_string "This is foo"
  > let bar () =
  >  print_string "This is bar"
  > let main =
  >  let () = print_string "Main function" in 
  >  let () = foo () in 
  >  let () = bar () in 0
  > let main =
  >  print_string "Main function"
  > let () = print_int 4
  Types:
  val bar : unit -> unit
  val foo : unit -> unit
  val main : unit
  
  Converted structure:
  let foo () = (print_string "This is foo");;
  
  let bar () = (print_string "This is bar");;
  
  let main = 
  	let () = (print_string "Main function") in 
  	let () = (foo ()) in 
  	let () = (bar ()) in 0;;
  
  let main = (print_string "Main function");;
  
  let () = (print_int 4);;
  
  
  Types after conversions:
  val bar : unit -> unit
  val foo : unit -> unit
  val main : unit

  $ ./anf_tests.exe < manytests/typed/016lists.ml
  Types:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  
  Converted structure:
  let rec length xs = 
  	let oba0 = xs in 
  	let oba34 = (( = ) oba0 []) in 
  	if oba34
  	then 0
  	else 
  	let oba35 = (#list_length_getter# oba0) in 
  	let oba36 = (( >= ) oba35 1) in 
  	if oba36
  	then 
  	let h = (#list_head_getter# oba0) in 
  	let tl = (#list_tail_getter# oba0) in 
  	let oba37 = (length tl) in (( + ) 1 oba37)
  	else (#matching_failed# ());;
  
  let rec helper acc oba1 = 
  	let oba2 = oba1 in 
  	let oba38 = (( = ) oba2 []) in 
  	if oba38
  	then acc
  	else 
  	let oba39 = (#list_length_getter# oba2) in 
  	let oba40 = (( >= ) oba39 1) in 
  	if oba40
  	then 
  	let oba3 = (#list_head_getter# oba2) in 
  	let oba4 = (#list_tail_getter# oba2) in 
  	let oba41 = (( + ) acc 1) in (helper oba41 oba4)
  	else (#matching_failed# ());;
  
  let length_tail = (helper 0);;
  
  let rec map f oba5 = 
  	let oba6 = oba5 in 
  	let oba42 = (( = ) oba6 []) in 
  	if oba42
  	then []
  	else 
  	let oba43 = (#list_length_getter# oba6) in 
  	let oba44 = (( = ) oba43 1) in 
  	if oba44
  	then 
  	let a = (#list_head_getter# oba6) in 
  	let oba45 = (f a) in oba45 :: []
  	else 
  	let oba46 = (#list_length_getter# oba6) in 
  	let oba47 = (( = ) oba46 2) in 
  	if oba47
  	then 
  	let oba7 = (#list_head_getter# oba6) in 
  	let oba48 = (#list_tail_getter# oba6) in 
  	let b = (#list_head_getter# oba48) in 
  	let oba49 = (f oba7) in 
  	let oba50 = (f b) in 
  	let oba51 = oba50 :: [] in oba49 :: oba51
  	else 
  	let oba52 = (#list_length_getter# oba6) in 
  	let oba53 = (( = ) oba52 3) in 
  	if oba53
  	then 
  	let oba8 = (#list_head_getter# oba6) in 
  	let oba54 = (#list_tail_getter# oba6) in 
  	let oba9 = (#list_head_getter# oba54) in 
  	let oba55 = (#list_tail_getter# oba6) in 
  	let oba56 = (#list_tail_getter# oba55) in 
  	let c = (#list_head_getter# oba56) in 
  	let oba57 = (f oba8) in 
  	let oba58 = (f oba9) in 
  	let oba59 = (f c) in 
  	let oba60 = oba59 :: [] in 
  	let oba61 = oba58 :: oba60 in oba57 :: oba61
  	else 
  	let oba62 = (#list_length_getter# oba6) in 
  	let oba63 = (( >= ) oba62 4) in 
  	if oba63
  	then 
  	let oba10 = (#list_head_getter# oba6) in 
  	let oba64 = (#list_tail_getter# oba6) in 
  	let oba11 = (#list_head_getter# oba64) in 
  	let oba65 = (#list_tail_getter# oba6) in 
  	let oba66 = (#list_tail_getter# oba65) in 
  	let oba12 = (#list_head_getter# oba66) in 
  	let oba67 = (#list_tail_getter# oba6) in 
  	let oba68 = (#list_tail_getter# oba67) in 
  	let oba69 = (#list_tail_getter# oba68) in 
  	let d = (#list_head_getter# oba69) in 
  	let oba70 = (#list_tail_getter# oba6) in 
  	let oba71 = (#list_tail_getter# oba70) in 
  	let oba72 = (#list_tail_getter# oba71) in 
  	let oba13 = (#list_tail_getter# oba72) in 
  	let oba73 = (f oba10) in 
  	let oba74 = (f oba11) in 
  	let oba75 = (f oba12) in 
  	let oba76 = (f d) in 
  	let oba77 = (map f oba13) in 
  	let oba78 = oba76 :: oba77 in 
  	let oba79 = oba75 :: oba78 in 
  	let oba80 = oba74 :: oba79 in oba73 :: oba80
  	else (#matching_failed# ());;
  
  let rec append oba14 ys = 
  	let oba15 = oba14 in 
  	let oba81 = (( = ) oba15 []) in 
  	if oba81
  	then ys
  	else 
  	let oba82 = (#list_length_getter# oba15) in 
  	let oba83 = (( >= ) oba82 1) in 
  	if oba83
  	then 
  	let x = (#list_head_getter# oba15) in 
  	let oba16 = (#list_tail_getter# oba15) in 
  	let oba84 = (append oba16 ys) in x :: oba84
  	else (#matching_failed# ());;
  
  let rec oba17 oba18 = 
  	let oba19 = oba18 in 
  	let oba85 = (( = ) oba19 []) in 
  	if oba85
  	then []
  	else 
  	let oba86 = (#list_length_getter# oba19) in 
  	let oba87 = (( >= ) oba86 1) in 
  	if oba87
  	then 
  	let oba20 = (#list_head_getter# oba19) in 
  	let oba21 = (#list_tail_getter# oba19) in 
  	let oba88 = (oba17 oba21) in (append oba20 oba88)
  	else (#matching_failed# ());;
  
  let concat = oba17;;
  
  let rec iter oba22 oba23 = 
  	let oba24 = oba23 in 
  	let oba89 = (( = ) oba24 []) in 
  	if oba89
  	then ()
  	else 
  	let oba90 = (#list_length_getter# oba24) in 
  	let oba91 = (( >= ) oba90 1) in 
  	if oba91
  	then 
  	let oba25 = (#list_head_getter# oba24) in 
  	let oba26 = (#list_tail_getter# oba24) in 
  	let () = (oba22 oba25) in (iter oba22 oba26)
  	else (#matching_failed# ());;
  
  let oba33 oba30 oba32 = (oba30, oba32);;
  
  let rec cartesian oba27 oba28 = 
  	let oba29 = oba27 in 
  	let oba92 = (( = ) oba29 []) in 
  	if oba92
  	then []
  	else 
  	let oba93 = (#list_length_getter# oba29) in 
  	let oba94 = (( >= ) oba93 1) in 
  	if oba94
  	then 
  	let oba30 = (#list_head_getter# oba29) in 
  	let oba31 = (#list_tail_getter# oba29) in 
  	let oba95 = (oba33 oba30) in 
  	let oba96 = (map oba95 oba28) in 
  	let oba97 = (cartesian oba31 oba28) in (append oba96 oba97)
  	else (#matching_failed# ());;
  
  let main = 
  	let oba98 = 3 :: [] in 
  	let oba99 = 2 :: oba98 in 
  	let oba100 = 1 :: oba99 in 
  	let () = (iter print_int oba100) in 
  	let oba101 = 2 :: [] in 
  	let oba102 = 1 :: oba101 in 
  	let oba103 = 4 :: [] in 
  	let oba104 = 3 :: oba103 in 
  	let oba105 = 2 :: oba104 in 
  	let oba106 = 1 :: oba105 in 
  	let oba107 = (cartesian oba102 oba106) in 
  	let oba108 = (length oba107) in 
  	let () = (print_int oba108) in 0;;
  
  
  Types after conversions:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val helper : int -> 'a list -> int
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val oba17 : 'a list list -> 'a list
  val oba33 : 'a -> 'b -> 'a * 'b

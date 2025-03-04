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
  let oba1 k n p = 
  	let oba3 = (( * ) p n) in (k oba3);;
  
  let rec fac_cps n k = 
  	let oba4 = (( = ) n 1) in 
  	if oba4
  	then (k 1)
  	else 
  	let oba6 = (oba1 k n) in 
  	let oba5 = (( - ) n 1) in (fac_cps oba5 oba6);;
  
  let oba2 oba0 = oba0;;
  
  let main = 
  	let oba7 = (fac_cps 4 oba2) in 
  	let () = (print_int oba7) in 0;;
  
  
  Types after conversions:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  val oba1 : (int -> 'a) -> int -> int -> 'a
  val oba2 : 'a -> 'a

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
  	let oba5 = (( - ) oba0 2) in 
  	let oba6 = (fib oba5) in 
  	let oba3 = (( - ) oba0 1) in 
  	let oba4 = (fib oba3) in (( + ) oba4 oba6);;
  
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
  let oba3 oba0 = (( + ) oba0 2);;
  
  let oba4 oba1 = (( * ) oba1 10);;
  
  let foo b = 
  	if b
  	then oba3
  	else oba4;;
  
  let oba2 x = 
  	let oba5 = (foo false x) in 
  	let oba6 = (foo true oba5) in 
  	let oba7 = (foo false oba6) in (foo true oba7);;
  
  let main = 
  	let oba8 = (oba2 11) in 
  	let () = (print_int oba8) in 0;;
  
  
  Types after conversions:
  val foo : bool -> int -> int
  val main : int
  val oba2 : int -> int
  val oba3 : int -> int
  val oba4 : int -> int

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
  	let oba8 = (( ~- ) 555555) in 
  	let oba6 = (( ~- ) 1) in 
  	let oba7 = (print_int oba6) in 
  	let oba5 = (print_int 4) in 
  	let oba4 = (print_int 2) in 
  	let oba3 = (print_int 1) in 
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
  	let oba5 = (oba0 y) in 
  	let oba6 = (oba1 oba5 2) in 
  	let oba4 = (oba0 y 1) in (( + ) oba4 oba6);;
  
  
  Types after conversions:
  val f : 'a -> int -> int
  val oba0 : int -> int -> int
  val oba1 : (int -> int) -> int -> int
  $ ./anf_tests.exe << EOF 
  > let rec fib n k =
  >   if n < 2
  >   then k n
  >   else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)))
  > 
  > let main = print_int(fib 6 (fun x -> x))
  Types:
  val fib : int -> (int -> 'a) -> 'a
  val main : unit
  
  Converted structure:
  let oba1 a k b = 
  	let oba3 = (( + ) a b) in (k oba3);;
  
  let oba0 fib k n a = 
  	let oba5 = (oba1 a k) in 
  	let oba4 = (( - ) n 2) in (fib oba4 oba5);;
  
  let rec fib n k = 
  	let oba6 = (( < ) n 2) in 
  	if oba6
  	then (k n)
  	else 
  	let oba8 = (oba0 fib k n) in 
  	let oba7 = (( - ) n 1) in (fib oba7 oba8);;
  
  let oba2 x = x;;
  
  let main = 
  	let oba9 = (fib 6 oba2) in (print_int oba9);;
  
  
  Types after conversions:
  val fib : int -> (int -> 'a) -> 'a
  val main : unit
  val oba0 : (int -> (int -> 'a) -> 'b) -> (int -> 'a) -> int -> int -> 'b
  val oba1 : int -> (int -> 'a) -> int -> 'a
  val oba2 : 'a -> 'a

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
  	let oba4 = (( = ) y 0) in 
  	if oba4
  	then x
  	else 
  	let oba0 = (( + ) x y) in 
  	let oba5 = (( - ) y 1) in (inner x oba5);;
  
  let rec middle x = (inner x x);;
  
  let rec outer n = 
  	let oba6 = (( = ) n 0) in 
  	if oba6
  	then 0
  	else 
  	let result = (middle n) in 
  	let oba7 = (( - ) n 1) in 
  	let oba1 = (outer oba7) in (( + ) result oba1);;
  
  let final_result = (outer 5);;
  
  let oba2 oba3 = (( * ) oba3 oba3);;
  
  let final_transformed = (oba2 final_result);;
  
  let () = (print_int final_transformed);;
  
  
  Types after conversions:
  val final_result : int
  val final_transformed : int
  val inner : int -> int -> int
  val middle : int -> int
  val oba2 : int -> int
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
  	let oba15 = (( = ) z 1) in 
  	if oba15
  	then oba7
  	else 
  	let oba8 = (( * ) oba7 2) in 
  	let oba16 = (( - ) z 1) in (oba6 oba5 oba16);;
  
  let rec oba4 oba3 y = 
  	let oba17 = (( = ) y 0) in 
  	if oba17
  	then oba3
  	else 
  	let oba5 = (( + ) oba3 y) in (oba6 oba5 y);;
  
  let rec oba2 oba1 x = 
  	let oba3 = (( + ) x oba1) in (oba4 oba3 x);;
  
  let rec oba0 n = 
  	let oba1 = (( * ) n 2) in 
  	let oba18 = (( = ) n 0) in 
  	if oba18
  	then 0
  	else 
  	let oba9 = (oba2 oba1 n) in 
  	let oba19 = (( - ) n 1) in 
  	let oba10 = (oba0 oba19) in (( + ) oba9 oba10);;
  
  let oba11 = (oba0 5);;
  
  let oba12 oba13 = (( - ) oba13 7);;
  
  let oba14 = (oba12 oba11);;
  
  let () = (print_int oba14);;
  
  
  Types after conversions:
  val oba0 : int -> int
  val oba11 : int
  val oba12 : int -> int
  val oba14 : int
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
  
  let oba0 = (print_string "Main function");;
  
  let () = (print_int 4);;
  
  
  Types after conversions:
  val bar : unit -> unit
  val foo : unit -> unit
  val main : int
  val oba0 : unit

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
  	let pat0 = xs in 
  	let oba27 = (( = ) pat0 []) in 
  	if oba27
  	then 0
  	else 
  	let oba28 = (list_length_getter pat0) in 
  	let oba29 = (( >= ) oba28 1) in 
  	if oba29
  	then 
  	let h = (list_head_getter pat0) in 
  	let tl = (list_tail_getter pat0) in 
  	let oba30 = (length tl) in (( + ) 1 oba30)
  	else (matching_failed ());;
  
  let rec helper acc oba0 = 
  	let pat0 = oba0 in 
  	let oba31 = (( = ) pat0 []) in 
  	if oba31
  	then acc
  	else 
  	let oba32 = (list_length_getter pat0) in 
  	let oba33 = (( >= ) oba32 1) in 
  	if oba33
  	then 
  	let oba1 = (list_head_getter pat0) in 
  	let oba2 = (list_tail_getter pat0) in 
  	let oba34 = (( + ) acc 1) in (helper oba34 oba2)
  	else (matching_failed ());;
  
  let length_tail = (helper 0);;
  
  let rec map f oba3 = 
  	let pat0 = oba3 in 
  	let oba35 = (( = ) pat0 []) in 
  	if oba35
  	then []
  	else 
  	let oba36 = (list_length_getter pat0) in 
  	let oba37 = (( = ) oba36 1) in 
  	if oba37
  	then 
  	let a = (list_head_getter pat0) in 
  	let oba38 = (f a) in oba38 :: []
  	else 
  	let oba39 = (list_length_getter pat0) in 
  	let oba40 = (( = ) oba39 2) in 
  	if oba40
  	then 
  	let oba4 = (list_head_getter pat0) in 
  	let oba41 = (list_tail_getter pat0) in 
  	let b = (list_head_getter oba41) in 
  	let oba42 = (f oba4) in 
  	let oba43 = (f b) in 
  	let oba44 = oba43 :: [] in oba42 :: oba44
  	else 
  	let oba45 = (list_length_getter pat0) in 
  	let oba46 = (( = ) oba45 3) in 
  	if oba46
  	then 
  	let oba5 = (list_head_getter pat0) in 
  	let oba47 = (list_tail_getter pat0) in 
  	let oba6 = (list_head_getter oba47) in 
  	let oba48 = (list_tail_getter pat0) in 
  	let oba49 = (list_tail_getter oba48) in 
  	let c = (list_head_getter oba49) in 
  	let oba50 = (f oba5) in 
  	let oba51 = (f oba6) in 
  	let oba52 = (f c) in 
  	let oba53 = oba52 :: [] in 
  	let oba54 = oba51 :: oba53 in oba50 :: oba54
  	else 
  	let oba55 = (list_length_getter pat0) in 
  	let oba56 = (( >= ) oba55 4) in 
  	if oba56
  	then 
  	let oba7 = (list_head_getter pat0) in 
  	let oba57 = (list_tail_getter pat0) in 
  	let oba8 = (list_head_getter oba57) in 
  	let oba58 = (list_tail_getter pat0) in 
  	let oba59 = (list_tail_getter oba58) in 
  	let oba9 = (list_head_getter oba59) in 
  	let oba60 = (list_tail_getter pat0) in 
  	let oba61 = (list_tail_getter oba60) in 
  	let oba62 = (list_tail_getter oba61) in 
  	let d = (list_head_getter oba62) in 
  	let oba63 = (list_tail_getter pat0) in 
  	let oba64 = (list_tail_getter oba63) in 
  	let oba65 = (list_tail_getter oba64) in 
  	let oba10 = (list_tail_getter oba65) in 
  	let oba66 = (f oba7) in 
  	let oba67 = (f oba8) in 
  	let oba68 = (f oba9) in 
  	let oba69 = (f d) in 
  	let oba70 = (map f oba10) in 
  	let oba71 = oba69 :: oba70 in 
  	let oba72 = oba68 :: oba71 in 
  	let oba73 = oba67 :: oba72 in oba66 :: oba73
  	else (matching_failed ());;
  
  let rec append oba11 ys = 
  	let pat0 = oba11 in 
  	let oba74 = (( = ) pat0 []) in 
  	if oba74
  	then ys
  	else 
  	let oba75 = (list_length_getter pat0) in 
  	let oba76 = (( >= ) oba75 1) in 
  	if oba76
  	then 
  	let x = (list_head_getter pat0) in 
  	let oba12 = (list_tail_getter pat0) in 
  	let oba77 = (append oba12 ys) in x :: oba77
  	else (matching_failed ());;
  
  let rec oba13 oba14 = 
  	let pat0 = oba14 in 
  	let oba78 = (( = ) pat0 []) in 
  	if oba78
  	then []
  	else 
  	let oba79 = (list_length_getter pat0) in 
  	let oba80 = (( >= ) oba79 1) in 
  	if oba80
  	then 
  	let oba15 = (list_head_getter pat0) in 
  	let oba16 = (list_tail_getter pat0) in 
  	let oba81 = (oba13 oba16) in (append oba15 oba81)
  	else (matching_failed ());;
  
  let concat = oba13;;
  
  let rec iter oba17 oba18 = 
  	let pat0 = oba18 in 
  	let oba82 = (( = ) pat0 []) in 
  	if oba82
  	then ()
  	else 
  	let oba83 = (list_length_getter pat0) in 
  	let oba84 = (( >= ) oba83 1) in 
  	if oba84
  	then 
  	let oba19 = (list_head_getter pat0) in 
  	let oba20 = (list_tail_getter pat0) in 
  	let () = (oba17 oba19) in (iter oba17 oba20)
  	else (matching_failed ());;
  
  let oba26 oba23 oba25 = (oba23, oba25);;
  
  let rec cartesian oba21 oba22 = 
  	let pat0 = oba21 in 
  	let oba85 = (( = ) pat0 []) in 
  	if oba85
  	then []
  	else 
  	let oba86 = (list_length_getter pat0) in 
  	let oba87 = (( >= ) oba86 1) in 
  	if oba87
  	then 
  	let oba23 = (list_head_getter pat0) in 
  	let oba24 = (list_tail_getter pat0) in 
  	let oba90 = (cartesian oba24 oba22) in 
  	let oba88 = (oba26 oba23) in 
  	let oba89 = (map oba88 oba22) in (append oba89 oba90)
  	else (matching_failed ());;
  
  let main = 
  	let oba91 = 3 :: [] in 
  	let oba92 = 2 :: oba91 in 
  	let oba93 = 1 :: oba92 in 
  	let () = (iter print_int oba93) in 
  	let oba96 = 4 :: [] in 
  	let oba97 = 3 :: oba96 in 
  	let oba98 = 2 :: oba97 in 
  	let oba99 = 1 :: oba98 in 
  	let oba94 = 2 :: [] in 
  	let oba95 = 1 :: oba94 in 
  	let oba100 = (cartesian oba95 oba99) in 
  	let oba101 = (length oba100) in 
  	let () = (print_int oba101) in 0;;
  
  
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
  val oba13 : 'a list list -> 'a list
  val oba26 : 'a -> 'b -> 'a * 'b

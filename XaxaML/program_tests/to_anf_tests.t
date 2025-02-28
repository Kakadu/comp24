# simple
  $ ./run_to_anf.exe << EOF
  > let sum a b = a + b
  > let inc x = sum x 1
  > let t = sum (sum 1 2) (inc 2)
  Types before modifications:
  val inc : int -> int
  val sum : int -> int -> int
  val t : int
  
  Types after modifications:
  val inc : int -> int
  val sum : int -> int -> int
  val t : int
  
  Modified ast:
  let sum a b =
    ( + ) a b
  let inc x =
    sum x 1
  let t =
    let a0 = sum 1 2 in
    let a1 = inc 2 in
    sum a0 a1
  $ ./run_to_anf.exe << EOF
  > let f a b = 
  >   let inc x = 
  >     let inc2 y = y + 1 in inc2 x
  >   in
  >   let sum a b = a + b in
  >   inc (sum a b) 
  Types before modifications:
  val f : int -> int -> int
  
  Types after modifications:
  val a2 : int -> int
  val a3 : int -> int
  val a4 : int -> int -> int
  val f : int -> int -> int
  
  Modified ast:
  let a2 y =
    ( + ) y 1
  let a3 x =
    a2 x
  let a4 a0 a1 =
    ( + ) a0 a1
  let f a b =
    let a5 = a4 a b in
    a3 a5
  $ ./run_to_anf.exe << EOF
  > let f = 
  >   let rec is_even n =
  >   if n = 0 then true
  >   else is_odd (n - 1)
  > 
  >   and is_odd n =
  >   if n = 0 then false
  >   else is_even (n - 1)
  > in (is_even 4, is_odd 5)
  Types before modifications:
  val f : bool * bool
  
  Types after modifications:
  val a0 : int -> bool
  val a1 : int -> bool
  val f : bool * bool
  
  Modified ast:
  let rec a0 n =
    let a2 = ( = ) n 0 in
    if a2
    then false
    else let a3 = ( - ) n 1 in
      a1 a3
    
  and a1 n =
    let a4 = ( = ) n 0 in
    if a4
    then true
    else let a5 = ( - ) n 1 in
      a0 a5
    
  let f =
    let a6 = a1 4 in
    let a7 = a0 5 in
    (a6, a7)

  $ ./run_to_anf.exe << EOF
  > let rec y x =
  >   let a z = y x + z in
  >   let b x = a 5 + x in
  >   b 5
  Types before modifications:
  val y : 'a -> int
  
  Types after modifications:
  val a1 : 'a -> ('a -> int) -> int -> int
  val a2 : (int -> int) -> int -> int
  val y : 'a -> int
  
  Modified ast:
  let a1 x y z =
    let a3 = y x in
    ( + ) a3 z
  let a2 a a0 =
    let a4 = a 5 in
    ( + ) a4 a0
  let rec y x =
    let a5 = a1 x y in
    a2 a5 5

  $ ./run_to_anf.exe << EOF
  > let f a b =
  >   let a x = b + x in
  >   let b x = a 1 + x in
  >   a 1 + b 2;;
  Types before modifications:
  val f : 'a -> int -> int
  
  Types after modifications:
  val a2 : int -> int -> int
  val a3 : (int -> int) -> int -> int
  val f : 'a -> int -> int
  
  Modified ast:
  let a2 b x =
    ( + ) b x
  let a3 a0 x =
    let a4 = a0 1 in
    ( + ) a4 x
  let f a b =
    let a5 = a2 b 1 in
    let a7 = a2 b in
    let a6 = a3 a7 2 in
    ( + ) a5 a6
  $ ./run_to_anf.exe << EOF
  > let abc = 
  >   let (a, (b, c)) = (1, (2, 3)) in 
  >   let (d::e) = [1; 2; 3] in
  >   let f (x, y) = x + y in 
  >   0
  Types before modifications:
  val abc : int
  
  Types after modifications:
  val a3 : 'a -> int
  val abc : int
  
  Modified ast:
  let a3 a0 =
    let x = #unpack_tuple a0 0 in
    let y = #unpack_tuple a0 1 in
    ( + ) x y
  let abc =
    let a4 = (2, 3) in
    let a2 = (1, a4) in
    let a = #unpack_tuple a2 0 in
    let a5 = #unpack_tuple a2 1 in
    let b = #unpack_tuple a5 0 in
    let a6 = #unpack_tuple a2 1 in
    let c = #unpack_tuple a6 1 in
    let a8 = (3 :: []) in
    let a7 = (2 :: a8) in
    let a1 = (1 :: a7) in
    let a10 = #list_length a1 in
    let a9 = ( > ) a10 0 in
    if a9
    then let d = #list_hd a1 in
      let e = #list_tl a1 in
      0
    else #match_failure
    

  $ ./run_to_anf.exe < manytests/typed/001fac.ml
  Types before modifications:
  val fac : int -> int
  val main : int
  
  Types after modifications:
  val fac : int -> int
  val main : int
  
  Modified ast:
  let rec fac n =
    let a0 = ( <= ) n 1 in
    if a0
    then 1
    else let a2 = ( - ) n 1 in
      let a1 = fac a2 in
      ( * ) n a1
    
  let main =
    let a3 = fac 4 in
    let () = print_int a3 in
    0

  $ ./run_to_anf.exe < manytests/typed/002fac.ml
  Types before modifications:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Types after modifications:
  val a1 : (int -> 'a) -> int -> int -> 'a
  val a2 : 'a -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Modified ast:
  let a1 k n p =
    let a3 = ( * ) p n in
    k a3
  let rec fac_cps n k =
    let a4 = ( = ) n 1 in
    if a4
    then k 1
    else let a5 = ( - ) n 1 in
      let a6 = a1 k n in
      fac_cps a5 a6
    
  let a2 a0 =
    a0
  let main =
    let a7 = fac_cps 4 a2 in
    let () = print_int a7 in
    0

  $ ./run_to_anf.exe < manytests/typed/003fib.ml
  Types before modifications:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  
  Types after modifications:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  
  Modified ast:
  let rec fib_acc a b n =
    let a0 = ( = ) n 1 in
    if a0
    then b
    else let n1 = ( - ) n 1 in
      let ab = ( + ) a b in
      fib_acc b ab n1
    
  let rec fib n =
    let a1 = ( < ) n 2 in
    if a1
    then n
    else let a3 = ( - ) n 1 in
      let a2 = fib a3 in
      let a5 = ( - ) n 2 in
      let a4 = fib a5 in
      ( + ) a2 a4
    
  let main =
    let a6 = fib_acc 0 1 4 in
    let () = print_int a6 in
    let a7 = fib 4 in
    let () = print_int a7 in
    0

  $ ./run_to_anf.exe < manytests/typed/004manyargs.ml
  Types before modifications:
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  
  Types after modifications:
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  
  Modified ast:
  let wrap f =
    let a3 = ( = ) 1 1 in
    if a3
    then f
    else f
    
  let test3 a b c =
    let a0 = print_int a in
    let a1 = print_int b in
    let a2 = print_int c in
    0
  let test10 a b c d e f g h i j =
    let a11 = ( + ) a b in
    let a10 = ( + ) a11 c in
    let a9 = ( + ) a10 d in
    let a8 = ( + ) a9 e in
    let a7 = ( + ) a8 f in
    let a6 = ( + ) a7 g in
    let a5 = ( + ) a6 h in
    let a4 = ( + ) a5 i in
    ( + ) a4 j
  let main =
    let rez = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
    let () = print_int rez in
    let temp2 = wrap test3 1 10 100 in
    0

  $ ./run_to_anf.exe < manytests/typed/005fix.ml
  Types before modifications:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  
  Types after modifications:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  
  Modified ast:
  let rec fix f x =
    let a0 = fix f in
    f a0 x
  let fac self n =
    let a1 = ( <= ) n 1 in
    if a1
    then 1
    else let a3 = ( - ) n 1 in
      let a2 = self a3 in
      ( * ) n a2
    
  let main =
    let a4 = fix fac 6 in
    let () = print_int a4 in
    0

Here second declaration of function "foo" was renamed to "#2" during modifications
  $ ./run_to_anf.exe < manytests/typed/006partial.ml
  Types before modifications:
  val foo : int -> int
  val main : int
  
  Types after modifications:
  val a2 : int -> int
  val a3 : int -> int
  val a4 : int -> int
  val foo : bool -> int -> int
  val main : int
  
  Modified ast:
  let a3 a0 =
    ( + ) a0 2
  let a4 a1 =
    ( * ) a1 10
  let foo b =
    if b
    then a3
    else a4
    
  let a2 x =
    let a7 = foo false x in
    let a6 = foo true a7 in
    let a5 = foo false a6 in
    foo true a5
  let main =
    let a8 = a2 11 in
    let () = print_int a8 in
    0

  $ ./run_to_anf.exe < manytests/typed/006partial2.ml
  Types before modifications:
  val foo : int -> int -> int -> int
  val main : int
  
  Types after modifications:
  val foo : int -> int -> int -> int
  val main : int
  
  Modified ast:
  let foo a b c =
    let () = print_int a in
    let () = print_int b in
    let () = print_int c in
    let a3 = ( * ) b c in
    ( + ) a a3
  let main =
    let a0 = foo 1 in
    let a1 = a0 2 in
    let a2 = a1 3 in
    let () = print_int a2 in
    0

  $ ./run_to_anf.exe < manytests/typed/006partial3.ml
  Types before modifications:
  val foo : int -> int -> int -> unit
  val main : int
  
  Types after modifications:
  val a0 : int -> int -> unit
  val a1 : int -> unit
  val foo : int -> int -> int -> unit
  val main : int
  
  Modified ast:
  let a1 c =
    print_int c
  let a0 b =
    let () = print_int b in
    a1
  let foo a =
    let () = print_int a in
    a0
  let main =
    let () = foo 4 8 9 in
    0

  $ ./run_to_anf.exe < manytests/typed/007order.ml
  Types before modifications:
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  
  Types after modifications:
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  
  Modified ast:
  let _start () () a () b _c () d __ =
    let a0 = ( + ) a b in
    let () = print_int a0 in
    let () = print_int __ in
    let a2 = ( * ) a b in
    let a1 = ( / ) a2 _c in
    ( + ) a1 d
  let main =
    let a4 = print_int 1 in
    let a5 = print_int 2 in
    let a6 = print_int 4 in
    let a8 = ( ~- ) 1 in
    let a7 = print_int a8 in
    let a9 = ( ~- ) 555555 in
    let a3 = _start a4 a5 3 a6 100 1000 a7 10000 a9 in
    print_int a3

The explicitly specified types were erased after type inference 
  $ ./run_to_anf.exe < manytests/typed/008ascription.ml
  Types before modifications:
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int
  
  Types after modifications:
  val a0 : int -> bool -> int
  val a1 : int -> bool
  val addi : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val main : int
  
  Modified ast:
  let addi f g x =
    let a2 = g x in
    f x a2
  let a0 x b =
    if b
    then ( + ) x 1
    else ( * ) x 2
    
  let a1 _start =
    let a3 = ( / ) _start 2 in
    ( = ) a3 0
  let main =
    let a4 = addi a0 a1 4 in
    let () = print_int a4 in
    0

  $ ./run_to_anf.exe < manytests/typed/009let_poly.ml
  Types before modifications:
  val temp : int * bool
  
  Types after modifications:
  val a0 : 'a -> 'a
  val temp : int * bool
  
  Modified ast:
  let a0 x =
    x
  let temp =
    let a1 = a0 1 in
    let a2 = a0 true in
    (a1, a2)
Some types are different due to the impossibility to type the function "#unpack_tuple" correctly 
(see inferencer.ml for details )
  $ ./run_to_anf.exe < manytests/typed/015tuples.ml
  Types before modifications:
  val feven : 'a * (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val fodd : (int -> int) * 'a -> int -> int
  val main : int
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : (int -> int) * (int -> int)
  
  Types after modifications:
  val a1 : ('a -> 'b) -> 'a -> ('c -> 'd) * ('c -> 'd)
  val a2 : 'a -> ('a -> 'b) -> ('b -> 'c -> 'd) -> 'c -> 'd
  val feven : 'a -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : 'a -> ('b -> 'c) * ('b -> 'c)
  val fodd : 'a -> int -> int
  val main : int
  val map : ('a -> 'b) -> 'c -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : ('a -> 'b) * ('a -> 'b)
  
  Modified ast:
  let rec fix f x =
    let a3 = fix f in
    f a3 x
  let map f p =
    let a = #unpack_tuple p 0 in
    let b = #unpack_tuple p 1 in
    let a4 = f a in
    let a5 = f b in
    (a4, a5)
  let a2 a0 self li x =
    let a6 = self a0 in
    li a6 x
  let a1 self a0 =
    let a7 = a2 a0 self in
    map a7 a0
  let fixpoly l =
    fix a1 l
  let feven p n =
    let e = #unpack_tuple p 0 in
    let o = #unpack_tuple p 1 in
    let a8 = ( == ) n 0 in
    if a8
    then 1
    else let a9 = ( - ) n 1 in
      o a9
    
  let fodd p n =
    let e = #unpack_tuple p 0 in
    let o = #unpack_tuple p 1 in
    let a10 = ( == ) n 0 in
    if a10
    then 0
    else let a11 = ( - ) n 1 in
      e a11
    
  let tie =
    let a12 = (feven, fodd) in
    fixpoly a12
  let rec modd n =
    let a13 = ( = ) n 0 in
    if a13
    then 1
    else let a14 = ( - ) n 1 in
      meven a14
    
  and meven n =
    let a15 = ( = ) n 0 in
    if a15
    then 1
    else let a16 = ( - ) n 1 in
      modd a16
    
  let main =
    let a17 = modd 1 in
    let () = print_int a17 in
    let a18 = meven 2 in
    let () = print_int a18 in
    let even = #unpack_tuple tie 0 in
    let odd = #unpack_tuple tie 1 in
    let a19 = odd 3 in
    let () = print_int a19 in
    let a20 = even 4 in
    let () = print_int a20 in
    0

  $ ./run_to_anf.exe < manytests/typed/016lists.ml
  Types before modifications:
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  
  Types after modifications:
  val a1 : int -> 'a list -> int
  val a2 : 'a list list -> 'a list
  val a3 : 'a -> 'b -> 'a * 'b
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val main : int
  val map : ('a -> 'b) -> 'a list -> 'b list
  
  Modified ast:
  let rec length xs =
    let a4 = ( = ) xs [] in
    if a4
    then 0
    else let a6 = #list_length xs in
      let a5 = ( > ) a6 0 in
      if a5
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let a7 = length tl in
        ( + ) 1 a7
      else #match_failure
      
    
  let rec a1 acc xs =
    let a8 = ( = ) xs [] in
    if a8
    then acc
    else let a10 = #list_length xs in
      let a9 = ( > ) a10 0 in
      if a9
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let a11 = ( + ) acc 1 in
        a1 a11 tl
      else #match_failure
      
    
  let length_tail =
    a1 0
  let rec map f xs =
    let a12 = ( = ) xs [] in
    if a12
    then []
    else let a15 = #list_length xs in
      let a14 = ( > ) a15 0 in
      let a17 = #list_tl xs in
      let a16 = ( = ) a17 [] in
      let a13 = ( && ) a14 a16 in
      if a13
      then let a = #list_hd xs in
        let a18 = f a in
        (a18 :: [])
      else let a21 = #list_length xs in
        let a20 = ( > ) a21 1 in
        let a24 = #list_tl xs in
        let a23 = #list_tl a24 in
        let a22 = ( = ) a23 [] in
        let a19 = ( && ) a20 a22 in
        if a19
        then let a = #list_hd xs in
          let a25 = #list_tl xs in
          let b = #list_hd a25 in
          let a26 = f a in
          let a28 = f b in
          let a27 = (a28 :: []) in
          (a26 :: a27)
        else let a31 = #list_length xs in
          let a30 = ( > ) a31 2 in
          let a35 = #list_tl xs in
          let a34 = #list_tl a35 in
          let a33 = #list_tl a34 in
          let a32 = ( = ) a33 [] in
          let a29 = ( && ) a30 a32 in
          if a29
          then let a = #list_hd xs in
            let a36 = #list_tl xs in
            let b = #list_hd a36 in
            let a38 = #list_tl xs in
            let a37 = #list_tl a38 in
            let c = #list_hd a37 in
            let a39 = f a in
            let a41 = f b in
            let a43 = f c in
            let a42 = (a43 :: []) in
            let a40 = (a41 :: a42) in
            (a39 :: a40)
          else let a45 = #list_length xs in
            let a44 = ( > ) a45 3 in
            if a44
            then let a = #list_hd xs in
              let a46 = #list_tl xs in
              let b = #list_hd a46 in
              let a48 = #list_tl xs in
              let a47 = #list_tl a48 in
              let c = #list_hd a47 in
              let a51 = #list_tl xs in
              let a50 = #list_tl a51 in
              let a49 = #list_tl a50 in
              let d = #list_hd a49 in
              let a54 = #list_tl xs in
              let a53 = #list_tl a54 in
              let a52 = #list_tl a53 in
              let tl = #list_tl a52 in
              let a55 = f a in
              let a57 = f b in
              let a59 = f c in
              let a61 = f d in
              let a62 = map f tl in
              let a60 = (a61 :: a62) in
              let a58 = (a59 :: a60) in
              let a56 = (a57 :: a58) in
              (a55 :: a56)
            else #match_failure
            
          
        
      
    
  let rec append xs ys =
    let a63 = ( = ) xs [] in
    if a63
    then ys
    else let a65 = #list_length xs in
      let a64 = ( > ) a65 0 in
      if a64
      then let x = #list_hd xs in
        let a0 = #list_tl xs in
        let a66 = append a0 ys in
        (x :: a66)
      else #match_failure
      
    
  let rec a2 xs =
    let a67 = ( = ) xs [] in
    if a67
    then []
    else let a69 = #list_length xs in
      let a68 = ( > ) a69 0 in
      if a68
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let a70 = a2 tl in
        append h a70
      else #match_failure
      
    
  let concat =
    a2
  let rec iter f xs =
    let a71 = ( = ) xs [] in
    if a71
    then ()
    else let a73 = #list_length xs in
      let a72 = ( > ) a73 0 in
      if a72
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let () = f h in
        iter f tl
      else #match_failure
      
    
  let a3 h a =
    (h, a)
  let rec cartesian xs ys =
    let a74 = ( = ) xs [] in
    if a74
    then []
    else let a76 = #list_length xs in
      let a75 = ( > ) a76 0 in
      if a75
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let a78 = a3 h in
        let a77 = map a78 ys in
        let a79 = cartesian tl ys in
        append a77 a79
      else #match_failure
      
    
  let main =
    let a82 = (3 :: []) in
    let a81 = (2 :: a82) in
    let a80 = (1 :: a81) in
    let () = iter print_int a80 in
    let a86 = (2 :: []) in
    let a85 = (1 :: a86) in
    let a90 = (4 :: []) in
    let a89 = (3 :: a90) in
    let a88 = (2 :: a89) in
    let a87 = (1 :: a88) in
    let a84 = cartesian a85 a87 in
    let a83 = length a84 in
    let () = print_int a83 in
    0

Test for names generation
  $ ./run_to_anf.exe << EOF
  > let a0 = 1
  > let a3 = a0 + 1
  > let a101 = 2
  > let a1 a b c =
  >   let r k m = k + 3 in 
  >   let g j k = c + a3 in 
  >   let a4 = 3 + a0 in 
  >   1
  >   
  Types before modifications:
  val a0 : int
  val a1 : 'a -> 'b -> int -> int
  val a101 : int
  val a3 : int
  
  Types after modifications:
  val a0 : int
  val a1 : 'a -> 'b -> 'c -> int
  val a101 : int
  val a2 : int -> 'a -> int
  val a3 : int
  val a5 : int -> 'a -> 'b -> int
  
  Modified ast:
  let a0 =
    1
  let a3 =
    ( + ) a0 1
  let a101 =
    2
  let a2 k m =
    ( + ) k 3
  let a5 c j k =
    ( + ) c a3
  let a1 a b c =
    let a4 = ( + ) 3 a0 in
    1

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
  val a1 : int
  val fac : int -> int
  
  Modified ast:
  let rec fac n =
    let a2 = ( <= ) n 1 in
    if a2
    then 1
    else let a4 = ( - ) n 1 in
      let a3 = fac a4 in
      ( * ) n a3
    
  let a1 =
    let a5 = fac 4 in
    let a0 = print_int a5 in
    0

  $ ./run_to_anf.exe < manytests/typed/002fac.ml
  Types before modifications:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Types after modifications:
  val a1 : int
  val a3 : (int -> 'a) -> int -> int -> 'a
  val a4 : 'a -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  
  Modified ast:
  let a3 k n p =
    let a5 = ( * ) p n in
    k a5
  let rec fac_cps n k =
    let a6 = ( = ) n 1 in
    if a6
    then k 1
    else let a7 = ( - ) n 1 in
      let a8 = a3 k n in
      fac_cps a7 a8
    
  let a4 a2 =
    a2
  let a1 =
    let a9 = fac_cps 4 a4 in
    let a0 = print_int a9 in
    0

  $ ./run_to_anf.exe < manytests/typed/003fib.ml
  Types before modifications:
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  
  Types after modifications:
  val a2 : int
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  
  Modified ast:
  let rec fib_acc a b n =
    let a3 = ( = ) n 1 in
    if a3
    then b
    else let n1 = ( - ) n 1 in
      let ab = ( + ) a b in
      fib_acc b ab n1
    
  let rec fib n =
    let a4 = ( < ) n 2 in
    if a4
    then n
    else let a6 = ( - ) n 1 in
      let a5 = fib a6 in
      let a8 = ( - ) n 2 in
      let a7 = fib a8 in
      ( + ) a5 a7
    
  let a2 =
    let a9 = fib_acc 0 1 4 in
    let a1 = print_int a9 in
    let a10 = fib 4 in
    let a0 = print_int a10 in
    0

  $ ./run_to_anf.exe < manytests/typed/004manyargs.ml
  Types before modifications:
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  
  Types after modifications:
  val a4 : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  
  Modified ast:
  let wrap f =
    let a5 = ( = ) 1 1 in
    if a5
    then f
    else f
    
  let test3 a b c =
    let a1 = print_int a in
    let a2 = print_int b in
    let a3 = print_int c in
    0
  let test10 a b c d e f g h i j =
    let a13 = ( + ) a b in
    let a12 = ( + ) a13 c in
    let a11 = ( + ) a12 d in
    let a10 = ( + ) a11 e in
    let a9 = ( + ) a10 f in
    let a8 = ( + ) a9 g in
    let a7 = ( + ) a8 h in
    let a6 = ( + ) a7 i in
    ( + ) a6 j
  let a4 =
    let rez = wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000 in
    let a0 = print_int rez in
    let temp2 = wrap test3 1 10 100 in
    0

  $ ./run_to_anf.exe < manytests/typed/005fix.ml
  Types before modifications:
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  
  Types after modifications:
  val a1 : int
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  
  Modified ast:
  let rec fix f x =
    let a2 = fix f in
    f a2 x
  let fac self n =
    let a3 = ( <= ) n 1 in
    if a3
    then 1
    else let a5 = ( - ) n 1 in
      let a4 = self a5 in
      ( * ) n a4
    
  let a1 =
    let a6 = fix fac 6 in
    let a0 = print_int a6 in
    0

Here second declaration of function "foo" was renamed to "#2" during modifications
  $ ./run_to_anf.exe < manytests/typed/006partial.ml
  Types before modifications:
  val foo : int -> int
  val main : int
  
  Types after modifications:
  val a3 : int -> int
  val a4 : int
  val a5 : int -> int
  val a6 : int -> int
  val foo : bool -> int -> int
  
  Modified ast:
  let a5 a1 =
    ( + ) a1 2
  let a6 a2 =
    ( * ) a2 10
  let foo b =
    if b
    then a5
    else a6
    
  let a3 x =
    let a9 = foo false x in
    let a8 = foo true a9 in
    let a7 = foo false a8 in
    foo true a7
  let a4 =
    let a10 = a3 11 in
    let a0 = print_int a10 in
    0

  $ ./run_to_anf.exe < manytests/typed/006partial2.ml
  Types before modifications:
  val foo : int -> int -> int -> int
  val main : int
  
  Types after modifications:
  val a4 : int
  val foo : int -> int -> int -> int
  
  Modified ast:
  let foo a b c =
    let a2 = print_int a in
    let a1 = print_int b in
    let a0 = print_int c in
    let a8 = ( * ) b c in
    ( + ) a a8
  let a4 =
    let a5 = foo 1 in
    let a6 = a5 2 in
    let a7 = a6 3 in
    let a3 = print_int a7 in
    0

  $ ./run_to_anf.exe < manytests/typed/006partial3.ml
  Types before modifications:
  val foo : int -> int -> int -> unit
  val main : int
  
  Types after modifications:
  val a3 : int
  val a4 : int -> int -> unit
  val a5 : int -> unit
  val foo : int -> int -> int -> unit
  
  Modified ast:
  let a5 c =
    print_int c
  let a4 b =
    let a0 = print_int b in
    a5
  let foo a =
    let a1 = print_int a in
    a4
  let a3 =
    let a2 = foo 4 8 9 in
    0

  $ ./run_to_anf.exe < manytests/typed/007order.ml
  Types before modifications:
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  
  Types after modifications:
  val a7 : 'a -> 'b -> int -> 'c -> int -> int -> 'd -> int -> int -> int
  val a8 : unit
  
  Modified ast:
  let a7 a0 a1 a a2 b _c a3 d __ =
    let a6 = (a0, a1, a2, a3) in
    let a9 = ( + ) a b in
    let a5 = print_int a9 in
    let a4 = print_int __ in
    let a11 = ( * ) a b in
    let a10 = ( / ) a11 _c in
    ( + ) a10 d
  let a8 =
    let a13 = print_int 1 in
    let a14 = print_int 2 in
    let a15 = print_int 4 in
    let a17 = ( ~- ) 1 in
    let a16 = print_int a17 in
    let a18 = ( ~- ) 555555 in
    let a12 = a7 a13 a14 3 a15 100 1000 a16 10000 a18 in
    print_int a12

The explicitly specified types were erased after type inference 
  $ ./run_to_anf.exe < manytests/typed/008ascription.ml
  Types before modifications:
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int
  
  Types after modifications:
  val a1 : int
  val a3 : int -> bool -> int
  val a4 : int -> bool
  val addi : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  
  Modified ast:
  let addi f g x =
    let a5 = g x in
    f x a5
  let a3 x b =
    if b
    then ( + ) x 1
    else ( * ) x 2
    
  let a4 a2 =
    let a6 = ( / ) a2 2 in
    ( = ) a6 0
  let a1 =
    let a7 = addi a3 a4 4 in
    let a0 = print_int a7 in
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
  val a5 : int
  val a6 : ('a -> 'b) -> 'a -> ('c -> 'd) * ('c -> 'd)
  val a7 : 'a -> ('a -> 'b) -> ('b -> 'c -> 'd) -> 'c -> 'd
  val feven : 'a -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : 'a -> ('b -> 'c) * ('b -> 'c)
  val fodd : 'a -> int -> int
  val map : ('a -> 'b) -> 'c -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : ('a -> 'b) * ('a -> 'b)
  
  Modified ast:
  let rec fix f x =
    let a8 = fix f in
    f a8 x
  let map f p =
    let a = #unpack_tuple p 0 in
    let b = #unpack_tuple p 1 in
    let a9 = f a in
    let a10 = f b in
    (a9, a10)
  let a7 a4 self li x =
    let a11 = self a4 in
    li a11 x
  let a6 self a4 =
    let a12 = a7 a4 self in
    map a12 a4
  let fixpoly l =
    fix a6 l
  let feven p n =
    let e = #unpack_tuple p 0 in
    let o = #unpack_tuple p 1 in
    let a13 = ( == ) n 0 in
    if a13
    then 1
    else let a14 = ( - ) n 1 in
      o a14
    
  let fodd p n =
    let e = #unpack_tuple p 0 in
    let o = #unpack_tuple p 1 in
    let a15 = ( == ) n 0 in
    if a15
    then 0
    else let a16 = ( - ) n 1 in
      e a16
    
  let tie =
    let a17 = (feven, fodd) in
    fixpoly a17
  let rec modd n =
    let a18 = ( = ) n 0 in
    if a18
    then 1
    else let a19 = ( - ) n 1 in
      meven a19
    
  and meven n =
    let a20 = ( = ) n 0 in
    if a20
    then 1
    else let a21 = ( - ) n 1 in
      modd a21
    
  let a5 =
    let a22 = modd 1 in
    let a3 = print_int a22 in
    let a23 = meven 2 in
    let a2 = print_int a23 in
    let even = #unpack_tuple tie 0 in
    let odd = #unpack_tuple tie 1 in
    let a24 = odd 3 in
    let a1 = print_int a24 in
    let a25 = even 4 in
    let a0 = print_int a25 in
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
  val a4 : int
  val a5 : int -> 'a list -> int
  val a6 : 'a list list -> 'a list
  val a7 : 'a -> 'b -> 'a * 'b
  val append : 'a list -> 'a list -> 'a list
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val concat : 'a list list -> 'a list
  val iter : ('a -> 'b) -> 'a list -> unit
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  
  Modified ast:
  let rec length xs =
    let a8 = ( = ) xs [] in
    if a8
    then 0
    else let a10 = #list_length xs in
      let a9 = ( > ) a10 0 in
      if a9
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let a11 = length tl in
        ( + ) 1 a11
      else #match_failure
      
    
  let rec a5 acc xs =
    let a12 = ( = ) xs [] in
    if a12
    then acc
    else let a14 = #list_length xs in
      let a13 = ( > ) a14 0 in
      if a13
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let a15 = ( + ) acc 1 in
        a5 a15 tl
      else #match_failure
      
    
  let length_tail =
    a5 0
  let rec map f xs =
    let a16 = ( = ) xs [] in
    if a16
    then []
    else let a19 = #list_length xs in
      let a18 = ( > ) a19 0 in
      let a21 = #list_tl xs in
      let a20 = ( = ) a21 [] in
      let a17 = ( && ) a18 a20 in
      if a17
      then let a = #list_hd xs in
        let a22 = f a in
        (a22 :: [])
      else let a25 = #list_length xs in
        let a24 = ( > ) a25 1 in
        let a28 = #list_tl xs in
        let a27 = #list_tl a28 in
        let a26 = ( = ) a27 [] in
        let a23 = ( && ) a24 a26 in
        if a23
        then let a = #list_hd xs in
          let a29 = #list_tl xs in
          let b = #list_hd a29 in
          let a30 = f a in
          let a32 = f b in
          let a31 = (a32 :: []) in
          (a30 :: a31)
        else let a35 = #list_length xs in
          let a34 = ( > ) a35 2 in
          let a39 = #list_tl xs in
          let a38 = #list_tl a39 in
          let a37 = #list_tl a38 in
          let a36 = ( = ) a37 [] in
          let a33 = ( && ) a34 a36 in
          if a33
          then let a = #list_hd xs in
            let a40 = #list_tl xs in
            let b = #list_hd a40 in
            let a42 = #list_tl xs in
            let a41 = #list_tl a42 in
            let c = #list_hd a41 in
            let a43 = f a in
            let a45 = f b in
            let a47 = f c in
            let a46 = (a47 :: []) in
            let a44 = (a45 :: a46) in
            (a43 :: a44)
          else let a49 = #list_length xs in
            let a48 = ( > ) a49 3 in
            if a48
            then let a = #list_hd xs in
              let a50 = #list_tl xs in
              let b = #list_hd a50 in
              let a52 = #list_tl xs in
              let a51 = #list_tl a52 in
              let c = #list_hd a51 in
              let a55 = #list_tl xs in
              let a54 = #list_tl a55 in
              let a53 = #list_tl a54 in
              let d = #list_hd a53 in
              let a58 = #list_tl xs in
              let a57 = #list_tl a58 in
              let a56 = #list_tl a57 in
              let tl = #list_tl a56 in
              let a59 = f a in
              let a61 = f b in
              let a63 = f c in
              let a65 = f d in
              let a66 = map f tl in
              let a64 = (a65 :: a66) in
              let a62 = (a63 :: a64) in
              let a60 = (a61 :: a62) in
              (a59 :: a60)
            else #match_failure
            
          
        
      
    
  let rec append xs ys =
    let a67 = ( = ) xs [] in
    if a67
    then ys
    else let a69 = #list_length xs in
      let a68 = ( > ) a69 0 in
      if a68
      then let x = #list_hd xs in
        let a3 = #list_tl xs in
        let a70 = append a3 ys in
        (x :: a70)
      else #match_failure
      
    
  let rec a6 xs =
    let a71 = ( = ) xs [] in
    if a71
    then []
    else let a73 = #list_length xs in
      let a72 = ( > ) a73 0 in
      if a72
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let a74 = a6 tl in
        append h a74
      else #match_failure
      
    
  let concat =
    a6
  let rec iter f xs =
    let a75 = ( = ) xs [] in
    if a75
    then ()
    else let a77 = #list_length xs in
      let a76 = ( > ) a77 0 in
      if a76
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let a0 = f h in
        iter f tl
      else #match_failure
      
    
  let a7 h a =
    (h, a)
  let rec cartesian xs ys =
    let a78 = ( = ) xs [] in
    if a78
    then []
    else let a80 = #list_length xs in
      let a79 = ( > ) a80 0 in
      if a79
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let a82 = a7 h in
        let a81 = map a82 ys in
        let a83 = cartesian tl ys in
        append a81 a83
      else #match_failure
      
    
  let a4 =
    let a86 = (3 :: []) in
    let a85 = (2 :: a86) in
    let a84 = (1 :: a85) in
    let a2 = iter print_int a84 in
    let a90 = (2 :: []) in
    let a89 = (1 :: a90) in
    let a94 = (4 :: []) in
    let a93 = (3 :: a94) in
    let a92 = (2 :: a93) in
    let a91 = (1 :: a92) in
    let a88 = cartesian a89 a91 in
    let a87 = length a88 in
    let a1 = print_int a87 in
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

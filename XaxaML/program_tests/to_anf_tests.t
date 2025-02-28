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
    let #0 = sum 1 2 in
    let #1 = inc 2 in
    sum #0 #1
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
  val #2 : int -> int
  val #3 : int -> int
  val #4 : int -> int -> int
  val f : int -> int -> int
  
  Modified ast:
  let #2 y =
    ( + ) y 1
  let #3 x =
    #2 x
  let #4 #0 #1 =
    ( + ) #0 #1
  let f a b =
    let #5 = #4 a b in
    #3 #5
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
  val #0 : int -> bool
  val #1 : int -> bool
  val f : bool * bool
  
  Modified ast:
  let rec #0 n =
    let #2 = ( = ) n 0 in
    if #2
    then false
    else let #3 = ( - ) n 1 in
      #1 #3
    
  and #1 n =
    let #4 = ( = ) n 0 in
    if #4
    then true
    else let #5 = ( - ) n 1 in
      #0 #5
    
  let f =
    let #6 = #1 4 in
    let #7 = #0 5 in
    (#6, #7)

  $ ./run_to_anf.exe << EOF
  > let rec y x =
  >   let a z = y x + z in
  >   let b x = a 5 + x in
  >   b 5
  Types before modifications:
  val y : 'a -> int
  
  Types after modifications:
  val #1 : 'a -> ('a -> int) -> int -> int
  val #2 : (int -> int) -> int -> int
  val y : 'a -> int
  
  Modified ast:
  let #1 x y z =
    let #3 = y x in
    ( + ) #3 z
  let #2 a #0 =
    let #4 = a 5 in
    ( + ) #4 #0
  let rec y x =
    let #5 = #1 x y in
    #2 #5 5

  $ ./run_to_anf.exe << EOF
  > let f a b =
  >   let a x = b + x in
  >   let b x = a 1 + x in
  >   a 1 + b 2;;
  Types before modifications:
  val f : 'a -> int -> int
  
  Types after modifications:
  val #2 : int -> int -> int
  val #3 : (int -> int) -> int -> int
  val f : 'a -> int -> int
  
  Modified ast:
  let #2 b x =
    ( + ) b x
  let #3 #0 x =
    let #4 = #0 1 in
    ( + ) #4 x
  let f a b =
    let #5 = #2 b 1 in
    let #7 = #2 b in
    let #6 = #3 #7 2 in
    ( + ) #5 #6
  $ ./run_to_anf.exe << EOF
  > let abc = 
  >   let (a, (b, c)) = (1, (2, 3)) in 
  >   let (d::e) = [1; 2; 3] in
  >   let f (x, y) = x + y in 
  >   0
  Types before modifications:
  val abc : int
  
  Types after modifications:
  val #3 : 'a -> int
  val abc : int
  
  Modified ast:
  let #3 #0 =
    let x = #unpack_tuple #0 0 in
    let y = #unpack_tuple #0 1 in
    ( + ) x y
  let abc =
    let #4 = (2, 3) in
    let #2 = (1, #4) in
    let a = #unpack_tuple #2 0 in
    let #5 = #unpack_tuple #2 1 in
    let b = #unpack_tuple #5 0 in
    let #6 = #unpack_tuple #2 1 in
    let c = #unpack_tuple #6 1 in
    let #8 = (3 :: []) in
    let #7 = (2 :: #8) in
    let #1 = (1 :: #7) in
    let #10 = #list_length #1 in
    let #9 = ( > ) #10 0 in
    if #9
    then let d = #list_hd #1 in
      let e = #list_tl #1 in
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
    let #0 = ( <= ) n 1 in
    if #0
    then 1
    else let #2 = ( - ) n 1 in
      let #1 = fac #2 in
      ( * ) n #1
    
  let main =
    let #3 = fac 4 in
    let () = print_int #3 in
    0

  $ ./run_to_anf.exe < manytests/typed/002fac.ml
  Types before modifications:
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Types after modifications:
  val #1 : (int -> 'a) -> int -> int -> 'a
  val #2 : 'a -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  
  Modified ast:
  let #1 k n p =
    let #3 = ( * ) p n in
    k #3
  let rec fac_cps n k =
    let #4 = ( = ) n 1 in
    if #4
    then k 1
    else let #5 = ( - ) n 1 in
      let #6 = #1 k n in
      fac_cps #5 #6
    
  let #2 #0 =
    #0
  let main =
    let #7 = fac_cps 4 #2 in
    let () = print_int #7 in
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
    let #0 = ( = ) n 1 in
    if #0
    then b
    else let n1 = ( - ) n 1 in
      let ab = ( + ) a b in
      fib_acc b ab n1
    
  let rec fib n =
    let #1 = ( < ) n 2 in
    if #1
    then n
    else let #3 = ( - ) n 1 in
      let #2 = fib #3 in
      let #5 = ( - ) n 2 in
      let #4 = fib #5 in
      ( + ) #2 #4
    
  let main =
    let #6 = fib_acc 0 1 4 in
    let () = print_int #6 in
    let #7 = fib 4 in
    let () = print_int #7 in
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
    let #3 = ( = ) 1 1 in
    if #3
    then f
    else f
    
  let test3 a b c =
    let #0 = print_int a in
    let #1 = print_int b in
    let #2 = print_int c in
    0
  let test10 a b c d e f g h i j =
    let #11 = ( + ) a b in
    let #10 = ( + ) #11 c in
    let #9 = ( + ) #10 d in
    let #8 = ( + ) #9 e in
    let #7 = ( + ) #8 f in
    let #6 = ( + ) #7 g in
    let #5 = ( + ) #6 h in
    let #4 = ( + ) #5 i in
    ( + ) #4 j
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
    let #0 = fix f in
    f #0 x
  let fac self n =
    let #1 = ( <= ) n 1 in
    if #1
    then 1
    else let #3 = ( - ) n 1 in
      let #2 = self #3 in
      ( * ) n #2
    
  let main =
    let #4 = fix fac 6 in
    let () = print_int #4 in
    0

Here second declaration of function "foo" was renamed to "#2" during modifications
  $ ./run_to_anf.exe < manytests/typed/006partial.ml
  Types before modifications:
  val foo : int -> int
  val main : int
  
  Types after modifications:
  val #2 : int -> int
  val #3 : int -> int
  val #4 : int -> int
  val foo : bool -> int -> int
  val main : int
  
  Modified ast:
  let #3 #0 =
    ( + ) #0 2
  let #4 #1 =
    ( * ) #1 10
  let foo b =
    if b
    then #3
    else #4
    
  let #2 x =
    let #7 = foo false x in
    let #6 = foo true #7 in
    let #5 = foo false #6 in
    foo true #5
  let main =
    let #8 = #2 11 in
    let () = print_int #8 in
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
    let #3 = ( * ) b c in
    ( + ) a #3
  let main =
    let #0 = foo 1 in
    let #1 = #0 2 in
    let #2 = #1 3 in
    let () = print_int #2 in
    0

  $ ./run_to_anf.exe < manytests/typed/006partial3.ml
  Types before modifications:
  val foo : int -> int -> int -> unit
  val main : int
  
  Types after modifications:
  val #0 : int -> int -> unit
  val #1 : int -> unit
  val foo : int -> int -> int -> unit
  val main : int
  
  Modified ast:
  let #1 c =
    print_int c
  let #0 b =
    let () = print_int b in
    #1
  let foo a =
    let () = print_int a in
    #0
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
    let #0 = ( + ) a b in
    let () = print_int #0 in
    let () = print_int __ in
    let #2 = ( * ) a b in
    let #1 = ( / ) #2 _c in
    ( + ) #1 d
  let main =
    let #4 = print_int 1 in
    let #5 = print_int 2 in
    let #6 = print_int 4 in
    let #8 = ( ~- ) 1 in
    let #7 = print_int #8 in
    let #9 = ( ~- ) 555555 in
    let #3 = _start #4 #5 3 #6 100 1000 #7 10000 #9 in
    print_int #3

The explicitly specified types were erased after type inference 
  $ ./run_to_anf.exe < manytests/typed/008ascription.ml
  Types before modifications:
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int
  
  Types after modifications:
  val #0 : int -> bool -> int
  val #1 : int -> bool
  val addi : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val main : int
  
  Modified ast:
  let addi f g x =
    let #2 = g x in
    f x #2
  let #0 x b =
    if b
    then ( + ) x 1
    else ( * ) x 2
    
  let #1 _start =
    let #3 = ( / ) _start 2 in
    ( = ) #3 0
  let main =
    let #4 = addi #0 #1 4 in
    let () = print_int #4 in
    0

  $ ./run_to_anf.exe < manytests/typed/009let_poly.ml
  Types before modifications:
  val temp : int * bool
  
  Types after modifications:
  val #0 : 'a -> 'a
  val temp : int * bool
  
  Modified ast:
  let #0 x =
    x
  let temp =
    let #1 = #0 1 in
    let #2 = #0 true in
    (#1, #2)
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
  val #1 : ('a -> 'b) -> 'a -> ('c -> 'd) * ('c -> 'd)
  val #2 : 'a -> ('a -> 'b) -> ('b -> 'c -> 'd) -> 'c -> 'd
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
    let #3 = fix f in
    f #3 x
  let map f p =
    let a = #unpack_tuple p 0 in
    let b = #unpack_tuple p 1 in
    let #4 = f a in
    let #5 = f b in
    (#4, #5)
  let #2 #0 self li x =
    let #6 = self #0 in
    li #6 x
  let #1 self #0 =
    let #7 = #2 #0 self in
    map #7 #0
  let fixpoly l =
    fix #1 l
  let feven p n =
    let e = #unpack_tuple p 0 in
    let o = #unpack_tuple p 1 in
    let #8 = ( == ) n 0 in
    if #8
    then 1
    else let #9 = ( - ) n 1 in
      o #9
    
  let fodd p n =
    let e = #unpack_tuple p 0 in
    let o = #unpack_tuple p 1 in
    let #10 = ( == ) n 0 in
    if #10
    then 0
    else let #11 = ( - ) n 1 in
      e #11
    
  let tie =
    let #12 = (feven, fodd) in
    fixpoly #12
  let rec modd n =
    let #13 = ( = ) n 0 in
    if #13
    then 1
    else let #14 = ( - ) n 1 in
      meven #14
    
  and meven n =
    let #15 = ( = ) n 0 in
    if #15
    then 1
    else let #16 = ( - ) n 1 in
      modd #16
    
  let main =
    let #17 = modd 1 in
    let () = print_int #17 in
    let #18 = meven 2 in
    let () = print_int #18 in
    let even = #unpack_tuple tie 0 in
    let odd = #unpack_tuple tie 1 in
    let #19 = odd 3 in
    let () = print_int #19 in
    let #20 = even 4 in
    let () = print_int #20 in
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
  val #1 : int -> 'a list -> int
  val #2 : 'a list list -> 'a list
  val #3 : 'a -> 'b -> 'a * 'b
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
    let #4 = ( = ) xs [] in
    if #4
    then 0
    else let #6 = #list_length xs in
      let #5 = ( > ) #6 0 in
      if #5
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let #7 = length tl in
        ( + ) 1 #7
      else #match_failure
      
    
  let rec #1 acc xs =
    let #8 = ( = ) xs [] in
    if #8
    then acc
    else let #10 = #list_length xs in
      let #9 = ( > ) #10 0 in
      if #9
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let #11 = ( + ) acc 1 in
        #1 #11 tl
      else #match_failure
      
    
  let length_tail =
    #1 0
  let rec map f xs =
    let #12 = ( = ) xs [] in
    if #12
    then []
    else let #15 = #list_length xs in
      let #14 = ( > ) #15 0 in
      let #17 = #list_tl xs in
      let #16 = ( = ) #17 [] in
      let #13 = ( && ) #14 #16 in
      if #13
      then let a = #list_hd xs in
        let #18 = f a in
        (#18 :: [])
      else let #21 = #list_length xs in
        let #20 = ( > ) #21 1 in
        let #24 = #list_tl xs in
        let #23 = #list_tl #24 in
        let #22 = ( = ) #23 [] in
        let #19 = ( && ) #20 #22 in
        if #19
        then let a = #list_hd xs in
          let #25 = #list_tl xs in
          let b = #list_hd #25 in
          let #26 = f a in
          let #28 = f b in
          let #27 = (#28 :: []) in
          (#26 :: #27)
        else let #31 = #list_length xs in
          let #30 = ( > ) #31 2 in
          let #35 = #list_tl xs in
          let #34 = #list_tl #35 in
          let #33 = #list_tl #34 in
          let #32 = ( = ) #33 [] in
          let #29 = ( && ) #30 #32 in
          if #29
          then let a = #list_hd xs in
            let #36 = #list_tl xs in
            let b = #list_hd #36 in
            let #38 = #list_tl xs in
            let #37 = #list_tl #38 in
            let c = #list_hd #37 in
            let #39 = f a in
            let #41 = f b in
            let #43 = f c in
            let #42 = (#43 :: []) in
            let #40 = (#41 :: #42) in
            (#39 :: #40)
          else let #45 = #list_length xs in
            let #44 = ( > ) #45 3 in
            if #44
            then let a = #list_hd xs in
              let #46 = #list_tl xs in
              let b = #list_hd #46 in
              let #48 = #list_tl xs in
              let #47 = #list_tl #48 in
              let c = #list_hd #47 in
              let #51 = #list_tl xs in
              let #50 = #list_tl #51 in
              let #49 = #list_tl #50 in
              let d = #list_hd #49 in
              let #54 = #list_tl xs in
              let #53 = #list_tl #54 in
              let #52 = #list_tl #53 in
              let tl = #list_tl #52 in
              let #55 = f a in
              let #57 = f b in
              let #59 = f c in
              let #61 = f d in
              let #62 = map f tl in
              let #60 = (#61 :: #62) in
              let #58 = (#59 :: #60) in
              let #56 = (#57 :: #58) in
              (#55 :: #56)
            else #match_failure
            
          
        
      
    
  let rec append xs ys =
    let #63 = ( = ) xs [] in
    if #63
    then ys
    else let #65 = #list_length xs in
      let #64 = ( > ) #65 0 in
      if #64
      then let x = #list_hd xs in
        let #0 = #list_tl xs in
        let #66 = append #0 ys in
        (x :: #66)
      else #match_failure
      
    
  let rec #2 xs =
    let #67 = ( = ) xs [] in
    if #67
    then []
    else let #69 = #list_length xs in
      let #68 = ( > ) #69 0 in
      if #68
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let #70 = #2 tl in
        append h #70
      else #match_failure
      
    
  let concat =
    #2
  let rec iter f xs =
    let #71 = ( = ) xs [] in
    if #71
    then ()
    else let #73 = #list_length xs in
      let #72 = ( > ) #73 0 in
      if #72
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let () = f h in
        iter f tl
      else #match_failure
      
    
  let #3 h a =
    (h, a)
  let rec cartesian xs ys =
    let #74 = ( = ) xs [] in
    if #74
    then []
    else let #76 = #list_length xs in
      let #75 = ( > ) #76 0 in
      if #75
      then let h = #list_hd xs in
        let tl = #list_tl xs in
        let #78 = #3 h in
        let #77 = map #78 ys in
        let #79 = cartesian tl ys in
        append #77 #79
      else #match_failure
      
    
  let main =
    let #82 = (3 :: []) in
    let #81 = (2 :: #82) in
    let #80 = (1 :: #81) in
    let () = iter print_int #80 in
    let #86 = (2 :: []) in
    let #85 = (1 :: #86) in
    let #90 = (4 :: []) in
    let #89 = (3 :: #90) in
    let #88 = (2 :: #89) in
    let #87 = (1 :: #88) in
    let #84 = cartesian #85 #87 in
    let #83 = length #84 in
    let () = print_int #83 in
    0

  $ ./cc_runner.exe < manytests/do_not_type/001.ml
  Unbound value 'fac'

  $ ./cc_runner.exe < manytests/do_not_type/002if.ml
  This expression has type bool but an expression was expected of type int

  $ ./cc_runner.exe < manytests/do_not_type/003occurs.ml
  The type variable 'a occurs inside 'a -> 'b

  $ ./cc_runner.exe < manytests/do_not_type/004let_poly.ml
  This expression has type int but an expression was expected of type bool

  $ ./cc_runner.exe < manytests/do_not_type/015tuples.ml
  Only variables are allowed as left-side of 'let rec'

  $ ./cc_runner.exe < manytests/typed/001fac.ml
  Bindings before transformations:
  val fac: int -> int
  val main: int
  
  Bindings after transformations:
  val fac: int -> int
  val main: int
  
  ------------------------------
  
  let rec fac n =
    (if (( <= ) n) 1
    then
      1
    else
      (( * ) n) (fac ((( - ) n) 1)))
  ;;
  
  let cc_ac0_main = let () = print_int (fac 4) in
      0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/002fac.ml
  Bindings before transformations:
  val fac_cps: int -> (int -> 'a) -> 'a
  val main: int
  
  Bindings after transformations:
  val fac_cps: int -> (int -> 'a) -> 'a
  val main: int
  
  ------------------------------
  
  let rec fac_cps n k =
    (if (( = ) n) 1
    then
      k 1
    else
      (fac_cps ((( - ) n) 1)) (((fun cc0_n cc1_k p -> cc1_k ((( * ) p) cc0_n)) n) k))
  ;;
  
  let cc_ac1_main = let () = print_int ((fac_cps 4) (fun cc_ac0_print_int -> cc_ac0_print_int)) in
      0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/003fib.ml
  Bindings before transformations:
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  val main: int
  
  Bindings after transformations:
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  val main: int
  
  ------------------------------
  
  let rec fib_acc a b n =
    (if (( = ) n) 1
    then
      b
    else
      let n1 = (( - ) n) 1 in
        let ab = (( + ) a) b in
          ((fib_acc b) ab) n1)
  ;;
  
  let rec fib cc_ac0_n =
    (if (( < ) cc_ac0_n) 2
    then
      cc_ac0_n
    else
      (( + ) fib ((( - ) cc_ac0_n) 1)) (fib ((( - ) cc_ac0_n) 2)))
  ;;
  
  let cc_ac1_main = let () = print_int (((fib_acc 0) 1) 4) in
      let () = print_int (fib 4) in
        0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/004manyargs.ml
  Bindings before transformations:
  val main: int
  val test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3: int -> int -> int -> int
  val wrap: 'a -> 'a
  
  Bindings after transformations:
  val main: int
  val test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3: int -> int -> int -> int
  val wrap: 'a -> 'a
  
  ------------------------------
  
  let wrap f =
    (if (( = ) 1) 1
    then
      f
    else
      f)
  ;;
  
  let test3 a b c =
    let cc_ac0_a = print_int a in
      let cc_ac1_b = print_int b in
        let cc_ac2_c = print_int c in
          0
  ;;
  
  let test10 cc_ac3_a cc_ac4_b cc_ac5_c d e cc_ac6_f g h i j =
    (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) cc_ac3_a) cc_ac4_b) cc_ac5_c) d) e) cc_ac6_f) g) h) i) j
  ;;
  
  let cc_ac7_main = let rez = ((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000 in
      let () = print_int rez in
        let temp2 = (((wrap test3) 1) 10) 100 in
          0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/005fix.ml
  Bindings before transformations:
  val fac: (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main: int
  
  Bindings after transformations:
  val fac: (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main: int
  
  ------------------------------
  
  let rec fix f x =
    (f (fix f)) x
  ;;
  
  let fac self n =
    (if (( <= ) n) 1
    then
      1
    else
      (( * ) n) (self ((( - ) n) 1)))
  ;;
  
  let cc_ac0_main = let () = print_int ((fix fac) 6) in
      0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/006partial.ml
  Bindings before transformations:
  val foo: int -> int
  val main: int
  
  Bindings after transformations:
  val foo: int -> int
  val main: int
  
  ------------------------------
  
  let cc_ac1_foo b =
    (if b
    then
      (fun foo -> (( + ) foo) 2)
    else
      (fun cc_ac0_foo -> (( * ) cc_ac0_foo) 10))
  ;;
  
  let cc_ac2_foo x =
    (cc_ac1_foo true) ((cc_ac1_foo false) ((cc_ac1_foo true) ((cc_ac1_foo false) x)))
  ;;
  
  let cc_ac3_main = let () = print_int (cc_ac2_foo 11) in
      0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/006partial2.ml
  Bindings before transformations:
  val foo: int -> int -> int -> int
  val main: int
  
  Bindings after transformations:
  val foo: int -> int -> int -> int
  val main: int
  
  ------------------------------
  
  let foo a b c =
    let () = print_int a in
      let () = print_int b in
        let () = print_int c in
          (( + ) a) ((( * ) b) c)
  ;;
  
  let cc_ac3_main = let cc_ac0_foo = foo 1 in
      let cc_ac1_foo = cc_ac0_foo 2 in
        let cc_ac2_foo = cc_ac1_foo 3 in
          let () = print_int cc_ac2_foo in
            0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/006partial3.ml
  Bindings before transformations:
  val foo: int -> int -> int -> unit
  val main: int
  
  Bindings after transformations:
  val foo: int -> int -> int -> unit
  val main: int
  
  ------------------------------
  
  let foo a =
    let () = print_int a in
      (fun b -> let () = print_int b in
        (fun c -> print_int c))
  ;;
  
  let cc_ac0_main = let () = ((foo 4) 8) 9 in
      0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/007order.ml
  Bindings before transformations:
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit
  
  Bindings after transformations:
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit
  
  ------------------------------
  
  let cc_ac0__start () () a () b _c () d __ =
    let () = print_int ((( + ) a) b) in
      let () = print_int __ in
        (( + ) (( / ) (( * ) a) b) _c) d
  ;;
  
  let cc_ac1_main = print_int (((((((((cc_ac0__start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (~- 1))) 10000) (~- 555555))
  ;;
  

  $ ./cc_runner.exe < manytests/typed/008ascription.ml
  Bindings before transformations:
  val addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main: int
  
  Bindings after transformations:
  val addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main: int
  
  ------------------------------
  
  let addi f g x =
    (f x) (g x)
  ;;
  
  let cc_ac2_main = let () = print_int (((addi (fun cc_ac0_x b -> (if b
      then
        (( + ) cc_ac0_x) 1
      else
        (( * ) cc_ac0_x) 2))) (fun cc_ac1__start -> (( = ) (( / ) cc_ac1__start) 2) 0)) 4) in
      0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/009let_poly.ml
  Bindings before transformations:
  val temp: int * bool
  
  Bindings after transformations:
  val temp: int * bool
  
  ------------------------------
  
  let temp = let f x =
      x in
      (f 1, f true)
  ;;
  

  $ ./cc_runner.exe < manytests/typed/015tuples.ml
  Bindings before transformations:
  val feven: 'a * (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly: ((('a -> 'b) * ('a -> 'b)) -> ('a -> 'b)) * ((('a -> 'b) * ('a -> 'b)) -> ('a -> 'b)) -> ('a -> 'b) * ('a -> 'b)
  val fodd: (int -> int) * 'a -> int -> int
  val main: int
  val map: ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven: int -> int
  val modd: int -> int
  val tie: (int -> int) * (int -> int)
  
  Bindings after transformations:
  val feven: 'a * (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly: ((('a -> 'b) * ('a -> 'b)) -> ('a -> 'b)) * ((('a -> 'b) * ('a -> 'b)) -> ('a -> 'b)) -> ('a -> 'b) * ('a -> 'b)
  val fodd: (int -> int) * 'a -> int -> int
  val main: int
  val map: ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven: int -> int
  val modd: int -> int
  val tie: (int -> int) * (int -> int)
  
  ------------------------------
  
  let rec fix f x =
    (f (fix f)) x
  ;;
  
  let map cc_ac0_f p =
    let (a, b) = p in
      (cc_ac0_f a, cc_ac0_f b)
  ;;
  
  let fixpoly l =
    (fix (fun self cc_ac1_l -> (map (((fun cc0_self cc1_cc_ac1_l li cc_ac2_x -> (li (cc0_self cc1_cc_ac1_l)) cc_ac2_x) self) cc_ac1_l)) cc_ac1_l)) l
  ;;
  
  let feven cc_ac3_p n =
    let (e, o) = cc_ac3_p in
      (if (( == ) n) 0
      then
        1
      else
        o ((( - ) n) 1))
  ;;
  
  let fodd cc_ac4_p cc_ac5_n =
    let (cc_ac6_e, cc_ac7_o) = cc_ac4_p in
      (if (( == ) cc_ac5_n) 0
      then
        0
      else
        cc_ac6_e ((( - ) cc_ac5_n) 1))
  ;;
  
  let tie = fixpoly (feven, fodd)
  ;;
  
  let rec cc_ac_meven cc_ac8_n =
    (if (( = ) cc_ac8_n) 0
    then
      1
    else
      modd ((( - ) cc_ac8_n) 1))
  and modd cc_ac9_n =
    (if (( = ) cc_ac9_n) 0
    then
      1
    else
      cc_ac_meven ((( - ) cc_ac9_n) 1))
  ;;
  
  let cc_ac10_main = let () = print_int (modd 1) in
      let () = print_int (cc_ac_meven 2) in
        let (even, odd) = tie in
          let () = print_int (odd 3) in
            let () = print_int (even 4) in
              0
  ;;
  

  $ ./cc_runner.exe < manytests/typed/016lists.ml
  Bindings before transformations:
  val append: 'a list -> 'a list -> 'a list
  val cartesian: 'a list -> 'b list -> 'a * 'b list
  val concat: 'a list list -> 'a list
  val iter: ('a -> unit) -> 'a list -> unit
  val length: 'a list -> int
  val length_tail: 'a list -> int
  val main: int
  val map: ('a -> 'b) -> 'a list -> 'b list
  
  Bindings after transformations:
  val append: 'a list -> 'a list -> 'a list
  val cartesian: 'a list -> 'b list -> 'a * 'b list
  val concat: 'a list list -> 'a list
  val iter: ('a -> unit) -> 'a list -> unit
  val length: 'a list -> int
  val length_tail: 'a list -> int
  val main: int
  val map: ('a -> 'b) -> 'a list -> 'b list
  
  ------------------------------
  
  let rec length xs =
    match xs with
      | [] -> 
        0
      | (h :: tl) -> 
        (( + ) 1) (length tl)
  ;;
  
  let length_tail = let rec helper cc_ac_acc cc_ac0_xs =
      match cc_ac0_xs with
        | [] -> 
          cc_ac_acc
        | (cc_ac1_h :: cc_ac2_tl) -> 
          (helper ((( + ) cc_ac_acc) 1)) cc_ac2_tl in
      helper 0
  ;;
  
  let rec map f cc_ac3_xs =
    match cc_ac3_xs with
      | [] -> 
        []
      | (a :: []) -> 
        (f a :: [])
      | (cc_ac4_a :: (b :: [])) -> 
        (f cc_ac4_a :: (f b :: []))
      | (cc_ac5_a :: (cc_ac6_b :: (c :: []))) -> 
        (f cc_ac5_a :: (f cc_ac6_b :: (f c :: [])))
      | (cc_ac7_a :: (cc_ac8_b :: (cc_ac9_c :: (d :: cc_ac10_tl)))) -> 
        (f cc_ac7_a :: (f cc_ac8_b :: (f cc_ac9_c :: (f d :: (map f) cc_ac10_tl))))
  ;;
  
  let rec append cc_ac11_xs ys =
    match cc_ac11_xs with
      | [] -> 
        ys
      | (x :: cc_ac12_xs) -> 
        (x :: (append cc_ac12_xs) ys)
  ;;
  
  let concat = let rec cc_ac13_helper cc_ac14_xs =
      match cc_ac14_xs with
        | [] -> 
          []
        | (cc_ac15_h :: cc_ac16_tl) -> 
          (append cc_ac15_h) (cc_ac13_helper cc_ac16_tl) in
      cc_ac13_helper
  ;;
  
  let rec iter cc_ac17_f cc_ac18_xs =
    match cc_ac18_xs with
      | [] -> 
        ()
      | (cc_ac19_h :: cc_ac20_tl) -> 
        let () = cc_ac17_f cc_ac19_h in
          (iter cc_ac17_f) cc_ac20_tl
  ;;
  
  let rec cartesian cc_ac21_xs cc_ac22_ys =
    match cc_ac21_xs with
      | [] -> 
        []
      | (cc_ac23_h :: cc_ac24_tl) -> 
        (append ((map ((fun cc0_cc_ac23_h cc_ac25_a -> (cc0_cc_ac23_h, cc_ac25_a)) cc_ac23_h)) cc_ac22_ys)) ((cartesian cc_ac24_tl) cc_ac22_ys)
  ;;
  
  let cc_ac26_main = let () = (iter print_int) (1 :: (2 :: (3 :: []))) in
      let () = print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: [])))))) in
        0
  ;;
  


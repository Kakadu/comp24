  $ ./ll_runner.exe < manytests/do_not_type/001.ml
  Unbound value 'fac'

  $ ./ll_runner.exe < manytests/do_not_type/002if.ml
  This expression has type bool but an expression was expected of type int

  $ ./ll_runner.exe < manytests/do_not_type/003occurs.ml
  The type variable 'a occurs inside 'a -> 'b

  $ ./ll_runner.exe < manytests/do_not_type/004let_poly.ml
  This expression has type int but an expression was expected of type bool

  $ ./ll_runner.exe < manytests/do_not_type/015tuples.ml
  Only variables are allowed as left-side of 'let rec'

  $ ./ll_runner.exe < manytests/typed/001fac.ml
  Bindings before transformations:
  val fac: int -> int
  val main: int
  
  Bindings after transformations:
  val cc_ac0_main: int
  val fac: int -> int
  
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
  

  $ ./ll_runner.exe < manytests/typed/002fac.ml
  Bindings before transformations:
  val fac_cps: int -> (int -> 'a) -> 'a
  val main: int
  
  Bindings after transformations:
  val cc_ac1_main: int
  val fac_cps: int -> (int -> 'a) -> 'a
  val ll_0: int -> (int -> 'a) -> int -> 'a
  val ll_1: 'a -> 'a
  
  ------------------------------
  
  let ll_0 cc0_n cc1_k p =
    cc1_k ((( * ) p) cc0_n)
  ;;
  
  let rec fac_cps n k =
    (if (( = ) n) 1
    then
      k 1
    else
      (fac_cps ((( - ) n) 1)) ((ll_0 n) k))
  ;;
  
  let ll_1 cc_ac0_print_int =
    cc_ac0_print_int
  ;;
  
  let cc_ac1_main = let () = print_int ((fac_cps 4) ll_1) in
      0
  ;;
  

  $ ./ll_runner.exe < manytests/typed/003fib.ml
  Bindings before transformations:
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  val main: int
  
  Bindings after transformations:
  val cc_ac1_main: int
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  
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
  

  $ ./ll_runner.exe < manytests/typed/004manyargs.ml
  Bindings before transformations:
  val main: int
  val test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3: int -> int -> int -> int
  val wrap: 'a -> 'a
  
  Bindings after transformations:
  val cc_ac7_main: int
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
  

  $ ./ll_runner.exe < manytests/typed/005fix.ml
  Bindings before transformations:
  val fac: (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main: int
  
  Bindings after transformations:
  val cc_ac0_main: int
  val fac: (int -> int) -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  
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
  

  $ ./ll_runner.exe < manytests/typed/006partial.ml
  Bindings before transformations:
  val foo: int -> int
  val main: int
  
  Bindings after transformations:
  val cc_ac1_foo: bool -> int -> int
  val cc_ac2_foo: int -> int
  val cc_ac3_main: int
  val ll_0: int -> int
  val ll_1: int -> int
  
  ------------------------------
  
  let ll_0 foo =
    (( + ) foo) 2
  ;;
  
  let ll_1 cc_ac0_foo =
    (( * ) cc_ac0_foo) 10
  ;;
  
  let cc_ac1_foo b =
    (if b
    then
      ll_0
    else
      ll_1)
  ;;
  
  let cc_ac2_foo x =
    (cc_ac1_foo true) ((cc_ac1_foo false) ((cc_ac1_foo true) ((cc_ac1_foo false) x)))
  ;;
  
  let cc_ac3_main = let () = print_int (cc_ac2_foo 11) in
      0
  ;;
  

  $ ./ll_runner.exe < manytests/typed/006partial2.ml
  Bindings before transformations:
  val foo: int -> int -> int -> int
  val main: int
  
  Bindings after transformations:
  val cc_ac3_main: int
  val foo: int -> int -> int -> int
  
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
  

  $ ./ll_runner.exe < manytests/typed/006partial3.ml
  Bindings before transformations:
  val foo: int -> int -> int -> unit
  val main: int
  
  Bindings after transformations:
  val cc_ac0_main: int
  val foo: int -> int -> int -> unit
  val ll_0: int -> int -> unit
  val ll_1: int -> unit
  
  ------------------------------
  
  let ll_1 c =
    print_int c
  ;;
  
  let ll_0 b =
    let () = print_int b in
      ll_1
  ;;
  
  let foo a =
    let () = print_int a in
      ll_0
  ;;
  
  let cc_ac0_main = let () = ((foo 4) 8) 9 in
      0
  ;;
  

  $ ./ll_runner.exe < manytests/typed/007order.ml
  Bindings before transformations:
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit
  
  Bindings after transformations:
  val cc_ac0__start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val cc_ac1_main: unit
  
  ------------------------------
  
  let cc_ac0__start () () a () b _c () d __ =
    let () = print_int ((( + ) a) b) in
      let () = print_int __ in
        (( + ) (( / ) (( * ) a) b) _c) d
  ;;
  
  let cc_ac1_main = print_int (((((((((cc_ac0__start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (~- 1))) 10000) (~- 555555))
  ;;
  

  $ ./ll_runner.exe < manytests/typed/008ascription.ml
  Bindings before transformations:
  val addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main: int
  
  Bindings after transformations:
  val addi: ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val cc_ac2_main: int
  val ll_0: int -> bool -> int
  val ll_1: int -> bool
  
  ------------------------------
  
  let addi f g x =
    (f x) (g x)
  ;;
  
  let ll_0 cc_ac0_x b =
    (if b
    then
      (( + ) cc_ac0_x) 1
    else
      (( * ) cc_ac0_x) 2)
  ;;
  
  let ll_1 cc_ac1__start =
    (( = ) (( / ) cc_ac1__start) 2) 0
  ;;
  
  let cc_ac2_main = let () = print_int (((addi ll_0) ll_1) 4) in
      0
  ;;
  

  $ ./ll_runner.exe < manytests/typed/009let_poly.ml
  Bindings before transformations:
  val temp: int * bool
  
  Bindings after transformations:
  val f: 'a -> 'a
  val temp: int * bool
  
  ------------------------------
  
  let f x =
    x
  ;;
  
  let temp = (f 1, f true)
  ;;
  

  $ ./ll_runner.exe < manytests/typed/012fibcps.ml
  Bindings before transformations:
  val fib: int -> (int -> 'a) -> 'a
  val main: unit
  
  Unbound value 'fib'

  $ ./ll_runner.exe < manytests/typed/015tuples.ml
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
  val cc_ac10_main: int
  val cc_ac_meven: int -> int
  val feven: 'a -> int -> int
  val fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly: 'a -> ('b -> 'c) * ('b -> 'c)
  val fodd: 'a -> int -> int
  val ll_0: ('a -> 'b) -> 'a -> ('c -> 'd) * ('c -> 'd)
  val ll_1: ('a -> 'b) -> 'a -> ('b -> 'c -> 'd) -> 'c -> 'd
  val map: ('a -> 'b) -> 'c -> 'b * 'b
  val modd: int -> int
  val tie: ('a -> 'b) * ('a -> 'b)
  
  ------------------------------
  
  let rec fix f x =
    (f (fix f)) x
  ;;
  
  let map cc_ac0_f p =
    let me_4 = p in
      let a = (get_by_idx me_4) 0 in
        let b = (get_by_idx me_4) 1 in
          (cc_ac0_f a, cc_ac0_f b)
  ;;
  
  let ll_1 cc0_self cc1_cc_ac1_l li cc_ac2_x =
    (li (cc0_self cc1_cc_ac1_l)) cc_ac2_x
  ;;
  
  let ll_0 self cc_ac1_l =
    (map ((ll_1 self) cc_ac1_l)) cc_ac1_l
  ;;
  
  let fixpoly l =
    (fix ll_0) l
  ;;
  
  let feven cc_ac3_p n =
    let me_16 = cc_ac3_p in
      let e = (get_by_idx me_16) 0 in
        let o = (get_by_idx me_16) 1 in
          (if (( = ) n) 0
          then
            1
          else
            o ((( - ) n) 1))
  ;;
  
  let fodd cc_ac4_p cc_ac5_n =
    let me_20 = cc_ac4_p in
      let cc_ac6_e = (get_by_idx me_20) 0 in
        let cc_ac7_o = (get_by_idx me_20) 1 in
          (if (( = ) cc_ac5_n) 0
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
        let me_31 = tie in
          let even = (get_by_idx me_31) 0 in
            let odd = (get_by_idx me_31) 1 in
              let () = print_int (odd 3) in
                let () = print_int (even 4) in
                  0
  ;;
  

  $ ./ll_runner.exe < manytests/typed/016lists.ml
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
  val append: 'a list -> 'b list -> 'b list
  val cartesian: 'a list -> 'b list -> 'c list
  val cc_ac13_helper: 'a list -> 'b list
  val cc_ac26_main: int
  val concat: 'a list -> 'b list
  val helper: int -> 'a list -> int
  val iter: ('a -> unit) -> 'b list -> unit
  val length: 'a list -> int
  val length_tail: 'a list -> int
  val ll_0: 'a -> 'b -> 'a * 'b
  val map: ('a -> 'b) -> 'c list -> 'b list
  
  ------------------------------
  
  let rec length xs =
    (if (( = ) []) xs
    then
      0
    else
      (if (( >= ) get_list_len xs) 2
      then
        let tl = (get_list_tail xs) 1 in
          let h = (get_by_idx xs) 0 in
            (( + ) 1) (length tl)
      else
        part_match_fail ()))
  ;;
  
  let rec helper cc_ac_acc cc_ac0_xs =
    (if (( = ) []) cc_ac0_xs
    then
      cc_ac_acc
    else
      (if (( >= ) get_list_len cc_ac0_xs) 2
      then
        let cc_ac2_tl = (get_list_tail cc_ac0_xs) 1 in
          let cc_ac1_h = (get_by_idx cc_ac0_xs) 0 in
            (helper ((( + ) cc_ac_acc) 1)) cc_ac2_tl
      else
        part_match_fail ()))
  ;;
  
  let length_tail = helper 0
  ;;
  
  let rec map f cc_ac3_xs =
    (if (( = ) []) cc_ac3_xs
    then
      []
    else
      (if (( = ) 1) (get_list_len cc_ac3_xs)
      then
        let a = (get_by_idx cc_ac3_xs) 0 in
          (f a :: [])
      else
        (if (( = ) 2) (get_list_len cc_ac3_xs)
        then
          let cc_ac4_a = (get_by_idx cc_ac3_xs) 0 in
            let b = (get_by_idx cc_ac3_xs) 1 in
              (f cc_ac4_a :: (f b :: []))
        else
          (if (( = ) 3) (get_list_len cc_ac3_xs)
          then
            let cc_ac5_a = (get_by_idx cc_ac3_xs) 0 in
              let cc_ac6_b = (get_by_idx cc_ac3_xs) 1 in
                let c = (get_by_idx cc_ac3_xs) 2 in
                  (f cc_ac5_a :: (f cc_ac6_b :: (f c :: [])))
          else
            (if (( >= ) get_list_len cc_ac3_xs) 5
            then
              let cc_ac10_tl = (get_list_tail cc_ac3_xs) 4 in
                let cc_ac7_a = (get_by_idx cc_ac3_xs) 0 in
                  let cc_ac8_b = (get_by_idx cc_ac3_xs) 1 in
                    let cc_ac9_c = (get_by_idx cc_ac3_xs) 2 in
                      let d = (get_by_idx cc_ac3_xs) 3 in
                        (f cc_ac7_a :: (f cc_ac8_b :: (f cc_ac9_c :: (f d :: (map f) cc_ac10_tl))))
            else
              part_match_fail ())))))
  ;;
  
  let rec append cc_ac11_xs ys =
    (if (( = ) []) cc_ac11_xs
    then
      ys
    else
      (if (( >= ) get_list_len cc_ac11_xs) 2
      then
        let cc_ac12_xs = (get_list_tail cc_ac11_xs) 1 in
          let x = (get_by_idx cc_ac11_xs) 0 in
            (x :: (append cc_ac12_xs) ys)
      else
        part_match_fail ()))
  ;;
  
  let rec cc_ac13_helper cc_ac14_xs =
    (if (( = ) []) cc_ac14_xs
    then
      []
    else
      (if (( >= ) get_list_len cc_ac14_xs) 2
      then
        let cc_ac16_tl = (get_list_tail cc_ac14_xs) 1 in
          let cc_ac15_h = (get_by_idx cc_ac14_xs) 0 in
            (append cc_ac15_h) (cc_ac13_helper cc_ac16_tl)
      else
        part_match_fail ()))
  ;;
  
  let concat = cc_ac13_helper
  ;;
  
  let rec iter cc_ac17_f cc_ac18_xs =
    (if (( = ) []) cc_ac18_xs
    then
      ()
    else
      (if (( >= ) get_list_len cc_ac18_xs) 2
      then
        let cc_ac20_tl = (get_list_tail cc_ac18_xs) 1 in
          let cc_ac19_h = (get_by_idx cc_ac18_xs) 0 in
            let () = cc_ac17_f cc_ac19_h in
              (iter cc_ac17_f) cc_ac20_tl
      else
        part_match_fail ()))
  ;;
  
  let ll_0 cc0_cc_ac23_h cc_ac25_a =
    (cc0_cc_ac23_h, cc_ac25_a)
  ;;
  
  let rec cartesian cc_ac21_xs cc_ac22_ys =
    (if (( = ) []) cc_ac21_xs
    then
      []
    else
      (if (( >= ) get_list_len cc_ac21_xs) 2
      then
        let cc_ac24_tl = (get_list_tail cc_ac21_xs) 1 in
          let cc_ac23_h = (get_by_idx cc_ac21_xs) 0 in
            (append ((map (ll_0 cc_ac23_h)) cc_ac22_ys)) ((cartesian cc_ac24_tl) cc_ac22_ys)
      else
        part_match_fail ()))
  ;;
  
  let cc_ac26_main = let () = (iter print_int) (1 :: (2 :: (3 :: []))) in
      let () = print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: [])))))) in
        0
  ;;
  


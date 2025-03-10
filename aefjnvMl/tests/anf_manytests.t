  $ ./anf_runner.exe < manytests/do_not_type/001.ml
  Unbound value 'fac'

  $ ./anf_runner.exe < manytests/do_not_type/002if.ml
  This expression has type bool but an expression was expected of type int

  $ ./anf_runner.exe < manytests/do_not_type/003occurs.ml
  The type variable 'a occurs inside 'a -> 'b

  $ ./anf_runner.exe < manytests/do_not_type/004let_poly.ml
  This expression has type int but an expression was expected of type bool

  $ ./anf_runner.exe < manytests/do_not_type/015tuples.ml
  Only variables are allowed as left-side of 'let rec'

  $ ./anf_runner.exe < manytests/typed/001fac.ml
  Bindings before transformations:
  val fac: int -> int
  val main: int
  
  Bindings after transformations:
  val fac: int -> int
  val main: int
  
  ------------------------------
  
  let rec fac n =
    let nf_0 = (( <= ) n) 1 in
      (if nf_0
      then
        1
      else
        let nf_2 = (( - ) n) 1 in
          let nf_1 = fac nf_2 in
            (( * ) n) nf_1)
  ;;
  
  let cc_ac0_main = let nf_3 = fac 4 in
      let () = print_int nf_3 in
        0
  ;;
  

  $ ./anf_runner.exe < manytests/typed/002fac.ml
  Bindings before transformations:
  val fac_cps: int -> (int -> 'a) -> 'a
  val main: int
  
  Bindings after transformations:
  val fac_cps: int -> (int -> 'a) -> 'a
  val main: int
  
  ------------------------------
  
  let ll_0 cc0_n cc1_k p =
    let nf_0 = (( * ) p) cc0_n in
      cc1_k nf_0
  ;;
  
  let rec fac_cps n k =
    let nf_1 = (( = ) n) 1 in
      (if nf_1
      then
        k 1
      else
        let nf_3 = (ll_0 n) k in
          let nf_2 = (( - ) n) 1 in
            (fac_cps nf_2) nf_3)
  ;;
  
  let ll_1 cc_ac0_print_int =
    cc_ac0_print_int
  ;;
  
  let cc_ac1_main = let nf_4 = (fac_cps 4) ll_1 in
      let () = print_int nf_4 in
        0
  ;;
  

  $ ./anf_runner.exe < manytests/typed/003fib.ml
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
    let nf_0 = (( = ) n) 1 in
      (if nf_0
      then
        b
      else
        let n1 = (( - ) n) 1 in
          let ab = (( + ) a) b in
            ((fib_acc b) ab) n1)
  ;;
  
  let rec fib cc_ac0_n =
    let nf_1 = (( < ) cc_ac0_n) 2 in
      (if nf_1
      then
        cc_ac0_n
      else
        let nf_5 = (( - ) cc_ac0_n) 2 in
          let nf_4 = fib nf_5 in
            let nf_3 = (( - ) cc_ac0_n) 1 in
              let nf_2 = fib nf_3 in
                (( + ) nf_2) nf_4)
  ;;
  
  let cc_ac1_main = let nf_6 = ((fib_acc 0) 1) 4 in
      let () = print_int nf_6 in
        let nf_7 = fib 4 in
          let () = print_int nf_7 in
            0
  ;;
  

  $ ./anf_runner.exe < manytests/typed/004manyargs.ml
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
    let nf_0 = (( = ) 1) 1 in
      (if nf_0
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
    let nf_8 = (( + ) cc_ac3_a) cc_ac4_b in
      let nf_7 = (( + ) nf_8) cc_ac5_c in
        let nf_6 = (( + ) nf_7) d in
          let nf_5 = (( + ) nf_6) e in
            let nf_4 = (( + ) nf_5) cc_ac6_f in
              let nf_3 = (( + ) nf_4) g in
                let nf_2 = (( + ) nf_3) h in
                  let nf_1 = (( + ) nf_2) i in
                    (( + ) nf_1) j
  ;;
  
  let cc_ac7_main = let rez = ((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000 in
      let () = print_int rez in
        let temp2 = (((wrap test3) 1) 10) 100 in
          0
  ;;
  

  $ ./anf_runner.exe < manytests/typed/005fix.ml
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
    let nf_0 = fix f in
      (f nf_0) x
  ;;
  
  let fac self n =
    let nf_1 = (( <= ) n) 1 in
      (if nf_1
      then
        1
      else
        let nf_3 = (( - ) n) 1 in
          let nf_2 = self nf_3 in
            (( * ) n) nf_2)
  ;;
  
  let cc_ac0_main = let nf_4 = (fix fac) 6 in
      let () = print_int nf_4 in
        0
  ;;
  

  $ ./anf_runner.exe < manytests/typed/006partial.ml
  Bindings before transformations:
  val foo: int -> int
  val main: int
  
  Bindings after transformations:
  val foo: int -> int
  val main: int
  
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
    let nf_2 = (cc_ac1_foo false) x in
      let nf_1 = (cc_ac1_foo true) nf_2 in
        let nf_0 = (cc_ac1_foo false) nf_1 in
          (cc_ac1_foo true) nf_0
  ;;
  
  let cc_ac3_main = let nf_3 = cc_ac2_foo 11 in
      let () = print_int nf_3 in
        0
  ;;
  

  $ ./anf_runner.exe < manytests/typed/006partial2.ml
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
          let nf_0 = (( * ) b) c in
            (( + ) a) nf_0
  ;;
  
  let cc_ac3_main = let cc_ac0_foo = foo 1 in
      let cc_ac1_foo = cc_ac0_foo 2 in
        let cc_ac2_foo = cc_ac1_foo 3 in
          let () = print_int cc_ac2_foo in
            0
  ;;
  

  $ ./anf_runner.exe < manytests/typed/006partial3.ml
  Bindings before transformations:
  val foo: int -> int -> int -> unit
  val main: int
  
  Bindings after transformations:
  val foo: int -> int -> int -> unit
  val main: int
  
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
  

  $ ./anf_runner.exe < manytests/typed/007order.ml
  Bindings before transformations:
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit
  
  Bindings after transformations:
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit
  
  ------------------------------
  
  let cc_ac0__start () () a () b _c () d __ =
    let nf_0 = (( + ) a) b in
      let () = print_int nf_0 in
        let () = print_int __ in
          let nf_2 = (( * ) a) b in
            let nf_1 = (( / ) nf_2) _c in
              (( + ) nf_1) d
  ;;
  
  let cc_ac1_main = let nf_9 = ~- 555555 in
      let nf_8 = ~- 1 in
        let nf_7 = print_int nf_8 in
          let nf_6 = print_int 4 in
            let nf_5 = print_int 2 in
              let nf_4 = print_int 1 in
                let nf_3 = ((((((((cc_ac0__start nf_4) nf_5) 3) nf_6) 100) 1000) nf_7) 10000) nf_9 in
                  print_int nf_3
  ;;
  

  $ ./anf_runner.exe < manytests/typed/008ascription.ml
  Bindings before transformations:
  val addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main: int
  
  Bindings after transformations:
  val addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main: int
  
  ------------------------------
  
  let addi f g x =
    let nf_0 = g x in
      (f x) nf_0
  ;;
  
  let ll_0 cc_ac0_x b =
    (if b
    then
      (( + ) cc_ac0_x) 1
    else
      (( * ) cc_ac0_x) 2)
  ;;
  
  let ll_1 cc_ac1__start =
    let nf_1 = (( / ) cc_ac1__start) 2 in
      (( = ) nf_1) 0
  ;;
  
  let cc_ac2_main = let nf_2 = ((addi ll_0) ll_1) 4 in
      let () = print_int nf_2 in
        0
  ;;
  

  $ ./anf_runner.exe < manytests/typed/009let_poly.ml
  Bindings before transformations:
  val temp: int * bool
  
  Bindings after transformations:
  val temp: int * bool
  
  ------------------------------
  
  let f x =
    x
  ;;
  
  let temp = let nf_0 = f 1 in
      let nf_1 = f true in
        (nf_0, nf_1)
  ;;
  

  $ ./anf_runner.exe < manytests/typed/015tuples.ml
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
    let nf_0 = fix f in
      (f nf_0) x
  ;;
  
  let map cc_ac0_f p =
    let me_4 = p in
      let a = (get_by_idx me_4) 0 in
        let b = (get_by_idx me_4) 1 in
          let nf_1 = cc_ac0_f a in
            let nf_2 = cc_ac0_f b in
              (nf_1, nf_2)
  ;;
  
  let ll_1 cc0_self cc1_cc_ac1_l li cc_ac2_x =
    let nf_3 = cc0_self cc1_cc_ac1_l in
      (li nf_3) cc_ac2_x
  ;;
  
  let ll_0 self cc_ac1_l =
    let nf_4 = (ll_1 self) cc_ac1_l in
      (map nf_4) cc_ac1_l
  ;;
  
  let fixpoly l =
    (fix ll_0) l
  ;;
  
  let feven cc_ac3_p n =
    let me_16 = cc_ac3_p in
      let e = (get_by_idx me_16) 0 in
        let o = (get_by_idx me_16) 1 in
          let nf_5 = (( == ) n) 0 in
            (if nf_5
            then
              1
            else
              let nf_6 = (( - ) n) 1 in
                o nf_6)
  ;;
  
  let fodd cc_ac4_p cc_ac5_n =
    let me_20 = cc_ac4_p in
      let cc_ac6_e = (get_by_idx me_20) 0 in
        let cc_ac7_o = (get_by_idx me_20) 1 in
          let nf_7 = (( == ) cc_ac5_n) 0 in
            (if nf_7
            then
              0
            else
              let nf_8 = (( - ) cc_ac5_n) 1 in
                cc_ac6_e nf_8)
  ;;
  
  let tie = let nf_9 = (feven, fodd) in
      fixpoly nf_9
  ;;
  
  let rec cc_ac_meven cc_ac8_n =
    let nf_10 = (( = ) cc_ac8_n) 0 in
      (if nf_10
      then
        1
      else
        let nf_11 = (( - ) cc_ac8_n) 1 in
          modd nf_11)
  and modd cc_ac9_n =
    let nf_12 = (( = ) cc_ac9_n) 0 in
      (if nf_12
      then
        1
      else
        let nf_13 = (( - ) cc_ac9_n) 1 in
          cc_ac_meven nf_13)
  ;;
  
  let cc_ac10_main = let nf_14 = modd 1 in
      let () = print_int nf_14 in
        let nf_15 = cc_ac_meven 2 in
          let () = print_int nf_15 in
            let me_31 = tie in
              let even = (get_by_idx me_31) 0 in
                let odd = (get_by_idx me_31) 1 in
                  let nf_16 = odd 3 in
                    let () = print_int nf_16 in
                      let nf_17 = even 4 in
                        let () = print_int nf_17 in
                          0
  ;;
  

  $ ./anf_runner.exe < manytests/typed/016lists.ml
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
    let nf_0 = (( = ) []) xs in
      (if nf_0
      then
        0
      else
        let nf_2 = get_list_len xs in
          let nf_1 = (( >= ) nf_2) 2 in
            (if nf_1
            then
              let tl = (get_list_tail xs) 1 in
                let h = (get_by_idx xs) 0 in
                  let nf_3 = length tl in
                    (( + ) 1) nf_3
            else
              part_match_fail ()))
  ;;
  
  let rec helper cc_ac_acc cc_ac0_xs =
    let nf_4 = (( = ) []) cc_ac0_xs in
      (if nf_4
      then
        cc_ac_acc
      else
        let nf_6 = get_list_len cc_ac0_xs in
          let nf_5 = (( >= ) nf_6) 2 in
            (if nf_5
            then
              let cc_ac2_tl = (get_list_tail cc_ac0_xs) 1 in
                let cc_ac1_h = (get_by_idx cc_ac0_xs) 0 in
                  let nf_7 = (( + ) cc_ac_acc) 1 in
                    (helper nf_7) cc_ac2_tl
            else
              part_match_fail ()))
  ;;
  
  let length_tail = helper 0
  ;;
  
  let rec map f cc_ac3_xs =
    let nf_8 = (( = ) []) cc_ac3_xs in
      (if nf_8
      then
        []
      else
        let nf_10 = get_list_len cc_ac3_xs in
          let nf_9 = (( = ) 1) nf_10 in
            (if nf_9
            then
              let a = (get_by_idx cc_ac3_xs) 0 in
                let nf_11 = f a in
                  (nf_11 :: [])
            else
              let nf_13 = get_list_len cc_ac3_xs in
                let nf_12 = (( = ) 2) nf_13 in
                  (if nf_12
                  then
                    let cc_ac4_a = (get_by_idx cc_ac3_xs) 0 in
                      let b = (get_by_idx cc_ac3_xs) 1 in
                        let nf_14 = f b in
                          let nf_15 = f cc_ac4_a in
                            (nf_15 :: (nf_14 :: []))
                  else
                    let nf_17 = get_list_len cc_ac3_xs in
                      let nf_16 = (( = ) 3) nf_17 in
                        (if nf_16
                        then
                          let cc_ac5_a = (get_by_idx cc_ac3_xs) 0 in
                            let cc_ac6_b = (get_by_idx cc_ac3_xs) 1 in
                              let c = (get_by_idx cc_ac3_xs) 2 in
                                let nf_18 = f c in
                                  let nf_19 = f cc_ac6_b in
                                    let nf_20 = f cc_ac5_a in
                                      (nf_20 :: (nf_19 :: (nf_18 :: [])))
                        else
                          let nf_22 = get_list_len cc_ac3_xs in
                            let nf_21 = (( >= ) nf_22) 5 in
                              (if nf_21
                              then
                                let cc_ac10_tl = (get_list_tail cc_ac3_xs) 4 in
                                  let cc_ac7_a = (get_by_idx cc_ac3_xs) 0 in
                                    let cc_ac8_b = (get_by_idx cc_ac3_xs) 1 in
                                      let cc_ac9_c = (get_by_idx cc_ac3_xs) 2 in
                                        let d = (get_by_idx cc_ac3_xs) 3 in
                                          let nf_23 = (map f) cc_ac10_tl in
                                            let nf_24 = f d in
                                              let nf_25 = f cc_ac9_c in
                                                let nf_26 = f cc_ac8_b in
                                                  let nf_27 = f cc_ac7_a in
                                                    (nf_27 :: (nf_26 :: (nf_25 :: (nf_24 :: nf_23))))
                              else
                                part_match_fail ())))))
  ;;
  
  let rec append cc_ac11_xs ys =
    let nf_28 = (( = ) []) cc_ac11_xs in
      (if nf_28
      then
        ys
      else
        let nf_30 = get_list_len cc_ac11_xs in
          let nf_29 = (( >= ) nf_30) 2 in
            (if nf_29
            then
              let cc_ac12_xs = (get_list_tail cc_ac11_xs) 1 in
                let x = (get_by_idx cc_ac11_xs) 0 in
                  let nf_31 = (append cc_ac12_xs) ys in
                    (x :: nf_31)
            else
              part_match_fail ()))
  ;;
  
  let rec cc_ac13_helper cc_ac14_xs =
    let nf_32 = (( = ) []) cc_ac14_xs in
      (if nf_32
      then
        []
      else
        let nf_34 = get_list_len cc_ac14_xs in
          let nf_33 = (( >= ) nf_34) 2 in
            (if nf_33
            then
              let cc_ac16_tl = (get_list_tail cc_ac14_xs) 1 in
                let cc_ac15_h = (get_by_idx cc_ac14_xs) 0 in
                  let nf_35 = cc_ac13_helper cc_ac16_tl in
                    (append cc_ac15_h) nf_35
            else
              part_match_fail ()))
  ;;
  
  let concat = cc_ac13_helper
  ;;
  
  let rec iter cc_ac17_f cc_ac18_xs =
    let nf_36 = (( = ) []) cc_ac18_xs in
      (if nf_36
      then
        ()
      else
        let nf_38 = get_list_len cc_ac18_xs in
          let nf_37 = (( >= ) nf_38) 2 in
            (if nf_37
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
    let nf_39 = (( = ) []) cc_ac21_xs in
      (if nf_39
      then
        []
      else
        let nf_41 = get_list_len cc_ac21_xs in
          let nf_40 = (( >= ) nf_41) 2 in
            (if nf_40
            then
              let cc_ac24_tl = (get_list_tail cc_ac21_xs) 1 in
                let cc_ac23_h = (get_by_idx cc_ac21_xs) 0 in
                  let nf_44 = (cartesian cc_ac24_tl) cc_ac22_ys in
                    let nf_43 = ll_0 cc_ac23_h in
                      let nf_42 = (map nf_43) cc_ac22_ys in
                        (append nf_42) nf_44
            else
              part_match_fail ()))
  ;;
  
  let cc_ac26_main = let nf_45 = (1 :: (2 :: (3 :: []))) in
      let () = (iter print_int) nf_45 in
        let nf_49 = (1 :: (2 :: (3 :: (4 :: [])))) in
          let nf_48 = (1 :: (2 :: [])) in
            let nf_47 = (cartesian nf_48) nf_49 in
              let nf_46 = length nf_47 in
                let () = print_int nf_46 in
                  0
  ;;
  


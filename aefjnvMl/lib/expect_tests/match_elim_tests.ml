(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open AefjnvMl_lib

open Top_utils.Ast_test_utils
open Middleend

let me_test s =
  (* use [let*!] and [let+!] to exclude infer from pipeline *)
  let ast'_t =
    let* ast = Parser.parse s in
    let* ast' = Alpha_converter.rename_ast_with_uniq Common.Naming.alpha_prefix ast in
    let ast' =
      let open Match_elimination in
      let+! m_ast = Match_elim.eliminate_match_in_program ast' in
      let m_ast' = Optimizations.optimize m_ast in
      Me_converter.convert_me_program m_ast'
    in
    ast'
  in
  (* use [Common.Ast.pp_program] to show ast *)
  let ast_printer ast_ = Format.printf "%a\n" Common.Ast_pp.pp_program ast_ in
  print_result ast_printer ast'_t
;;

let%expect_test "" =
  let () = me_test {|
let a = 1;;
let b = 1;;
let c = a + b;;
  |} in
  [%expect
    {|
    let a = 1
    ;;

    let b = 1
    ;;

    let c = (( + ) a) b
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let rec even n =
  match n with
    | 0 -> true
    | x -> odd (x-1)
and odd n =
  match n with
    | 0 -> false
    | x -> even (x-1);;

  |}
  in
  [%expect
    {|
    let rec even n =
      (if (( = ) 0) n
      then
        true
      else
        let x = n in
          odd ((( - ) x) 1))
    and odd ac0_n =
      (if (( = ) 0) ac0_n
      then
        false
      else
        let ac1_x = ac0_n in
          even ((( - ) ac1_x) 1))
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
let even n =
  match n with
    | 0 -> 0
    | x -> (x-1)
;;
  |} in
  [%expect
    {|
    let even n =
      (if (( = ) 0) n
      then
        0
      else
        let x = n in
          (( - ) x) 1)
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let rec len l = 
  match l with
  | [] -> 0
  | _::tl -> 1 + (len tl)
;;
  |}
  in
  [%expect
    {|
        let rec len l =
          (if (( = ) []) l
          then
            0
          else
            (if (( >= ) get_list_len l) 2
            then
              let tl = (get_list_tail l) 1 in
                (( + ) 1) (len tl)
            else
              part_match_fail ()))
        ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
if true then 1 else 2;;
  |} in
  [%expect {|
    let me_0 = 1
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
if true && false then 1 else 2;;
  |} in
  [%expect {|
    let me_0 = 2
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
fun x -> if true then 1 else 2;;
  |} in
  [%expect {|
    let me_0 x =
      1
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
let (c, b) = (1, 2)
;;
  |} in
  [%expect
    {|
    let me_0 = (1, 2)
    ;;

    let c = (get_by_idx me_0) 0
    ;;

    let b = (get_by_idx me_0) 1
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
let a = 2
;; 
  |} in
  [%expect {|
    let a = 2
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
let ab = [[1]; [2]] in
match ab with 
| (a::c)::b -> 1
;; 
  |} in
  [%expect
    {|
    let me_0 = let ab = ((1 :: []) :: ((2 :: []) :: [])) in
        (if (if (( >= ) get_list_len ab) 2
          then
            (( >= ) get_list_len ((get_by_idx ab) 0)) 2
          else
            false)
        then
          let b = (get_list_tail ab) 1 in
            let c = (get_list_tail ((get_by_idx ab) 0)) 1 in
              let a = (get_by_idx ((get_by_idx ab) 0)) 0 in
                1
        else
          part_match_fail ())
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
let ab = (1, 2) in
match ab with 
| a, b -> 1
;; 
  |} in
  [%expect
    {|
    let me_0 = let ab = (1, 2) in
        let a = (get_by_idx ab) 0 in
          let b = (get_by_idx ab) 1 in
            1
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
let f (a, b) = a + b
;; 
  |} in
  [%expect
    {|
    let f me_1 =
      let a = (get_by_idx me_1) 0 in
        let b = (get_by_idx me_1) 1 in
          (( + ) a) b
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let rec fac n = if n<=1 then 1 else n * fac (n-1)

let main =
  let () = print_int (fac 4) in
  0

  |}
  in
  [%expect
    {|
    let rec fac n =
      (if (( <= ) n) 1
      then
        1
      else
        (( * ) n) (fac ((( - ) n) 1)))
    ;;

    let ac0_main = let () = print_int (fac 4) in
        0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let rec fac_cps n k =
  if n=1 then k 1 else
  fac_cps (n-1) (fun p -> k (p*n))

let main =
  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
  0

  |}
  in
  [%expect
    {|
    let rec fac_cps n k =
      (if (( = ) n) 1
      then
        k 1
      else
        (fac_cps ((( - ) n) 1)) (fun p -> k ((( * ) p) n)))
    ;;

    let ac1_main = let () = print_int ((fac_cps 4) (fun ac0_print_int -> ac0_print_int)) in
        0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let rec fib_acc a b n =
  if n=1 then b
  else
    let n1 = n-1 in
    let ab = a+b in
    fib_acc b ab n1

let rec fib n =
  if n<2
  then n
  else fib (n - 1) + fib (n - 2) 

let main =
  let () = print_int (fib_acc 0 1 4) in
  let () = print_int (fib 4) in
  0
  |}
  in
  [%expect
    {|
    let rec fib_acc a b n =
      (if (( = ) n) 1
      then
        b
      else
        let n1 = (( - ) n) 1 in
          let ab = (( + ) a) b in
            ((fib_acc b) ab) n1)
    ;;

    let rec fib ac0_n =
      (if (( < ) ac0_n) 2
      then
        ac0_n
      else
        (( + ) fib ((( - ) ac0_n) 1)) (fib ((( - ) ac0_n) 2)))
    ;;

    let ac1_main = let () = print_int (((fib_acc 0) 1) 4) in
        let () = print_int (fib 4) in
          0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let wrap f = if 1 = 1 then f else f

let test3 a b c =
  let a = print_int a in
  let b = print_int b in
  let c = print_int c in
  0

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let rez =
      (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
         1000000000)
  in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0

;; 
  |}
  in
  [%expect
    {|
    let wrap f =
      (if (( = ) 1) 1
      then
        f
      else
        f)
    ;;

    let test3 a b c =
      let ac0_a = print_int a in
        let ac1_b = print_int b in
          let ac2_c = print_int c in
            0
    ;;

    let test10 ac3_a ac4_b ac5_c d e ac6_f g h i j =
      (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) (( + ) ac3_a) ac4_b) ac5_c) d) e) ac6_f) g) h) i) j
    ;;

    let ac7_main = let rez = ((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000 in
        let () = print_int rez in
          let temp2 = (((wrap test3) 1) 10) 100 in
            0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let rec fix f x = f (fix f) x

let fac self n = if n<=1 then 1 else n * self (n-1)

let main =
  let () = print_int (fix fac 6) in
  0
;; 
  |}
  in
  [%expect
    {|
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

    let ac0_main = let () = print_int ((fix fac) 6) in
        0
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
let temp =
  let f = fun x -> x in
  (f 1, f true)
  |} in
  [%expect {|
    let temp = let f x =
        x in
        (f 1, f true)
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

let foo x = foo true (foo false (foo true (foo false x)))
let main =
  let () = print_int (foo 11) in
  0
  |}
  in
  [%expect
    {|
    let ac1_foo b =
      (if b
      then
        (fun foo -> (( + ) foo) 2)
      else
        (fun ac0_foo -> (( * ) ac0_foo) 10))
    ;;

    let ac2_foo x =
      (ac1_foo true) ((ac1_foo false) ((ac1_foo true) ((ac1_foo false) x)))
    ;;

    let ac3_main = let () = print_int (ac2_foo 11) in
        0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

let foo x = foo true (foo false (foo true (foo false x)))
let main =
  let () = print_int (foo 11) in
  0
  |}
  in
  [%expect
    {|
    let ac1_foo b =
      (if b
      then
        (fun foo -> (( + ) foo) 2)
      else
        (fun ac0_foo -> (( * ) ac0_foo) 10))
    ;;

    let ac2_foo x =
      (ac1_foo true) ((ac1_foo false) ((ac1_foo true) ((ac1_foo false) x)))
    ;;

    let ac3_main = let () = print_int (ac2_foo 11) in
        0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let foo a b c =
  let () = print_int a in
  let () = print_int b in
  let () = print_int c in
  a + b * c

let main =
  let foo = foo 1 in
  let foo = foo 2 in
  let foo = foo 3 in
  let () = print_int foo in
  0
  |}
  in
  [%expect
    {|
    let foo a b c =
      let () = print_int a in
        let () = print_int b in
          let () = print_int c in
            (( + ) a) ((( * ) b) c)
    ;;

    let ac3_main = let ac0_foo = foo 1 in
        let ac1_foo = ac0_foo 2 in
          let ac2_foo = ac1_foo 3 in
            let () = print_int ac2_foo in
              0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let foo a =
  let () = print_int a in fun b ->
  let () = print_int b in fun c ->
  print_int c

let main =
  let () = foo 4 8 9 in
  0
  |}
  in
  [%expect
    {|
    let foo a =
      let () = print_int a in
        (fun b -> let () = print_int b in
          (fun c -> print_int c))
    ;;

    let ac0_main = let () = ((foo 4) 8) 9 in
        0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let _start () () a () b _c () d __ =
  let () = print_int (a+b) in
  let () = print_int __ in
  a*b / _c + d


let main =
  print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
  |}
  in
  [%expect
    {|
    let ac0__start () () a () b _c () d __ =
      let () = print_int ((( + ) a) b) in
        let () = print_int __ in
          (( + ) (( / ) (( * ) a) b) _c) d
    ;;

    let ac1_main = print_int (((((((((ac0__start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (~- 1))) 10000) (~- 555555))
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let addi = fun f g x -> (f x (g x: bool) : int)

let main =
  let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
  0
  |}
  in
  [%expect
    {|
    let addi f g x =
      (f x) (g x)
    ;;

    let ac2_main = let () = print_int (((addi (fun ac0_x b -> (if b
        then
          (( + ) ac0_x) 1
        else
          (( * ) ac0_x) 2))) (fun ac1__start -> (( = ) (( / ) ac1__start) 2) 0)) 4) in
        0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let rec length xs =
  match xs with
  | [] -> 0
  | h::tl -> 1 + length tl

let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0

let rec map f xs =
  match xs with
  | [] -> []
  | a::[] -> [f a]
  | a::b::[] -> [f a; f b]
  | a::b::c::[] -> [f a; f b; f c]
  | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl

let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

let concat =
  let rec helper xs =
    match xs with
    | [] -> []
    | h::tl -> append h (helper tl)
  in helper

let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl

let rec cartesian xs ys =
  match xs with
  | [] -> []
  | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)

let main =
  let () = iter print_int [1;2;3] in
  let () = print_int (length (cartesian [1;2] [1;2;3;4])) in
  0
  |}
  in
  [%expect
    {|
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

    let length_tail = let rec helper ac_acc ac0_xs =
        (if (( = ) []) ac0_xs
        then
          ac_acc
        else
          (if (( >= ) get_list_len ac0_xs) 2
          then
            let ac2_tl = (get_list_tail ac0_xs) 1 in
              let ac1_h = (get_by_idx ac0_xs) 0 in
                (helper ((( + ) ac_acc) 1)) ac2_tl
          else
            part_match_fail ())) in
        helper 0
    ;;

    let rec map f ac3_xs =
      (if (( = ) []) ac3_xs
      then
        []
      else
        (if (( = ) 1) (get_list_len ac3_xs)
        then
          let a = (get_by_idx ac3_xs) 0 in
            (f a :: [])
        else
          (if (( = ) 2) (get_list_len ac3_xs)
          then
            let ac4_a = (get_by_idx ac3_xs) 0 in
              let b = (get_by_idx ac3_xs) 1 in
                (f ac4_a :: (f b :: []))
          else
            (if (( = ) 3) (get_list_len ac3_xs)
            then
              let ac5_a = (get_by_idx ac3_xs) 0 in
                let ac6_b = (get_by_idx ac3_xs) 1 in
                  let c = (get_by_idx ac3_xs) 2 in
                    (f ac5_a :: (f ac6_b :: (f c :: [])))
            else
              (if (( >= ) get_list_len ac3_xs) 5
              then
                let ac10_tl = (get_list_tail ac3_xs) 4 in
                  let ac7_a = (get_by_idx ac3_xs) 0 in
                    let ac8_b = (get_by_idx ac3_xs) 1 in
                      let ac9_c = (get_by_idx ac3_xs) 2 in
                        let d = (get_by_idx ac3_xs) 3 in
                          (f ac7_a :: (f ac8_b :: (f ac9_c :: (f d :: (map f) ac10_tl))))
              else
                part_match_fail ())))))
    ;;

    let rec append ac11_xs ys =
      (if (( = ) []) ac11_xs
      then
        ys
      else
        (if (( >= ) get_list_len ac11_xs) 2
        then
          let ac12_xs = (get_list_tail ac11_xs) 1 in
            let x = (get_by_idx ac11_xs) 0 in
              (x :: (append ac12_xs) ys)
        else
          part_match_fail ()))
    ;;

    let concat = let rec ac13_helper ac14_xs =
        (if (( = ) []) ac14_xs
        then
          []
        else
          (if (( >= ) get_list_len ac14_xs) 2
          then
            let ac16_tl = (get_list_tail ac14_xs) 1 in
              let ac15_h = (get_by_idx ac14_xs) 0 in
                (append ac15_h) (ac13_helper ac16_tl)
          else
            part_match_fail ())) in
        ac13_helper
    ;;

    let rec iter ac17_f ac18_xs =
      (if (( = ) []) ac18_xs
      then
        ()
      else
        (if (( >= ) get_list_len ac18_xs) 2
        then
          let ac20_tl = (get_list_tail ac18_xs) 1 in
            let ac19_h = (get_by_idx ac18_xs) 0 in
              let () = ac17_f ac19_h in
                (iter ac17_f) ac20_tl
        else
          part_match_fail ()))
    ;;

    let rec cartesian ac21_xs ac22_ys =
      (if (( = ) []) ac21_xs
      then
        []
      else
        (if (( >= ) get_list_len ac21_xs) 2
        then
          let ac24_tl = (get_list_tail ac21_xs) 1 in
            let ac23_h = (get_by_idx ac21_xs) 0 in
              (append ((map (fun ac25_a -> (ac23_h, ac25_a))) ac22_ys)) ((cartesian ac24_tl) ac22_ys)
        else
          part_match_fail ()))
    ;;

    let ac26_main = let () = (iter print_int) (1 :: (2 :: (3 :: []))) in
        let () = print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: [])))))) in
          0
    ;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let rec fix f x = f (fix f) x
let map f p = let (a,b) = p in (f a, f b)
let fixpoly l =
  fix (fun self l -> map (fun li x -> li (self l) x) l) l
let feven p n =
  let (e, o) = p in
  if n == 0 then 1 else o (n - 1)
let fodd p n =
  let (e, o) = p in
  if n == 0 then 0 else e (n - 1)
let tie = fixpoly (feven, fodd)

let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1)
let main =
  let () = print_int (modd 1) in
  let () = print_int (meven 2) in
  let (even,odd) = tie in
  let () = print_int (odd 3) in
  let () = print_int (even 4) in
  0
  |}
  in
  [%expect
    {|
    let rec fix f x =
      (f (fix f)) x
    ;;

    let map ac0_f p =
      let me_4 = p in
        let a = (get_by_idx me_4) 0 in
          let b = (get_by_idx me_4) 1 in
            (ac0_f a, ac0_f b)
    ;;

    let fixpoly l =
      (fix (fun self ac1_l -> (map (fun li ac2_x -> (li (self ac1_l)) ac2_x)) ac1_l)) l
    ;;

    let feven ac3_p n =
      let me_14 = ac3_p in
        let e = (get_by_idx me_14) 0 in
          let o = (get_by_idx me_14) 1 in
            (if (( == ) n) 0
            then
              1
            else
              o ((( - ) n) 1))
    ;;

    let fodd ac4_p ac5_n =
      let me_18 = ac4_p in
        let ac6_e = (get_by_idx me_18) 0 in
          let ac7_o = (get_by_idx me_18) 1 in
            (if (( == ) ac5_n) 0
            then
              0
            else
              ac6_e ((( - ) ac5_n) 1))
    ;;

    let tie = fixpoly (feven, fodd)
    ;;

    let rec ac_meven ac8_n =
      (if (( = ) ac8_n) 0
      then
        1
      else
        modd ((( - ) ac8_n) 1))
    and modd ac9_n =
      (if (( = ) ac9_n) 0
      then
        1
      else
        ac_meven ((( - ) ac9_n) 1))
    ;;

    let ac10_main = let () = print_int (modd 1) in
        let () = print_int (ac_meven 2) in
          let me_29 = tie in
            let even = (get_by_idx me_29) 0 in
              let odd = (get_by_idx me_29) 1 in
                let () = print_int (odd 3) in
                  let () = print_int (even 4) in
                    0
    ;; |}]
;;

let%expect_test "" =
  let () = me_test {|
match () with
| () -> 1
  |} in
  [%expect {|
    let me_0 = let () = () in
        1
    ;; |}]
;;

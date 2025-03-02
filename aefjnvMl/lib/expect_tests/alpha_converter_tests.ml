(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open AefjnvMl_lib

open Top_utils.Ast_test_utils
open Middleend

let alpha_test s =
  let ast'_t =
    let* ast = Parser.parse s in
    let* ast' = Alpha_converter.rename_ast_with_uniq Common.Naming.alpha_prefix ast in
    Ok ast'
  in
  let ast_printer ast_ = Format.printf "%a\n" Common.Ast_pp.pp_program ast_ in
  print_result ast_printer ast'_t
;;

let%expect_test "" =
  let () = alpha_test {|
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
  let () = alpha_test {|
let a = 1;;
let b = a;;
let c = 
  let a = b in
  a
;;
  |} in
  [%expect
    {|
    let a = 1
    ;;

    let b = a
    ;;

    let c = let ac0_a = b in
        ac0_a
    ;; |}]
;;

let%expect_test "" =
  let () = alpha_test {|
let rt_a = 1
let ll_b = rt_a
let main = 1
;;
  |} in
  [%expect
    {|
    let ac_rt_a = 1
    ;;

    let ac_ll_b = ac_rt_a
    ;;

    let ac0_main = 1
    ;; |}]
;;

let%expect_test "" =
  let () = alpha_test {|
let rec (+) b c = c + b
;;
  |} in
  [%expect {|
    let rec op_plus b c =
      (op_plus c) b
    ;; |}]
;;

let%expect_test "" =
  let () = alpha_test {|
let (+) b c = c + b
let (+) b c = c + b
;;
  |} in
  [%expect
    {|
    let op_plus b c =
      (( + ) c) b
    ;;

    let ac2_op_plus ac0_b ac1_c =
      (op_plus ac1_c) ac0_b
    ;; |}]
;;

let%expect_test "" =
  let () =
    alpha_test
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
      match n with
        | 0 ->
          true
        | x ->
          odd ((( - ) x) 1)
    and odd ac0_n =
      match ac0_n with
        | 0 ->
          false
        | ac1_x ->
          even ((( - ) ac1_x) 1)
    ;; |}]
;;

let%expect_test "" =
  let () = alpha_test {|
let a = 10;;
let a, b = 1, 2;;
  |} in
  [%expect {|
    let a = 10
    ;;

    let (ac0_a, b) = (1, 2)
    ;; |}]
;;

let%expect_test "" =
  let () =
    alpha_test
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
    match xs with
      | [] ->
        0
      | (h :: tl) ->
        (( + ) 1) (length tl)
  ;;

  let length_tail = let rec helper ac_acc ac0_xs =
      match ac0_xs with
        | [] ->
          ac_acc
        | (ac1_h :: ac2_tl) ->
          (helper ((( + ) ac_acc) 1)) ac2_tl in
      helper 0
  ;;

  let rec map f ac3_xs =
    match ac3_xs with
      | [] ->
        []
      | (a :: []) ->
        (f a :: [])
      | (ac4_a :: (b :: [])) ->
        (f ac4_a :: (f b :: []))
      | (ac5_a :: (ac6_b :: (c :: []))) ->
        (f ac5_a :: (f ac6_b :: (f c :: [])))
      | (ac7_a :: (ac8_b :: (ac9_c :: (d :: ac10_tl)))) ->
        (f ac7_a :: (f ac8_b :: (f ac9_c :: (f d :: (map f) ac10_tl))))
  ;;

  let rec append ac11_xs ys =
    match ac11_xs with
      | [] ->
        ys
      | (x :: ac12_xs) ->
        (x :: (append ac12_xs) ys)
  ;;

  let concat = let rec ac13_helper ac14_xs =
      match ac14_xs with
        | [] ->
          []
        | (ac15_h :: ac16_tl) ->
          (append ac15_h) (ac13_helper ac16_tl) in
      ac13_helper
  ;;

  let rec iter ac17_f ac18_xs =
    match ac18_xs with
      | [] ->
        ()
      | (ac19_h :: ac20_tl) ->
        let () = ac17_f ac19_h in
          (iter ac17_f) ac20_tl
  ;;

  let rec cartesian ac21_xs ac22_ys =
    match ac21_xs with
      | [] ->
        []
      | (ac23_h :: ac24_tl) ->
        (append ((map (fun ac25_a -> (ac23_h, ac25_a))) ac22_ys)) ((cartesian ac24_tl) ac22_ys)
  ;;

  let ac26_main = let () = (iter print_int) (1 :: (2 :: (3 :: []))) in
      let () = print_int (length ((cartesian (1 :: (2 :: []))) (1 :: (2 :: (3 :: (4 :: [])))))) in
        0
  ;; |}]
;;

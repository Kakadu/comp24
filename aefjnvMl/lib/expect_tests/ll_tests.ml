(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open AefjnvMl_lib

open Top_utils.Ast_test_utils
open Middleend

let ll_test s =
  (* use [let*!] and [let+!] to exclude infer from pipeline *)
  let ast'_t =
    let* ast = Parser.parse s in
    let* ast' = Alpha_converter.rename_ast_with_uniq Common.Naming.alpha_prefix ast in
    let ast' = Middleend.Closure_conversion.convert_program ast' in
    let* ast' = Alpha_converter.rename_ast_with_uniq Common.Naming.cc_prefix ast' in
    let ast' =
      let open Match_elimination in
      let*! m_ast = Match_elim.eliminate_match_in_program ast' in
      let m_ast' = Optimizations.optimize m_ast in
      let open Ll_conversion in
      let+! ll_ast = Ll.lift_lambdas m_ast' in
      Ll_converter.convert_ll_program ll_ast
    in
    ast'
  in
  (* use [Common.Ast.pp_program] to show ast *)
  let ast_printer ast_ = Format.printf "%a\n" Common.Ast_pp.pp_program ast_ in
  print_result ast_printer ast'_t
;;

let%expect_test "" =
  let () =
    ll_test
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
    ;; |}]
;;

let%expect_test "" =
  let () =
    ll_test
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
    ;; |}]
;;

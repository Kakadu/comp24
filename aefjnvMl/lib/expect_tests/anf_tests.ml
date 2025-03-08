(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open AefjnvMl_lib

open Top_utils.Ast_test_utils

let anf_test s =
  (* use [let*!] and [let+!] to exclude infer from pipeline *)
  let ast'_t =
    let open Middleend in
    let* ast = Parser.parse s in
    let* ast' = Alpha_converter.rename_ast_with_uniq Common.Naming.alpha_prefix ast in
    let ast' = Closure_conversion.convert_program ast' in
    let* ast' = Alpha_converter.rename_ast_with_uniq Common.Naming.cc_prefix ast' in
    let ast' =
      let open Match_elimination in
      let*! m_ast = Match_elim.eliminate_match_in_program ast' in
      let m_ast' = Optimizations.optimize m_ast in
      let open Ll_conversion in
      let*! ll_ast = Ll.lift_lambdas m_ast' in
      let open Anf_conversion in
      let+! anf_ast = Anf.convert_to_anf ll_ast in
      Anf_converter.convert_anf_program anf_ast
    in
    ast'
  in
  (* use [Common.Ast.pp_program] to show ast *)
  let ast_printer ast_ = Format.printf "%a\n" Common.Ast_pp.pp_program ast_ in
  print_result ast_printer ast'_t
;;

let%expect_test "" =
  let () =
    anf_test
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
      let nf_0 = (( * ) p) cc0_n in
        cc1_k nf_0
    ;;

    let rec fac_cps n k =
      let nf_1 = (( = ) n) 1 in
        (if nf_1
        then
          k 1
        else
          let nf_2 = (( - ) n) 1 in
            let nf_3 = (ll_0 n) k in
              (fac_cps nf_2) nf_3)
    ;;

    let ll_1 cc_ac0_print_int =
      cc_ac0_print_int
    ;;

    let cc_ac1_main = let nf_4 = (fac_cps 4) ll_1 in
        let () = print_int nf_4 in
          0
    ;; |}]
;;

let%expect_test "" =
  let () =
    anf_test
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
    The type variable 'a occurs inside int -> ('a -> 'a -> int -> bool) -> bool |}]
;;

let%expect_test "" =
  let () =
    anf_test
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
      let nf_0 = (( = ) []) xs in
        (if nf_0
        then
          0
        else
          let nf_2 = get_list_len xs in
            let nf_1 = (( >= ) nf_2) 2 in
              (if nf_1
              then
                let h = (get_by_idx xs) 0 in
                  let tl = (get_by_idx xs) 1 in
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
                let cc_ac1_h = (get_by_idx cc_ac0_xs) 0 in
                  let cc_ac2_tl = (get_by_idx cc_ac0_xs) 1 in
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
          let nf_11 = get_list_len cc_ac3_xs in
            let nf_10 = (( = ) 2) nf_11 in
              let nf_9 = (if nf_10
                then
                  let nf_12 = (get_by_idx cc_ac3_xs) 1 in
                    (( = ) []) nf_12
                else
                  false) in
                (if nf_9
                then
                  let a = (get_by_idx cc_ac3_xs) 0 in
                    let nf_13 = f a in
                      (nf_13 :: [])
                else
                  let nf_16 = get_list_len cc_ac3_xs in
                    let nf_15 = (( = ) 3) nf_16 in
                      let nf_14 = (if nf_15
                        then
                          let nf_17 = (get_by_idx cc_ac3_xs) 2 in
                            (( = ) []) nf_17
                        else
                          false) in
                        (if nf_14
                        then
                          let cc_ac4_a = (get_by_idx cc_ac3_xs) 0 in
                            let b = (get_by_idx cc_ac3_xs) 1 in
                              let nf_18 = f b in
                                let nf_19 = f cc_ac4_a in
                                  (nf_19 :: (nf_18 :: []))
                        else
                          let nf_22 = get_list_len cc_ac3_xs in
                            let nf_21 = (( = ) 4) nf_22 in
                              let nf_20 = (if nf_21
                                then
                                  let nf_23 = (get_by_idx cc_ac3_xs) 3 in
                                    (( = ) []) nf_23
                                else
                                  false) in
                                (if nf_20
                                then
                                  let cc_ac5_a = (get_by_idx cc_ac3_xs) 0 in
                                    let cc_ac6_b = (get_by_idx cc_ac3_xs) 1 in
                                      let c = (get_by_idx cc_ac3_xs) 2 in
                                        let nf_24 = f c in
                                          let nf_25 = f cc_ac6_b in
                                            let nf_26 = f cc_ac5_a in
                                              (nf_26 :: (nf_25 :: (nf_24 :: [])))
                                else
                                  let nf_28 = get_list_len cc_ac3_xs in
                                    let nf_27 = (( >= ) nf_28) 5 in
                                      (if nf_27
                                      then
                                        let cc_ac7_a = (get_by_idx cc_ac3_xs) 0 in
                                          let cc_ac8_b = (get_by_idx cc_ac3_xs) 1 in
                                            let cc_ac9_c = (get_by_idx cc_ac3_xs) 2 in
                                              let d = (get_by_idx cc_ac3_xs) 3 in
                                                let cc_ac10_tl = (get_by_idx cc_ac3_xs) 4 in
                                                  let nf_29 = (map f) cc_ac10_tl in
                                                    let nf_30 = f d in
                                                      let nf_31 = f cc_ac9_c in
                                                        let nf_32 = f cc_ac8_b in
                                                          let nf_33 = f cc_ac7_a in
                                                            (nf_33 :: (nf_32 :: (nf_31 :: (nf_30 :: nf_29))))
                                      else
                                        part_match_fail ())))))
    ;;

    let rec append cc_ac11_xs ys =
      let nf_34 = (( = ) []) cc_ac11_xs in
        (if nf_34
        then
          ys
        else
          let nf_36 = get_list_len cc_ac11_xs in
            let nf_35 = (( >= ) nf_36) 2 in
              (if nf_35
              then
                let x = (get_by_idx cc_ac11_xs) 0 in
                  let cc_ac12_xs = (get_by_idx cc_ac11_xs) 1 in
                    let nf_37 = (append cc_ac12_xs) ys in
                      (x :: nf_37)
              else
                part_match_fail ()))
    ;;

    let rec cc_ac13_helper cc_ac14_xs =
      let nf_38 = (( = ) []) cc_ac14_xs in
        (if nf_38
        then
          []
        else
          let nf_40 = get_list_len cc_ac14_xs in
            let nf_39 = (( >= ) nf_40) 2 in
              (if nf_39
              then
                let cc_ac15_h = (get_by_idx cc_ac14_xs) 0 in
                  let cc_ac16_tl = (get_by_idx cc_ac14_xs) 1 in
                    let nf_41 = cc_ac13_helper cc_ac16_tl in
                      (append cc_ac15_h) nf_41
              else
                part_match_fail ()))
    ;;

    let concat = cc_ac13_helper
    ;;

    let rec iter cc_ac17_f cc_ac18_xs =
      let nf_42 = (( = ) []) cc_ac18_xs in
        (if nf_42
        then
          ()
        else
          let nf_44 = get_list_len cc_ac18_xs in
            let nf_43 = (( >= ) nf_44) 2 in
              (if nf_43
              then
                let cc_ac19_h = (get_by_idx cc_ac18_xs) 0 in
                  let cc_ac20_tl = (get_by_idx cc_ac18_xs) 1 in
                    let () = cc_ac17_f cc_ac19_h in
                      (iter cc_ac17_f) cc_ac20_tl
              else
                part_match_fail ()))
    ;;

    let ll_0 cc0_cc_ac23_h cc_ac25_a =
      (cc0_cc_ac23_h, cc_ac25_a)
    ;;

    let rec cartesian cc_ac21_xs cc_ac22_ys =
      let nf_45 = (( = ) []) cc_ac21_xs in
        (if nf_45
        then
          []
        else
          let nf_47 = get_list_len cc_ac21_xs in
            let nf_46 = (( >= ) nf_47) 2 in
              (if nf_46
              then
                let cc_ac23_h = (get_by_idx cc_ac21_xs) 0 in
                  let cc_ac24_tl = (get_by_idx cc_ac21_xs) 1 in
                    let nf_49 = ll_0 cc_ac23_h in
                      let nf_48 = (map nf_49) cc_ac22_ys in
                        let nf_50 = (cartesian cc_ac24_tl) cc_ac22_ys in
                          (append nf_48) nf_50
              else
                part_match_fail ()))
    ;;

    let cc_ac26_main = let nf_51 = (1 :: (2 :: (3 :: []))) in
        let () = (iter print_int) nf_51 in
          let nf_54 = (1 :: (2 :: [])) in
            let nf_55 = (1 :: (2 :: (3 :: (4 :: [])))) in
              let nf_53 = (cartesian nf_54) nf_55 in
                let nf_52 = length nf_53 in
                  let () = print_int nf_52 in
                    0
    ;; |}]
;;

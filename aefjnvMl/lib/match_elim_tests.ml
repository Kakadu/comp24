(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Top_utils.Ast_test_utils
open Middleend

let me_test s =
  let ast'_t =
    let*! ast' =
      let* ast = Parser.parse s in
      let* ast' = Alpha_converter.rename_ast_with_uniq Common.Naming.alpha_prefix ast in
      Ok ast'
    in
    let*! ast'' = Match_elim.eliminate_match_in_program ast' in
    Result.ok @@ Optimizations.optimize ast''
  in
  let ast_printer ast_ = Format.printf "%a\n" Me_ast_pp.pp_me_program ast_ in
  print_result ast_printer ast'_t
;;

let%expect_test "" =
  let () = me_test {|
let a = 1;;
let b = 1;;
let c = a + b;;
  |} in
  [%expect {|
    let a = 1;;
    let b = 1;;
    let c = (( + ) a) b;; |}]
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
     let me_1 = n in
    let me_2 = me_1 in
    (if (( = ) 0) me_2
     then 0
     else let x = me_1 in
    (( - ) x) 1);; |}]
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
         let me_1 = l in
        let me_2 = me_1 in
        (if (( = ) []) me_2
         then 0
         else let me_4 = me_1 in
        (if (if (( >= ) get_list_len_plus_one me_4) 2
         then let me_3 = (get_by_idx me_4) 0 in
        let tl = (get_by_idx me_4) 1 in
        true
         else false)
         then let me_3 = (get_by_idx me_4) 0 in
        let tl = (get_by_idx me_4) 1 in
        (( + ) 1) (len tl)
         else fail_pt_match ()));; |}]
;;

let%expect_test "" =
  let () = me_test {|
if true then 1 else 2;;
  |} in
  [%expect {|
    let me_1 = 1;; |}]
;;

let%expect_test "" =
  let () = me_test {|
if true && false then 1 else 2;;
  |} in
  [%expect {|
    let me_1 = 2;; |}]
;;

let%expect_test "" =
  let () = me_test {|
fun x -> if true then 1 else 2;;
  |} in
  [%expect {|
    let me_1 x =
     1;; |}]
;;

let%expect_test "" =
  let () = me_test {|
let (c, b) = (1, 2)
;;
  |} in
  [%expect
    {|
    let me_1 = (if let c = (get_by_idx me_1) 0 in
    let b = (get_by_idx me_1) 1 in
    true
     then (1, 2)
     else fail_pt_match ())
    and c = (get_by_idx me_1) 0
    and b = (get_by_idx me_1) 1;; |}]
;;

let%expect_test "" =
  let () =
    me_test
      {|
let rec len l =
   let me_1 = l in
   let me_2 = me_1 in
   (if (( = ) []) me_2
   then 0
   else let me_4 = me_1 in
   (if (if (( >= ) get_list_len_plus_one me_4) 2
   then let me_3 = (get_by_idx me_4) 0 in
   let tl = (get_by_idx me_4) 1 in
   true
   else false)
   then let me_3 = (get_by_idx me_4) 0 in
   let tl = (get_by_idx me_4) 1 in
   (( + ) 1) (len tl)
   else fail_pt_match ()))
;; 
  |}
  in
  [%expect {| =doesn't find binded value -- get_list_len_plus_one |}]
;;

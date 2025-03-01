(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open AefjnvMl_lib
open Top_utils.Ast_test_utils
open Middleend
open Match_elimination

let me_test s =
  (** use [let*!] and [let+!] to exclude infer from pipeline *)
  let ast'_t =
    let* ast = Parser.parse s in
    let*! ast' = Alpha_converter.rename_ast_with_uniq Common.Naming.alpha_prefix ast in
    let+! m_ast = Match_elim.eliminate_match_in_program ast' in 
    let m_ast' = Optimizations.optimize m_ast in
    let ast' = Me_converter.convert_program m_ast' in 
    ast'
  in
  (** use [Common.Ast.pp_program] to show ast *)
  let ast_printer ast_ = Format.printf "%a\n" Common.Ast_pp.pp_program ast_ in
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
      let me_2 = n in
        (if (( = ) 0) me_2
        then
          0
        else
          let x = me_2 in
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
          let me_2 = l in
            (if (( = ) []) me_2
            then
              0
            else
              (if (( >= ) get_list_len_plus_one me_2) 2
              then
                let tl = (get_by_idx me_2) 1 in
                  (( + ) 1) (len tl)
              else
                fail_pt_match ()));; |}]
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
    This definition is recursive but is missing the 'rec' keyword |}]
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

let%expect_test "" =
  let () = me_test {|
let a = 2
;; 
  |} in
  [%expect {|
    let a = 2;; |}]
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
    let me_1 = let ab = ((1 :: []) :: ((2 :: []) :: [])) in
        let me_3 = ab in
          (if (if (( >= ) get_list_len_plus_one me_3) 2
            then
              (( >= ) get_list_len_plus_one ((get_by_idx me_3) 0)) 2
            else
              false)
          then
            let a = (get_by_idx ((get_by_idx me_3) 0)) 0 in
              let c = (get_by_idx ((get_by_idx me_3) 0)) 1 in
                let b = (get_by_idx me_3) 1 in
                  1
          else
            fail_pt_match ());; |}]
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
    let me_1 = let ab = (1, 2) in
        let me_3 = ab in
          let a = (get_by_idx me_3) 0 in
            let b = (get_by_idx me_3) 1 in
              1;; |}]
;;

let%expect_test "" =
  let () = me_test {|
let f (a, b) = a + b
;; 
  |} in
  [%expect
    {|
    let f me_2 =
      let a = (get_by_idx me_2) 0 in
        let b = (get_by_idx me_2) 1 in
          (( + ) a) b;; |}]
;;

let%expect_test "" =
  let () = me_test {|
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
  |} in
  [%expect
    {|
    This definition is recursive but is missing the 'rec' keyword |}]
;;

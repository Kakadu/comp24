(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let parse_simplify_and_print_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure = To_simple_ast.convert parse_result in
    printf "%a" Simple_ast_pretty_printer.print_structure structure
  | Error _ -> printf "Syntax error"
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = 4;; |};
  [%expect {|
    let a = 4;; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun [] -> [];; |};
  [%expect {|
    let a #gen_pat_expr#0 = if ([]  =  #gen_pat_expr#0) then [] else (#gen_matching_failed# ());; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a x = fun () (a, b) -> (x, a, b);; |};
  [%expect {|
    let a x = (fun #gen_pat_expr#0 #gen_pat_expr#1 -> if (()  =  #gen_pat_expr#0) then let a = ((#gen_tuple_getter# 0) #gen_pat_expr#1) in let b = ((#gen_tuple_getter# 1) #gen_pat_expr#1) in (x, a, b) else (#gen_matching_failed# ()));; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun "a" -> 5;; |};
  [%expect {|
    let a #gen_pat_expr#0 = if ("a"  =  #gen_pat_expr#0) then 5 else (#gen_matching_failed# ());; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let (a, b) = (4, 5);; |};
  [%expect
    {|
    let #gen_pat_expr#0 = (4, 5);;
    let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) and b = ((#gen_tuple_getter# 1) #gen_pat_expr#0);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| 
  let a = 4;; 
  let (a, b) = (4, a);; |};
  [%expect
    {|
    let a = 4;;
    let #gen_pat_expr#0 = (4, a);;
    let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) and b = ((#gen_tuple_getter# 1) #gen_pat_expr#0);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| 
  let a = 4;; 
  let (a, b) = (5, 6) and c = a;; |};
  [%expect
    {|
    let a = 4;;
    let #gen_pat_expr#0 = (5, 6);;
    let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) and b = ((#gen_tuple_getter# 1) #gen_pat_expr#0) and c = a;; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| 
  let a = 5;;
  let a = 6 and (c, b) = (a, a);;
|};
  [%expect
    {|
    let a = 5;;
    let #gen_pat_expr#1 = (a, a);;
    let a = 6 and c = ((#gen_tuple_getter# 0) #gen_pat_expr#1) and b = ((#gen_tuple_getter# 1) #gen_pat_expr#1);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let (a, b) = (fun x -> (4, x)) 4;;  |};
  [%expect
    {|
    let #gen_pat_expr#0 = ((fun x -> (4, x)) 4);;
    let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) and b = ((#gen_tuple_getter# 1) #gen_pat_expr#0);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result
    {| let c = let (a, b) = (fun x -> (4, x)) 4 in (a, b);;  |};
  [%expect
    {|
    let c = let #gen_pat_expr#0 = ((fun x -> (4, x)) 4) in let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let b = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in (a, b);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result
    {| let c = let (a, (b, c)) = (fun x -> (4, (x, x))) 4 in (a, b, c);;  |};
  [%expect
    {|
    let c = let #gen_pat_expr#0 = ((fun x -> (4, (x, x))) 4) in let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let b = ((#gen_tuple_getter# 0) ((#gen_tuple_getter# 1) #gen_pat_expr#0)) in let c = ((#gen_tuple_getter# 1) ((#gen_tuple_getter# 1) #gen_pat_expr#0)) in (a, b, c);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| fun x (y, z) (x1, x2) -> x |};
  [%expect
    {|
    (fun x #gen_pat_expr#0 #gen_pat_expr#1 -> let y = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let z = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in let x1 = ((#gen_tuple_getter# 0) #gen_pat_expr#1) in let x2 = ((#gen_tuple_getter# 1) #gen_pat_expr#1) in x) |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = match b with | (d, c) -> c |};
  [%expect
    {|
    let a = let #gen_pat_expr#0 = b in if true then let d = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let c = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in c else (#gen_matching_failed# ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = match b with | (d, c) -> c | (a, b) -> a |};
  [%expect
    {|
    let a = let #gen_pat_expr#0 = b in if true then let d = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in let c = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in c else (#gen_matching_failed# ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = match b with | (5, c) -> c |};
  [%expect
    {|
    let a = let #gen_pat_expr#0 = b in if (5  =  ((#gen_tuple_getter# 0) #gen_pat_expr#0)) then let c = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in c else (#gen_matching_failed# ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result
    {| let a b = match b with | (5, (b, c), 4) -> (b, c);; |};
  [%expect
    {|
    let a b = let #gen_pat_expr#0 = b in if ((5  =  ((#gen_tuple_getter# 0) #gen_pat_expr#0))  &&  (4  =  ((#gen_tuple_getter# 2) #gen_pat_expr#0))) then let b = ((#gen_tuple_getter# 0) ((#gen_tuple_getter# 1) #gen_pat_expr#0)) in let c = ((#gen_tuple_getter# 1) ((#gen_tuple_getter# 1) #gen_pat_expr#0)) in (b, c) else (#gen_matching_failed# ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result
    {| let a = match b with | (5, a) -> (a, a) | (a, 5) -> (a, a) |};
  [%expect
    {|
    let a = let #gen_pat_expr#0 = b in if (5  =  ((#gen_tuple_getter# 0) #gen_pat_expr#0)) then let a = ((#gen_tuple_getter# 1) #gen_pat_expr#0) in (a, a) else if (5  =  ((#gen_tuple_getter# 1) #gen_pat_expr#0)) then let a = ((#gen_tuple_getter# 0) #gen_pat_expr#0) in (a, a) else (#gen_matching_failed# ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun b -> match b with | h :: tl -> (h, tl) |};
  [%expect
    {|
    let a b = let #gen_pat_expr#0 = b in if ((#gen_list_getter_length# #gen_pat_expr#0)  >=  1) then let h = (#gen_list_getter_head# #gen_pat_expr#0) in let tl = (#gen_list_getter_tail# #gen_pat_expr#0) in (h, tl) else (#gen_matching_failed# ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun b -> match b with | [] -> () |};
  [%expect
    {|
    let a b = let #gen_pat_expr#0 = b in if ([]  =  #gen_pat_expr#0) then () else (#gen_matching_failed# ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun b -> match b with | 1 :: 2 :: [] -> 1 |};
  [%expect
    {|
    let a b = let #gen_pat_expr#0 = b in if (((((#gen_list_getter_length# #gen_pat_expr#0)  =  2)  &&  (1  =  (#gen_list_getter_head# #gen_pat_expr#0)))  &&  (2  =  (#gen_list_getter_head# (#gen_list_getter_tail# #gen_pat_expr#0))))  &&  ([]  =  (#gen_list_getter_tail# (#gen_list_getter_tail# #gen_pat_expr#0)))) then 1 else (#gen_matching_failed# ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun x -> let b = x in b;; |};
  [%expect {|
    let a x = let b = x in b;; 
    |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let () = print_int 5;; |};
  [%expect {|
    let #gen_pat_expr#0 = (print_int 5);;
    let ;;
    |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let [] = [];; |};
  [%expect {|
    let #gen_pat_expr#0 = [];;
    let ;;
    |}]
;;

let%expect_test "" =
parse_simplify_and_print_result {| let b = let 4 :: a = 3 :: [] in a;; |};
  [%expect {| let b = let #gen_pat_expr#0 = 3 :: [] in let a = if (((#gen_list_getter_length# #gen_pat_expr#0)  >=  1)  &&  (4  =  (#gen_list_getter_head# #gen_pat_expr#0))) then (#gen_list_getter_tail# #gen_pat_expr#0) else (#gen_matching_failed# ()) in a;; |}]
;;

let%expect_test "" =
parse_simplify_and_print_result {| let b = let a :: [] = 4 :: [] in a;; |};
  [%expect {| let b = let #gen_pat_expr#0 = 4 :: [] in let a = if (((#gen_list_getter_length# #gen_pat_expr#0)  =  1)  &&  ([]  =  (#gen_list_getter_tail# #gen_pat_expr#0))) then (#gen_list_getter_head# #gen_pat_expr#0) else (#gen_matching_failed# ()) in a;; |}]
;;


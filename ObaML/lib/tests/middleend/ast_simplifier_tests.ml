(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let parse_simplify_and_print_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure, _ = Alpha_conversion.run_alpha_conversion parse_result in
    let structure = To_simple_ast.convert structure in
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
  [%expect
    {|
    let a pat0 =
    	if (pat0  =  [])
    	then []
    	else (matching_failed ());; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a x = fun () (a, b) -> (x, a, b);; |};
  [%expect
    {|
    let a x = (fun () pat0 ->
    	let oba0 = ((tuple_getter 0) pat0) in
    	let b = ((tuple_getter 1) pat0) in (x, oba0, b));; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun "a" -> 5;; |};
  [%expect
    {|
    let a pat0 =
    	if (pat0  =  "a")
    	then 5
    	else (matching_failed ());; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let (a, b) = (4, 5);; |};
  [%expect
    {|
    let pat0 = (4, 5);;

    let a = ((tuple_getter 0) pat0)
    and b = ((tuple_getter 1) pat0);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| 
  let a = 4;; 
  let (a, b) = (4, a);; |};
  [%expect
    {|
    let a = 4;;

    let pat0 = (4, a);;

    let oba0 = ((tuple_getter 0) pat0)
    and b = ((tuple_getter 1) pat0);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| 
  let a = 4;; 
  let (a, b) = (5, 6) and c = a;; |};
  [%expect
    {|
    let a = 4;;

    let pat0 = (5, 6);;

    let oba0 = ((tuple_getter 0) pat0)
    and b = ((tuple_getter 1) pat0)
    and c = a;; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| 
  let a = 5;;
  let a = 6 and (c, b) = (a, a);;
|};
  [%expect
    {|
    let a = 5;;

    let pat1 = (a, a);;

    let oba0 = 6
    and c = ((tuple_getter 0) pat1)
    and b = ((tuple_getter 1) pat1);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let (a, b) = (fun x -> (4, x)) 4;;  |};
  [%expect
    {|
    let pat0 = ((fun x -> (4, x)) 4);;

    let a = ((tuple_getter 0) pat0)
    and b = ((tuple_getter 1) pat0);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result
    {| let c = let (a, b) = (fun x -> (4, x)) 4 in (a, b);;  |};
  [%expect
    {|
    let c =
    	let pat0 = ((fun x -> (4, x)) 4) in
    	let a = ((tuple_getter 0) pat0) in
    	let b = ((tuple_getter 1) pat0) in (a, b);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result
    {| let c = let (a, (b, c)) = (fun x -> (4, (x, x))) 4 in (a, b, c);;  |};
  [%expect
    {|
    let c =
    	let pat0 = ((fun x -> (4, (x, x))) 4) in
    	let a = ((tuple_getter 0) pat0) in
    	let b = ((tuple_getter 0) ((tuple_getter 1) pat0)) in
    	let oba0 = ((tuple_getter 1) ((tuple_getter 1) pat0)) in (a, b, oba0);; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| fun x (y, z) (x1, x2) -> x |};
  [%expect
    {|
    (fun x pat0 pat1 ->
    	let y = ((tuple_getter 0) pat0) in
    	let z = ((tuple_getter 1) pat0) in
    	let x1 = ((tuple_getter 0) pat1) in
    	let x2 = ((tuple_getter 1) pat1) in x) |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = match b with | (d, c) -> c |};
  [%expect
    {|
    let a =
    	let pat0 = b in
    	if true
    	then
    	let d = ((tuple_getter 0) pat0) in
    	let c = ((tuple_getter 1) pat0) in c
    	else (matching_failed ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = match b with | (d, c) -> c | (a, b) -> a |};
  [%expect
    {|
    let a =
    	let pat0 = b in
    	if true
    	then
    	let d = ((tuple_getter 0) pat0) in
    	let c = ((tuple_getter 1) pat0) in c
    	else (matching_failed ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = match b with | (5, c) -> c |};
  [%expect
    {|
    let a =
    	let pat0 = b in
    	if (((tuple_getter 0) pat0)  =  5)
    	then
    	let c = ((tuple_getter 1) pat0) in c
    	else (matching_failed ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result
    {| let a b = match b with | (5, (b, c), 4) -> (b, c);; |};
  [%expect
    {|
    let a b =
    	let pat0 = b in
    	if ((((tuple_getter 0) pat0)  =  5)  &&  (((tuple_getter 2) pat0)  =  4))
    	then
    	let oba0 = ((tuple_getter 0) ((tuple_getter 1) pat0)) in
    	let c = ((tuple_getter 1) ((tuple_getter 1) pat0)) in (oba0, c)
    	else (matching_failed ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result
    {| let a = match b with | (5, a) -> (a, a) | (a, 5) -> (a, a) |};
  [%expect
    {|
    let a =
    	let pat0 = b in
    	if (((tuple_getter 0) pat0)  =  5)
    	then
    	let oba0 = ((tuple_getter 1) pat0) in (oba0, oba0)
    	else
    	if (((tuple_getter 1) pat0)  =  5)
    	then
    	let oba1 = ((tuple_getter 0) pat0) in (oba1, oba1)
    	else (matching_failed ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun b -> match b with | h :: tl -> (h, tl) |};
  [%expect
    {|
    let a b =
    	let pat0 = b in
    	if ((list_length_getter pat0)  >=  1)
    	then
    	let h = (list_head_getter pat0) in
    	let tl = (list_tail_getter pat0) in (h, tl)
    	else (matching_failed ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun b -> match b with | [] -> () |};
  [%expect
    {|
    let a b =
    	let pat0 = b in
    	if (pat0  =  [])
    	then ()
    	else (matching_failed ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun b -> match b with | 1 :: 2 :: [] -> 1 |};
  [%expect
    {|
    let a b =
    	let pat0 = b in
    	if ((((list_length_getter pat0)  =  2)  &&  ((list_head_getter pat0)  =  1))  &&  ((list_head_getter (list_tail_getter pat0))  =  2))
    	then 1
    	else (matching_failed ());;
     |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let a = fun x -> let b = x in b;; |};
  [%expect {|
    let a x =
    	let b = x in b;;
    |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let () = print_int 5;; |};
  [%expect {|
    let () = (print_int 5);;
    |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result
    {| let a = let 1 :: [] = 2 :: [] in let () = print_int 5 in 0;; |};
  [%expect
    {|
    let a =
    	let pat0 = (2 :: []) in
    	let () =
    	if (((list_length_getter pat0)  =  1)  &&  ((list_head_getter pat0)  =  1))
    	then ()
    	else (matching_failed ()) in
    	let () = (print_int 5) in 0;;
    |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let 1 :: [] = 2 :: [];; |};
  [%expect
    {|
    let pat0 = (2 :: []);;

    let () = (((list_length_getter pat0)  =  1)  &&  ((list_head_getter pat0)  =  1));;
    |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let b = let 4 :: a = 3 :: [] in a;; |};
  [%expect
    {|
      let b =
      	let pat0 = (3 :: []) in
      	let () =
      	if (((list_length_getter pat0)  >=  1)  &&  ((list_head_getter pat0)  =  4))
      	then ()
      	else (matching_failed ()) in
      	let a = (list_tail_getter pat0) in a;; |}]
;;

let%expect_test "" =
  parse_simplify_and_print_result {| let b = let a :: [] = 4 :: [] in a;; |};
  [%expect
    {|
      let b =
      	let pat0 = (4 :: []) in
      	let () =
      	if ((list_length_getter pat0)  =  1)
      	then ()
      	else (matching_failed ()) in
      	let a = (list_head_getter pat0) in a;; |}]
;;

(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let parse_alpha_convert_and_print_result str alpha_conversion_setting =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure = To_simple_ast.convert parse_result in
    let structure, _ =
      Alpha_conversion.run_alpha_conversion structure alpha_conversion_setting
    in
    printf "%a" Simple_ast_pretty_printer.print_structure structure
  | Error _ -> printf "Syntax error"
;;

let parse_inner_alpha_and_print str = parse_alpha_convert_and_print_result str Inner

let%expect_test "" =
  parse_inner_alpha_and_print {| let a = 4;; 
let b = 4;;|};
  [%expect {|
    let a = 4;;

    let b = 4;; |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {| 
  let a = 4;; 
  let (a, b) = (5, 6) and c = a;; |};
  [%expect
    {|
    let a = 4;;

    let #pat#0 = (5, 6);;

    let a = ((#tuple_getter# 0) #pat#0)
    and b = ((#tuple_getter# 1) #pat#0)
    and c = a;; |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {|let map p = let (a,b) = p in a + b |};
  [%expect
    {|       
  let map p =
  	let oba0 = p in
  	let a = ((#tuple_getter# 0) oba0) in
  	let b = ((#tuple_getter# 1) oba0) in (a  +  b);; |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print
    {| let f x y = 
      let x z = y + z in 
      let y z = x 1 + z in
      x 1 + y 2;; |};
  [%expect
    {|
      let f x y =
      	let oba0 z = (y  +  z) in
      	let oba1 oba2 = ((oba0 1)  +  oba2) in ((oba0 1)  +  (oba1 2));; |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {|fun x () x -> x () |};
  [%expect {|       
  (fun x () oba0 -> (oba0 ())) |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {| let rec a a = a;; |};
  [%expect {|       
  let rec a oba0 = oba0;; |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {| let rec oba0 x x = x;; |};
  [%expect {|       
  let rec oba0 x oba1 = oba1;; |}]
;;

let%expect_test "006partial2" =
  parse_inner_alpha_and_print
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
|};
  [%expect
    {|
    let foo a b c =
    	let () = (print_int a) in
    	let () = (print_int b) in
    	let () = (print_int c) in (a  +  (b  *  c));;

    let main =
    	let oba0 = (foo 1) in
    	let oba1 = (oba0 2) in
    	let oba2 = (oba1 3) in
    	let () = (print_int oba2) in 0;; |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {|
    let a = 5

    let a = let a = a in a;;
  |};
  [%expect {|
      let a = 5;;

      let a =
      	let oba0 = a in oba0;; |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print
    {|
    let main = 5

    let main =
        let foo = (fun x -> x) in
        let foo = foo 2 in
        let () = print_int foo in
        0
  |};
  [%expect
    {|
      let main = 5;;

      let main =
      	let foo x = x in
      	let oba0 = (foo 2) in
      	let () = (print_int oba0) in 0;; |}]
;;

let parse_all_alpha_and_print str = parse_alpha_convert_and_print_result str All

let%expect_test "loss one of `main` functions with ALL setting (the error will not be \
                 thrown)"
  =
  parse_all_alpha_and_print
    {|
    let main = 5

    let main =
        let foo = (fun x -> x) in
        let foo = foo 2 in
        let () = print_int foo in
        0
  |};
  [%expect
    {|
      let main = 5;;

      let oba0 =
      	let foo x = x in
      	let oba1 = (foo 2) in
      	let () = (print_int oba1) in 0;; |}]
;;

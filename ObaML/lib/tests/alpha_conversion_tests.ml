(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let parse_alpha_convert_and_print_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure = Ast_simplifier.simplify_ast parse_result in
    let structure = Alpha_conversion.run_alpha_conversion structure in
    printf "%a" Simple_ast_pretty_printer.print_structure structure
  | Error _ -> printf "Syntax error"
;;

let%expect_test "" =
  parse_alpha_convert_and_print_result {| let a = 4;; 
let b = 4;;|};
  [%expect {|
    let obaml0 = 4;;
    let obaml1 = 4;; |}]
;;

let%expect_test "" =
  parse_alpha_convert_and_print_result
    {| 
  let a = 4;; 
  let (a, b) = (5, 6) and c = a;; |};
  [%expect
    {|
    let obaml0 = 4;;
    let obaml1 = (5, 6);;
    let obaml2 = ((#gen_tuple_getter# 0) obaml1) and obaml3 = ((#gen_tuple_getter# 1) obaml1) and obaml4 = obaml0;; |}]
;;

let%expect_test "" =
  parse_alpha_convert_and_print_result {|let map p = let (a,b) = p in a + b |};
  [%expect
    {|       
  let obaml0 obaml1 = let obaml2 = obaml1 in let obaml3 = ((#gen_tuple_getter# 0) obaml2) in let obaml4 = ((#gen_tuple_getter# 1) obaml2) in (obaml3  +  obaml4);; |}]
;;

let%expect_test "" =
  parse_alpha_convert_and_print_result
    {| let f a b = 
      let a x = b + x in 
      let b x = a 1 + x in
      a 1 + b 2;; |};
  [%expect
    {|
      let obaml0 obaml1 obaml2 = let obaml3 obaml4 = (obaml2  +  obaml4) in let obaml5 obaml6 = ((obaml3 1)  +  obaml6) in ((obaml3 1)  +  (obaml5 2));; |}]
;;

let%expect_test "" =
  parse_alpha_convert_and_print_result {|fun x () x -> x () |};
  [%expect {|       
  (fun obaml0 obaml1 obaml2 -> (obaml2 ())) |}]
;;

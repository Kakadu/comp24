(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let parse_alpha_convert_and_print_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure = To_simple_ast.convert parse_result in
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
    {| let f x y = 
      let x z = y + z in 
      let y z = x 1 + z in
      x 1 + y 2;; |};
  [%expect
    {|
      let obaml0 obaml1 obaml2 = let obaml3 obaml4 = (obaml2  +  obaml4) in let obaml5 obaml6 = ((obaml3 1)  +  obaml6) in ((obaml3 1)  +  (obaml5 2));; |}]
;;

let%expect_test "" =
  parse_alpha_convert_and_print_result {|fun x () x -> x () |};
  [%expect {|       
  (fun obaml0 obaml1 obaml2 -> (obaml2 ())) |}]
;;

let%expect_test "015tuples" =
  parse_alpha_convert_and_print_result
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
|};
  [%expect {|
    let obaml0 obaml1 obaml2 = ((obaml1 (obaml0 obaml1)) obaml2);;
    let obaml3 obaml4 obaml5 = let obaml6 = obaml5 in let obaml7 = ((#gen_tuple_getter# 0) obaml6) in let obaml8 = ((#gen_tuple_getter# 1) obaml6) in ((obaml4 obaml7), (obaml4 obaml8));;
    let obaml9 obaml10 = ((obaml0 (fun obaml11 obaml12 -> ((obaml3 (fun obaml13 obaml14 -> ((obaml13 (obaml11 obaml12)) obaml14))) obaml12))) obaml10);;
    let obaml15 obaml16 obaml17 = let obaml18 = obaml16 in let obaml19 = ((#gen_tuple_getter# 0) obaml18) in let obaml20 = ((#gen_tuple_getter# 1) obaml18) in if (obaml17  ==  0) then 1 else (obaml20 (obaml17  -  1));;
    let obaml21 obaml22 obaml23 = let obaml24 = obaml22 in let obaml25 = ((#gen_tuple_getter# 0) obaml24) in let obaml26 = ((#gen_tuple_getter# 1) obaml24) in if (obaml23  ==  0) then 0 else (obaml25 (obaml23  -  1));;
    let obaml27 = (obaml9 (obaml15, obaml21));;
    let obaml28 obaml31 = if (obaml31  =  0) then 1 else (obaml29 (obaml31  -  1)) and obaml29 obaml30 = if (obaml30  =  0) then 1 else (obaml28 (obaml30  -  1));;
    let obaml32 = let obaml33 = (print_int (obaml29 1)) in let obaml34 = (print_int (obaml28 2)) in let obaml35 = obaml27 in let obaml36 = ((#gen_tuple_getter# 0) obaml35) in let obaml37 = ((#gen_tuple_getter# 1) obaml35) in let obaml38 = (print_int (obaml37 3)) in let obaml39 = (print_int (obaml36 4)) in 0;; |}]
;;

(** Copyright 2023-2024, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib
open Ast
open Parser

(* TESTS  PARSER*)

let start_test parser show input =
  let res = start_parsing parser input in
  match res with
  | Ok res -> Format.printf "%s" (show res)
  | Error err -> Format.printf "%s" err
;;

(* Test pattern parser *)

let%expect_test _ =
  let test = "true" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PConst (CBool true)) |}]
;;

let%expect_test _ =
  let test = "\"itsastring\"" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PConst (CString "itsastring")) |}]
;;

let%expect_test _ =
  let test = "951753" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PConst (CInt 951753)) |}]
;;

let%expect_test _ =
  let test = "var" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PVar ("var", TUnknown)) |}]
;;

let%expect_test _ =
  let test = "(var : string)" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PVar ("var", TString)) |}]
;;

let%expect_test _ =
  let test = "let" in
  start_test parse_pattern show_pattern test;
  [%expect {| : You can not use "let" keywords as vars |}]
;;

(* Test expression *)

(* EConst *)

let%expect_test _ =
  let test = "1" in
  start_test parse_econst show_expression test;
  [%expect {| (EConst (CInt 1)) |}]
;;

let%expect_test _ =
  let test = "false" in
  start_test parse_econst show_expression test;
  [%expect {| (EConst (CBool false)) |}]
;;

let%expect_test _ =
  let test = "false" in
  start_test parse_econst show_expression test;
  [%expect {| (EConst (CBool false)) |}]
;;

let%expect_test _ =
  let test = "\"popka\"" in
  start_test parse_econst show_expression test;
  [%expect {| (EConst (CString "popka")) |}]
;;

(* EVar *)

let%expect_test _ =
  let test = "papa" in
  start_test parse_evar show_expression test;
  [%expect {| (EVar ("papa", TUnknown)) |}]
;;

let%expect_test _ =
  let test = "(papa : int)" in
  start_test parse_evar show_expression test;
  [%expect {| (EVar ("papa", TInt)) |}]
;;

(* EBinaryOp *)

let%expect_test _ =
  let test = "1 + 2" in
  start_test parse_ebinop show_expression test;
  [%expect {| (EBinaryOp (Add, (EConst (CInt 1)), (EConst (CInt 2)))) |}]
;;

let%expect_test _ =
  let test = "3 + 2 * 4 - 1" in
  start_test parse_ebinop show_expression test;
  [%expect
    {|
    (EBinaryOp (Sub,
       (EBinaryOp (Add, (EConst (CInt 3)),
          (EBinaryOp (Mul, (EConst (CInt 2)), (EConst (CInt 4)))))),
       (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  let test = "1 + a / 3" in
  start_test parse_ebinop show_expression test;
  [%expect
    {|
    (EBinaryOp (Add, (EConst (CInt 1)),
       (EBinaryOp (Div, (EVar ("a", TUnknown)), (EConst (CInt 3)))))) |}]
;;

let%expect_test _ =
  let test = "1 + a" in
  start_test parse_ebinop show_expression test;
  [%expect {|
    (EBinaryOp (Add, (EConst (CInt 1)), (EVar ("a", TUnknown)))) |}]
;;

let%expect_test _ =
  let test = "1 + a" in
  start_test parse_ebinop show_expression test;
  [%expect {|
    (EBinaryOp (Add, (EConst (CInt 1)), (EVar ("a", TUnknown)))) |}]
;;

let%expect_test _ =
  let test = "true || false" in
  start_test parse_ebinop show_expression test;
  [%expect {| (EBinaryOp (Or, (EConst (CBool true)), (EConst (CBool false)))) |}]
;;

(* EIfElse *)

let%expect_test _ =
  let test = "if n = 1 then 1 else 3" in
  start_test parse_eifelse show_expression test;
  [%expect
    {|
    (EIfElse ((EBinaryOp (Eq, (EVar ("n", TUnknown)), (EConst (CInt 1)))),
       (EConst (CInt 1)), (EConst (CInt 3)))) |}]
;;

(* EFun *)

let%expect_test _ =
  let test = "fun x -> 3 + x" in
  start_test parse_efun show_expression test;
  [%expect
    {|
    (EFun ((PVar ("x", TUnknown)),
       (EBinaryOp (Add, (EConst (CInt 3)), (EVar ("x", TUnknown)))))) |}]
;;

let%expect_test _ =
  let test = "(fun x -> 3 + x)" in
  start_test parse_efun show_expression test;
  [%expect
    {|
    (EFun ((PVar ("x", TUnknown)),
       (EBinaryOp (Add, (EConst (CInt 3)), (EVar ("x", TUnknown)))))) |}]
;;

let%expect_test _ =
  let test = "fun x y -> y + x" in
  start_test parse_efun show_expression test;
  [%expect
    {|
    (EFun ((PVar ("x", TUnknown)),
       (EFun ((PVar ("y", TUnknown)),
          (EBinaryOp (Add, (EVar ("y", TUnknown)), (EVar ("x", TUnknown))))))
       )) |}]
;;

(* EApp *)

let%expect_test _ =
  let test = "in_popka yes no" in
  start_test parse_eapp show_expression test;
  [%expect
    {|
    (EApp ((EApp ((EVar ("in_popka", TUnknown)), (EVar ("yes", TUnknown)))),
       (EVar ("no", TUnknown)))) |}]
;;

(* ELetIn *)

let%expect_test _ =
  let test = "let sum a b = a + b in five 2 3" in
  start_test parse_eletin show_expression test;
  [%expect
    {|
    (ELetIn (Notrec, "sum",
       (EFun ((PVar ("a", TUnknown)),
          (EFun ((PVar ("b", TUnknown)),
             (EBinaryOp (Add, (EVar ("a", TUnknown)), (EVar ("b", TUnknown))))))
          )),
       (EApp ((EApp ((EVar ("five", TUnknown)), (EConst (CInt 2)))),
          (EConst (CInt 3))))
       )) |}]
;;

(* Run time test*)

let%expect_test _ =
  let test = "five a" in
  start_test parse_eapp show_expression test;
  [%expect {|
    (EApp ((EVar ("five", TUnknown)), (EVar ("a", TUnknown)))) |}]
;;

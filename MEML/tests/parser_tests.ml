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
  start_test parse_expression show_expression test;
  [%expect {| (EConst (CInt 1)) |}]
;;

let%expect_test _ =
  let test = "false" in
  start_test parse_expression show_expression test;
  [%expect {| (EConst (CBool false)) |}]
;;

let%expect_test _ =
  let test = "\"popka\"" in
  start_test parse_expression show_expression test;
  [%expect {| (EConst (CString "popka")) |}]
;;

(* EVar *)

let%expect_test _ =
  let test = "papa" in
  start_test parse_expression show_expression test;
  [%expect {| (EVar ("papa", TUnknown)) |}]
;;

let%expect_test _ =
  let test = "(papa : int)" in
  start_test parse_expression show_expression test;
  [%expect {| (EVar ("papa", TInt)) |}]
;;

(* EBinaryOp *)

let%expect_test _ =
  let test = "3 + 2 * 4 - 1" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EBinaryOp (Sub,
       (EBinaryOp (Add, (EConst (CInt 3)),
          (EBinaryOp (Mul, (EConst (CInt 2)), (EConst (CInt 4)))))),
       (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  let test = "1 + (a * 3) - x" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EBinaryOp (Sub,
       (EBinaryOp (Add, (EConst (CInt 1)),
          (EBinaryOp (Mul, (EVar ("a", TUnknown)), (EConst (CInt 3)))))),
       (EVar ("x", TUnknown)))) |}]
;;

let%expect_test _ =
  let test = "true || false" in
  start_test parse_expression show_expression test;
  [%expect {| (EBinaryOp (Or, (EConst (CBool true)), (EConst (CBool false)))) |}]
;;

(* EIfElse *)

let%expect_test _ =
  let test = "if n = 1 then 1 else 3" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EIfElse ((EBinaryOp (Eq, (EVar ("n", TUnknown)), (EConst (CInt 1)))),
       (EConst (CInt 1)), (EConst (CInt 3)))) |}]
;;

(* EFun *)

let%expect_test _ =
  let test = "fun x -> 3 + x" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EFun ((PVar ("x", TUnknown)),
       (EBinaryOp (Add, (EConst (CInt 3)), (EVar ("x", TUnknown)))))) |}]
;;

let%expect_test _ =
  let test = "(fun x -> 3 + x)" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EFun ((PVar ("x", TUnknown)),
       (EBinaryOp (Add, (EConst (CInt 3)), (EVar ("x", TUnknown)))))) |}]
;;

let%expect_test _ =
  let test = "fun x y -> y + x" in
  start_test parse_expression show_expression test;
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
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EApp ((EApp ((EVar ("in_popka", TUnknown)), (EVar ("yes", TUnknown)))),
       (EVar ("no", TUnknown)))) |}]
;;

(* ELetIn *)

let%expect_test _ =
  let test = "let sum a b = a + b in sum 2 3" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (ELetIn (Notrec, "sum",
       (EFun ((PVar ("a", TUnknown)),
          (EFun ((PVar ("b", TUnknown)),
             (EBinaryOp (Add, (EVar ("a", TUnknown)), (EVar ("b", TUnknown))))))
          )),
       (EApp ((EApp ((EVar ("sum", TUnknown)), (EConst (CInt 2)))),
          (EConst (CInt 3))))
       )) |}]
;;

(** Test bindings *)

let%expect_test _ =
  let test = "let plusfive x = let five a = a + 5 in five x" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec, "plusfive",
       (EFun ((PVar ("x", TUnknown)),
          (ELetIn (Notrec, "five",
             (EFun ((PVar ("a", TUnknown)),
                (EBinaryOp (Add, (EVar ("a", TUnknown)), (EConst (CInt 5)))))),
             (EApp ((EVar ("five", TUnknown)), (EVar ("x", TUnknown))))))
          ))
       )) |}]
;;

let%expect_test _ =
  let test = "4 + 3" in
  start_test parse_bindings show_bindings test;
  [%expect {|
    (Expression (EBinaryOp (Add, (EConst (CInt 4)), (EConst (CInt 3))))) |}]
;;

let%expect_test _ =
  let test = "let (+) x y = x - y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec, "Add",
       (EFun ((PVar ("x", TUnknown)),
          (EFun ((PVar ("y", TUnknown)),
             (EBinaryOp (Sub, (EVar ("x", TUnknown)), (EVar ("y", TUnknown))))))
          ))
       ))|}]
;;

let%expect_test _ =
  let test = "let f (x: int) (y: int) = x + y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec, "f",
       (EFun ((PVar ("x", TInt)),
          (EFun ((PVar ("y", TInt)),
             (EBinaryOp (Add, (EVar ("x", TUnknown)), (EVar ("y", TUnknown))))))
          ))
       ))
 |}]
;;

let%expect_test _ =
  let test = "let f (x: int) = x + 4" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec, "f",
       (EFun ((PVar ("x", TInt)),
          (EBinaryOp (Add, (EVar ("x", TUnknown)), (EConst (CInt 4))))))
       ))
 |}]
;;

let%expect_test _ =
  let test = "2 * (4 + 4) * 1" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Expression
       (EBinaryOp (Mul,
          (EBinaryOp (Mul, (EConst (CInt 2)),
             (EBinaryOp (Add, (EConst (CInt 4)), (EConst (CInt 4)))))),
          (EConst (CInt 1)))))
 |}]
;;

let%expect_test _ =
  let test =
    "let rec fib n = \n\
    \      if n < 1 \n\
    \        then 1 \n\
    \      else fib (n - 1) + fib (n - 2)"
  in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Rec, "fib",
       (EFun ((PVar ("n", TUnknown)),
          (EIfElse (
             (EBinaryOp (Less, (EVar ("n", TUnknown)), (EConst (CInt 1)))),
             (EConst (CInt 1)),
             (EBinaryOp (Add,
                (EApp ((EVar ("fib", TUnknown)),
                   (EBinaryOp (Sub, (EVar ("n", TUnknown)), (EConst (CInt 1)))))),
                (EApp ((EVar ("fib", TUnknown)),
                   (EBinaryOp (Sub, (EVar ("n", TUnknown)), (EConst (CInt 2))))))
                ))
             ))
          ))
       ))
 |}]
;;

let%expect_test _ =
  let test = "let rec fac n = if n <= 1 then 1 else n * fac n" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Rec, "fac",
       (EFun ((PVar ("n", TUnknown)),
          (EIfElse ((EBinaryOp (Leq, (EVar ("n", TUnknown)), (EConst (CInt 1)))),
             (EConst (CInt 1)),
             (EBinaryOp (Mul, (EVar ("n", TUnknown)),
                (EApp ((EVar ("fac", TUnknown)), (EVar ("n", TUnknown))))))
             ))
          ))
       ))
 |}]
;;

let%expect_test _ =
  let test =
    "let rec cps_fact x = \n\
    \      let helper x acc = \n\
    \        if x = 1 \n\
    \          then acc x \n\
    \        else helper (x - 1) (fun n -> n * acc x) \n\
    \      in \n\
    \      helper x (fun a -> a)"
  in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Rec, "cps_fact",
       (EFun ((PVar ("x", TUnknown)),
          (ELetIn (Notrec, "helper",
             (EFun ((PVar ("x", TUnknown)),
                (EFun ((PVar ("acc", TUnknown)),
                   (EIfElse (
                      (EBinaryOp (Eq, (EVar ("x", TUnknown)), (EConst (CInt 1)))),
                      (EApp ((EVar ("acc", TUnknown)), (EVar ("x", TUnknown)))),
                      (EApp (
                         (EApp ((EVar ("helper", TUnknown)),
                            (EBinaryOp (Sub, (EVar ("x", TUnknown)),
                               (EConst (CInt 1))))
                            )),
                         (EFun ((PVar ("n", TUnknown)),
                            (EBinaryOp (Mul, (EVar ("n", TUnknown)),
                               (EApp ((EVar ("acc", TUnknown)),
                                  (EVar ("x", TUnknown))))
                               ))
                            ))
                         ))
                      ))
                   ))
                )),
             (EApp ((EApp ((EVar ("helper", TUnknown)), (EVar ("x", TUnknown)))),
                (EFun ((PVar ("a", TUnknown)), (EVar ("a", TUnknown))))))
             ))
          ))
       ))
 |}]
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

let%expect_test _ =
  parse_program_with_print {| (1 : int) |};
  [%expect
    {|
    [(SExpression (ETyped ((EConstant (CInt 1)), (TDGround GTDInt))))] |}]
;;

let%expect_test _ =
  parse_program_with_print {| ("s" : string) |};
  [%expect
    {|
    [(SExpression (ETyped ((EConstant (CString "s")), (TDGround GTDString))))] |}]
;;

let%expect_test _ =
  parse_program_with_print {| fun (x :: int) -> x |};
  [%expect
    {|
    Syntax error. |}]
;;

let%expect_test _ =
  parse_program_with_print {| fun (x : int) -> ((x + 1) : int) |};
  [%expect
    {|
    [(SExpression
        (EFun (((PTyped ((PVar (Id "x")), (TDGround GTDInt))), []),
           (ETyped (
              (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
                 [(EConstant (CInt 1))])),
              (TDGround GTDInt)))
           )))
      ] |}]
;;

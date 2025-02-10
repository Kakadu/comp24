(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

let%expect_test _ =
  parse_program_with_print {| (1 : int) |};
  [%expect {|
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
  [%expect {|
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

let%expect_test _ =
  parse_program_with_print {| fun ((x, y) : int * int) -> x + y |};
  [%expect
    {|
    [(SExpression
        (EFun (
           ((PTyped ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
               (TDTuple ((TDGround GTDInt), (TDGround GTDInt), [])))),
            []),
           (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
              [(EIdentifier (Id "y"))]))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_program_with_print {| fun (x : int) -> ((x, x) : int * int) |};
  [%expect
    {|
    [(SExpression
        (EFun (((PTyped ((PVar (Id "x")), (TDGround GTDInt))), []),
           (ETyped (
              (ETuple ((EIdentifier (Id "x")), (EIdentifier (Id "x")), [])),
              (TDTuple ((TDGround GTDInt), (TDGround GTDInt), []))))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_program_with_print {| let x = (1, 2) in (x : int * int) |};
  [%expect
    {|
    [(SExpression
        (ELetIn (
           ((PVar (Id "x")),
            (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), []))),
           [],
           (ETyped ((EIdentifier (Id "x")),
              (TDTuple ((TDGround GTDInt), (TDGround GTDInt), []))))
           )))
      ]
    |}]
;;

let%expect_test _ =
  parse_program_with_print {| let x = (fun (y : int) -> y + 1) in (x 5 : int) |};
  [%expect
    {|
    [(SExpression
        (ELetIn (
           ((PVar (Id "x")),
            (EFun (((PTyped ((PVar (Id "y")), (TDGround GTDInt))), []),
               (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "y")),
                  [(EConstant (CInt 1))]))
               ))),
           [],
           (ETyped (
              (EApplication ((EIdentifier (Id "x")), (EConstant (CInt 5)), [])),
              (TDGround GTDInt)))
           )))
      ]
    |}]
;;

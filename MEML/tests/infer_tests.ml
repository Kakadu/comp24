(** Copyright 2024-2025, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib
open Ast
open Inferencer

let%expect_test "1" =
  print_result (EConst (CInt 1));
  [%expect {| int |}]
;;

let%expect_test "false" =
  print_result (EConst (CBool false));
  [%expect {| bool |}]
;;

let%expect_test "fun x y -> y + x" =
  print_result
    (EFun
       ( PVar ("x", TUnknown)
       , EFun
           ( PVar ("y", TUnknown)
           , EBinaryOp (Add, EVar ("y", TUnknown), EVar ("x", TUnknown)) ) ));
  [%expect {| int -> int -> int |}]
;;

let%expect_test "fun (x: Int) -> x + x" =
  print_result
    (EFun (PVar ("x", TInt), EBinaryOp (Mul, EVar ("x", TUnknown), EVar ("x", TUnknown))));
  [%expect {| int -> int |}]
;;

let%expect_test "EList_ty_test" =
  print_result
    (EList
       ( EConst (CInt 1)
       , EList
           (EConst (CInt 2), EList (EConst (CInt 3), EList (EConst (CInt 4), EConst CNil)))
       ));
  [%expect {| int list |}]
;;

let%expect_test "ETuple_ty_test" =
  print_result
    (ETuple
       [ EConst (CInt 1)
       ; EConst (CInt 2)
       ; EConst (CInt 3)
       ; EConst (CInt 4)
       ; EConst (CInt 5)
       ]);
  [%expect {| (int * int * int * int * int) |}]
;;

let%expect_test "let plusfive x = let five a = a + 5 in five x" =
  print_prog_result
    [ Let
        ( Notrec
        , "plusfive"
        , EFun
            ( PVar ("x", TUnknown)
            , ELetIn
                ( Notrec
                , "five"
                , EFun
                    ( PVar ("a", TUnknown)
                    , EBinaryOp (Add, EVar ("a", TUnknown), EConst (CInt 5)) )
                , EApp (EVar ("five", TUnknown), EVar ("x", TUnknown)) ) ) )
    ];
  [%expect {| plusfive : int -> int |}]
;;

let%expect_test "let f x y = x + y" =
  print_prog_result
    [ Let
        ( Notrec
        , "f"
        , EFun
            ( PVar ("x", TUnknown)
            , EFun
                ( PVar ("y", TUnknown)
                , EBinaryOp (Add, EVar ("x", TUnknown), EVar ("y", TUnknown)) ) ) )
    ];
  [%expect {| f : int -> int -> int |}]
;;

let%expect_test "let idk (fs: int) (sn: int) = fs + sn * fs" =
  print_prog_result
    [ Let
        ( Notrec
        , "idk"
        , EFun
            ( PVar ("fs", TInt)
            , EFun
                ( PVar ("sn", TInt)
                , EBinaryOp
                    ( Add
                    , EVar ("fs", TUnknown)
                    , EBinaryOp (Mul, EVar ("sn", TUnknown), EVar ("fs", TUnknown)) ) ) )
        )
    ];
  [%expect {| idk : int -> int -> int |}]
;;

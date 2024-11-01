(** Copyright 2023-2024, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open MEML_lib
open Ast
open Inferencer

let%expect_test _ =
  print_result (EConst (CInt 1));
  [%expect {| int |}]
;;

let%expect_test _ =
  print_result (EConst (CBool false));
  [%expect {| bool |}]
;;

let%expect_test _ =
  print_result (EConst (CString "popka"));
  [%expect {| string |}]
;;

(* fun x y -> y + x *)
let%expect_test _ =
  print_result
    (EFun
       ( PVar ("x", TUnknown)
       , EFun
           ( PVar ("y", TUnknown)
           , EBinaryOp (Add, EVar ("y", TUnknown), EVar ("x", TUnknown)) ) ));
  [%expect {| int -> int -> int |}]
;;

(* fun (x: Int) -> x + x *)
let%expect_test _ =
  print_result
    (EFun (PVar ("x", TInt), EBinaryOp (Mul, EVar ("x", TUnknown), EVar ("x", TUnknown))));
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  print_result
    (EFun
       ( PVar ("x", TUnknown)
       , EFun
           ( PVar ("y", TUnknown)
           , EBinaryOp (Add, EVar ("y", TUnknown), EVar ("x", TUnknown)) ) ));
  [%expect {| int -> int -> int |}]
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

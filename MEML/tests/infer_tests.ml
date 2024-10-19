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

let%expect_test _ =
  print_result
    (EList
       ( EConst (CInt 1)
       , EList
           (EConst (CInt 2), EList (EConst (CInt 3), EList (EConst (CInt 4), EConst CNil)))
       ));
  [%expect {| int list |}]
;;

let%expect_test _ =
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

let%expect_test "let f (x: int) = x + 4" =
  print_prog_result
    [ Let
        ( Notrec
        , "f"
        , EFun (PVar ("x", TInt), EBinaryOp (Add, EVar ("x", TUnknown), EConst (CInt 4)))
        )
    ];
  [%expect {| f : int -> int |}]
;;

let%expect_test "let f (x: int) = x + 4" =
  print_prog_result
    [ Let
        ( Notrec
        , "f"
        , EFun (PVar ("x", TInt), EBinaryOp (Add, EVar ("x", TUnknown), EConst (CInt 4)))
        )
    ];
  [%expect {| f : int -> int |}]
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

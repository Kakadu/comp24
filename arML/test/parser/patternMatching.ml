(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* open ArML_lib.Runner *)

(* See declarations with pattern matching tests in declaration.ml *)
(* See "let in" with pattern matching tests in function.ml *)
(* See "function" expr with pattern matching tests in function.ml *)

(* Match with *)

open ArML_lib.Runner

let%expect_test _ =
  parse_expression
    {| 
    match 2 with
    | 0 -> "zero"
    | 1 -> "one"
    | 2 -> "two"
    | _ -> "other"
  |};
  [%expect
    {|
    (EMatchWith ((EConstant (CInt 2)),
       ((PConst (CInt 0)), (EConstant (CString "zero"))),
       [((PConst (CInt 1)), (EConstant (CString "one")));
         ((PConst (CInt 2)), (EConstant (CString "two")));
         (PAny, (EConstant (CString "other")))]
       )) |}]
;;

let%expect_test _ =
  parse_expression {| 
    match (3, 4) with (x, y) -> x + y
  |};
  [%expect
    {|
    (EMatchWith ((ETuple ((EConstant (CInt 3)), (EConstant (CInt 4)), [])),
       ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
        (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
           [(EIdentifier (Id "y"))]))),
       [])) |}]
;;

let%expect_test _ =
  parse_expression
    {| 
    match ((1, 2), 3) with
    | ((x, y), z) -> x + y + z
  |};
  [%expect
    {|
    (EMatchWith (
       (ETuple ((ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [])),
          (EConstant (CInt 3)), [])),
       ((PTuple ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
           (PVar (Id "z")), [])),
        (EApplication ((EIdentifier (Id "( + )")),
           (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
              [(EIdentifier (Id "y"))])),
           [(EIdentifier (Id "z"))]))),
       [])) |}]
;;

let%expect_test _ =
  parse_expression
    {| 
    match (1 :: 2 :: 3 :: []) with
    | [] -> []
    | x :: xs -> [x]
  |};
  [%expect
    {|
    (EMatchWith (
       (EListConstructor ((EConstant (CInt 1)),
          (EListConstructor ((EConstant (CInt 2)),
             (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
          )),
       (PNill, EEmptyList),
       [((PListConstructor ((PVar (Id "x")), (PVar (Id "xs")))),
         (EListConstructor ((EIdentifier (Id "x")), EEmptyList)))]
       )) |}]
;;

let%expect_test _ =
  parse_expression
    {| 
    match (3, [1; 2; 3]) with
    | (x, []) -> x
    | (x, y) -> y
  |};
  [%expect
    {|
    (EMatchWith (
       (ETuple ((EConstant (CInt 3)),
          (EListConstructor ((EConstant (CInt 1)),
             (EListConstructor ((EConstant (CInt 2)),
                (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
             )),
          [])),
       ((PTuple ((PVar (Id "x")), PNill, [])), (EIdentifier (Id "x"))),
       [((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])), (EIdentifier (Id "y")))
         ]
       )) |}]
;;

let%expect_test _ =
  parse_expression
    {| 
    match (5, [1; 2; 3]) with
    | (x, []) -> "empty list"
    | (x, [y]) -> "one element in list"
    | (x, y :: ys) -> "head"
  |};
  [%expect
    {|
    (EMatchWith (
       (ETuple ((EConstant (CInt 5)),
          (EListConstructor ((EConstant (CInt 1)),
             (EListConstructor ((EConstant (CInt 2)),
                (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
             )),
          [])),
       ((PTuple ((PVar (Id "x")), PNill, [])), (EConstant (CString "empty list"))),
       [((PTuple ((PVar (Id "x")), (PListConstructor ((PVar (Id "y")), PNill)),
            [])),
         (EConstant (CString "one element in list")));
         ((PTuple ((PVar (Id "x")),
             (PListConstructor ((PVar (Id "y")), (PVar (Id "ys")))), [])),
          (EConstant (CString "head")))
         ]
       )) |}]
;;

(* ---------------- *)

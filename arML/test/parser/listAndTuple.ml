(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Lists *)

let%expect_test _ =
  parse_expression {| [] |};
  [%expect {|
    EEmptyList |}]
;;

let%expect_test _ =
  parse_expression {| [1; 2; 3] |};
  [%expect
    {|
    (EListConstructor ((EConstant (CInt 1)),
       (EListConstructor ((EConstant (CInt 2)),
          (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| [x; y; z] |};
  [%expect
    {|
    (EListConstructor ((EIdentifier (Id "x")),
       (EListConstructor ((EIdentifier (Id "y")),
          (EListConstructor ((EIdentifier (Id "z")), EEmptyList))))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| x :: y :: z :: [] |};
  [%expect
    {|
    (EListConstructor ((EIdentifier (Id "x")),
       (EListConstructor ((EIdentifier (Id "y")),
          (EListConstructor ((EIdentifier (Id "z")), EEmptyList))))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| (0, 0) :: (1, 2) :: [(3, 4); (5, 6)] |};
  [%expect
    {|
    (EListConstructor ((ETuple ((EConstant (CInt 0)), (EConstant (CInt 0)), [])),
       (EListConstructor (
          (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [])),
          (EListConstructor (
             (ETuple ((EConstant (CInt 3)), (EConstant (CInt 4)), [])),
             (EListConstructor (
                (ETuple ((EConstant (CInt 5)), (EConstant (CInt 6)), [])),
                EEmptyList))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| (fun x y -> x) :: (fun x _ -> x) :: [] |};
  [%expect
    {|
    (EListConstructor (
       (EFun (((PVar (Id "x")), [(PVar (Id "y"))]), (EIdentifier (Id "x")))),
       (EListConstructor (
          (EFun (((PVar (Id "x")), [PAny]), (EIdentifier (Id "x")))), EEmptyList
          ))
       )) |}]
;;

(* ---------------- *)

(* Tuples *)

let%expect_test _ =
  parse_expression {| () |};
  (* An empty tuple is not parsed *)
  [%expect {|
    (EConstant CUnit) |}]
;;

let%expect_test _ =
  parse_expression {| (1, 2) |};
  [%expect {|
    (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [])) |}]
;;

let%expect_test _ =
  parse_expression {| (1, 2, 3) |};
  [%expect
    {|
    (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [(EConstant (CInt 3))])) |}]
;;

let%expect_test _ =
  parse_expression {| (1, 2, a) |};
  [%expect
    {|
    (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [(EIdentifier (Id "a"))]
       )) |}]
;;

let%expect_test _ =
  parse_expression {| (fun x -> x, fun y -> y) |};
  [%expect
    {|
    (ETuple ((EFun (((PVar (Id "x")), []), (EIdentifier (Id "x")))),
       (EFun (((PVar (Id "y")), []), (EIdentifier (Id "y")))), [])) |}]
;;

let%expect_test _ =
  parse_expression {| (1, fun x -> x, (), [1 ; 3; 5]) |};
  [%expect
    {|
    (ETuple ((EConstant (CInt 1)),
       (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x")))),
       [(EConstant CUnit);
         (EListConstructor ((EConstant (CInt 1)),
            (EListConstructor ((EConstant (CInt 3)),
               (EListConstructor ((EConstant (CInt 5)), EEmptyList))))
            ))
         ]
       )) |}]
;;

let%expect_test _ =
  parse_expression {| ((1, 2), 1) |};
  [%expect
    {|
    (ETuple ((ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [])),
       (EConstant (CInt 1)), [])) |}]
;;

let%expect_test _ =
  parse_expression {| ((fun x -> x) 0, (fun y -> y) 0) |};
  [%expect
    {|
    (ETuple (
       (EApplication ((EFun (((PVar (Id "x")), []), (EIdentifier (Id "x")))),
          (EConstant (CInt 0)), [])),
       (EApplication ((EFun (((PVar (Id "y")), []), (EIdentifier (Id "y")))),
          (EConstant (CInt 0)), [])),
       [])) |}]
;;

let%expect_test _ =
  parse_expression {| (let f x = x in f 0, let f y = y in 0) |};
  [%expect
    {|
    (ETuple (
       (ELetIn (
          ((PVar (Id "f")),
           (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
          [], (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 0)), [])))),
       (ELetIn (
          ((PVar (Id "f")),
           (EFun (((PVar (Id "y")), []), (EIdentifier (Id "y"))))),
          [], (EConstant (CInt 0)))),
       [])) |}]
;;

(* ---------------- *)

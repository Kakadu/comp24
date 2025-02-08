(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* List *)

let%expect_test _ =
  parse_program_with_print {| [] |};
  [%expect {|
    [(SExpression EEmptyList)] |}]
;;

let%expect_test _ =
  parse_program_with_print {| [1; 2; 3] |};
  [%expect
    {|
    [(SExpression
        (EListConstructor ((EConstant (CInt 1)),
           (EListConstructor ((EConstant (CInt 2)),
              (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_program_with_print {| [1; 2; a] |};
  [%expect {|
    [(SExpression
        (EListConstructor ((EConstant (CInt 1)),
           (EListConstructor ((EConstant (CInt 2)),
              (EListConstructor ((EIdentifier (Id "a")), EEmptyList))))
           )))
      ] |}]
;;

(* Tuple *)

let%expect_test _ =
  parse_program_with_print {| () |};
  (* An empty tuple is not parsed *)
  [%expect {|
    [(SExpression (EConstant CUnit))] |}]
;;


let%expect_test _ =
  parse_program_with_print {| (1, 2, 3) |};
  [%expect
    {|
    [(SExpression
        (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)),
           [(EConstant (CInt 3))])))
      ] |}]
;;

let%expect_test _ =
  parse_program_with_print {| (1, 2, a) |};
  [%expect
    {|
    [(SExpression
        (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)),
           [(EIdentifier (Id "a"))])))
      ] |}]
;;
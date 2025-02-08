(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

let%expect_test _ =
  parse_program_with_print {| fun x -> x |};
  [%expect
    {|
    [(SExpression (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x")))))] |}]
;;

let%expect_test _ =
  parse_program_with_print {| fun x y z -> x + y + z |};
  [%expect
    {|
    [(SExpression
        (EFun (((PVar (Id "x")), [(PVar (Id "y")); (PVar (Id "z"))]),
           (EApplication ((EIdentifier (Id "( + )")),
              [(EApplication ((EIdentifier (Id "( + )")),
                  [(EIdentifier (Id "x")); (EIdentifier (Id "y"))]));
                (EIdentifier (Id "z"))]
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_program_with_print {| fun (x : int) -> x + 1 |};
  [%expect{|
    [(SExpression
        (EFun (((PTyped ((PVar (Id "x")), (TDGround GTDInt))), []),
           (EApplication ((EIdentifier (Id "( + )")),
              [(EIdentifier (Id "x")); (EConstant (CInt 1))]))
           )))
      ] |} ]
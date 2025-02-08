(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

let%expect_test _ =
  parse_program_with_print {| let x = [] |};
  [%expect {|
    [(SDeclaration (DOrdinary (((PVar (Id "x")), EEmptyList), [])))] |}]
;;

let%expect_test _ =
  parse_program_with_print {| let x = 1 |};
  [%expect {|
    [(SDeclaration (DOrdinary (((PVar (Id "x")), (EConstant (CInt 1))), [])))] |}]
;;

let%expect_test _ =
  parse_program_with_print {| let x = x + 1 |};
  [%expect {|
    [(SDeclaration
        (DOrdinary (
           ((PVar (Id "x")),
            (EApplication ((EIdentifier (Id "( + )")),
               [(EIdentifier (Id "x")); (EConstant (CInt 1))]))),
           [])))
      ] |}]
;;

let%expect_test _ =
  parse_program_with_print {| let f x y = x + y |};
  [%expect {|
    [(SDeclaration
        (DOrdinary (
           ((PVar (Id "f")),
            (EFun (((PVar (Id "x")), [(PVar (Id "y"))]),
               (EApplication ((EIdentifier (Id "( + )")),
                  [(EIdentifier (Id "x")); (EIdentifier (Id "y"))]))
               ))),
           [])))
      ] |}]
;;

let%expect_test _ =
  parse_program_with_print {| let rec f x = f (x - 1)|};
  [%expect
    {|
    [(SDeclaration
        (DRecursive (
           ((PVar (Id "f")),
            (EFun (((PVar (Id "x")), []),
               (EApplication ((EIdentifier (Id "f")),
                  [(EApplication ((EIdentifier (Id "( - )")),
                      [(EIdentifier (Id "x")); (EConstant (CInt 1))]))
                    ]
                  ))
               ))),
           [])))
      ] |}]
;;

let%expect_test _ =
  parse_program_with_print {| let f x y = x + y let main = f 1 2|};
  [%expect{|
    [(SDeclaration
        (DOrdinary (
           ((PVar (Id "f")),
            (EFun (((PVar (Id "x")), [(PVar (Id "y"))]),
               (EApplication ((EIdentifier (Id "( + )")),
                  [(EIdentifier (Id "x")); (EIdentifier (Id "y"))]))
               ))),
           [])));
      (SDeclaration
         (DOrdinary (
            ((PVar (Id "main")),
             (EApplication ((EIdentifier (Id "f")),
                [(EConstant (CInt 1)); (EConstant (CInt 2))]))),
            [])))
      ] |} ]
;;

let%expect_test _ =
  parse_program_with_print {| let f = let x = 1 in 2 + x * 3|};
  [%expect{|
    [(SDeclaration
        (DOrdinary (
           ((PVar (Id "f")),
            (ELetIn (((PVar (Id "x")), (EConstant (CInt 1))), [],
               (EApplication ((EIdentifier (Id "( + )")),
                  [(EConstant (CInt 2));
                    (EApplication ((EIdentifier (Id "( * )")),
                       [(EIdentifier (Id "x")); (EConstant (CInt 3))]))
                    ]
                  ))
               ))),
           [])))
      ] |} ]
;;

let%expect_test _ =
  parse_program_with_print {| 1 + let x = 1 in x |};
  [%expect{|
    [(SExpression
        (EApplication ((EIdentifier (Id "( + )")),
           [(EConstant (CInt 1));
             (ELetIn (((PVar (Id "x")), (EConstant (CInt 1))), [],
                (EIdentifier (Id "x"))))
             ]
           )))
      ] |} ]
;;

let%expect_test _ =
  parse_program_with_print {| let recx = 1 |};
  [%expect{| [(SDeclaration (DOrdinary (((PVar (Id "recx")), (EConstant (CInt 1))), [])))] |}]
;;

let%expect_test _ =
  parse_program_with_print {| letrec = 1 |};
  [%expect{|
    [(SExpression
        (EApplication ((EIdentifier (Id "( = )")),
           [(EIdentifier (Id "letrec")); (EConstant (CInt 1))])))
      ] |}]



(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Fml_lib.Parser
open Fml_lib.Ast

let parse_with_print input =
  match parse input with
  | Ok ast -> Format.printf "%a\n" pp_program ast
  | Error message -> Format.printf "Error: %s\n" message
;;

let%expect_test _ =
  parse_with_print {| let n = 5|};
  [%expect
    {| [(SingleDecl (DDeclaration (NoRec, (PIdentifier "n"), (EConst (CInt 5)))))] |}]
;;

let%expect_test _ =
  parse_with_print {| let flag = true|};
  [%expect
    {|
    [(SingleDecl
        (DDeclaration (NoRec, (PIdentifier "flag"), (EConst (CBool true)))))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| let flag = false|};
  [%expect
    {|
    [(SingleDecl
        (DDeclaration (NoRec, (PIdentifier "flag"), (EConst (CBool false)))))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| let (h::tl) = lst|};
  [%expect
    {|
    [(SingleDecl
        (DDeclaration (NoRec, (PCons ((PIdentifier "h"), (PIdentifier "tl"))),
           (EIdentifier "lst"))))
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {| let mymatch x =
        match x with
          | 1 -> true
          | _ -> false|};
  [%expect
    {|
    [(SingleDecl
        (DDeclaration (NoRec, (PIdentifier "mymatch"),
           (EFun ((PIdentifier "x"),
              (EMatch ((EIdentifier "x"),
                 [((PConst (CInt 1)), (EConst (CBool true)));
                   (PAny, (EConst (CBool false)))]
                 ))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {|
        let a = 4::[5; 6]|};
  [%expect
    {|
        [(SingleDecl
            (DDeclaration (NoRec, (PIdentifier "a"),
               (ECons ((EConst (CInt 4)),
                  (ECons ((EConst (CInt 5)), (ECons ((EConst (CInt 6)), ENill))))))
               )))
          ]
    |}]
;;

let%expect_test _ =
  parse_with_print {| let fix f = let rec g x = f (g x) in g|};
  [%expect
    {|
    [(SingleDecl
        (DDeclaration (NoRec, (PIdentifier "fix"),
           (EFun ((PIdentifier "f"),
              (ELetIn (Rec, (PIdentifier "g"),
                 (EFun ((PIdentifier "x"),
                    (EApplication ((EIdentifier "f"),
                       (EApplication ((EIdentifier "g"), (EIdentifier "x")))))
                    )),
                 (EIdentifier "g")))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {|
        let add_one (x: int) = 1 + x|};
  [%expect
    {|
        [(SingleDecl
            (DDeclaration (NoRec, (PIdentifier "add_one"),
               (EFun ((PConstraint ((PIdentifier "x"), AInt)),
                  (EApplication (
                     (EApplication ((EIdentifier "( + )"), (EConst (CInt 1)))),
                     (EIdentifier "x")))
                  ))
               )))
          ]
    |}]
;;

let%expect_test _ =
  parse_with_print {|
        let (+) a = 1 + a |};
  [%expect
    {|
        [(SingleDecl
            (DDeclaration (NoRec, (PIdentifier "( + )"),
               (EFun ((PIdentifier "a"),
                  (EApplication (
                     (EApplication ((EIdentifier "( + )"), (EConst (CInt 1)))),
                     (EIdentifier "a")))
                  ))
               )))
          ]
    |}]
;;

let%expect_test _ =
  parse_with_print {|
        let f a b = b + a |};
  [%expect
    {|
        [(SingleDecl
            (DDeclaration (NoRec, (PIdentifier "f"),
               (EFun ((PIdentifier "a"),
                  (EFun ((PIdentifier "b"),
                     (EApplication (
                        (EApplication ((EIdentifier "( + )"), (EIdentifier "b"))),
                        (EIdentifier "a")))
                     ))
                  ))
               )))
          ]
    |}]
;;

let%expect_test _ =
  parse_with_print
    {|let rec first x = match x with
  | 1 -> 1
  | x -> second (x + 10)
and second x = first (x + 1);;

|};
  [%expect
    {|
    [(MutableRecDecl
        [(DDeclaration (Rec, (PIdentifier "first"),
            (EFun ((PIdentifier "x"),
               (EMatch ((EIdentifier "x"),
                  [((PConst (CInt 1)), (EConst (CInt 1)));
                    ((PIdentifier "x"),
                     (EApplication ((EIdentifier "second"),
                        (EApplication (
                           (EApplication ((EIdentifier "( + )"),
                              (EIdentifier "x"))),
                           (EConst (CInt 10))))
                        )))
                    ]
                  ))
               ))
            ));
          (DDeclaration (NoRec, (PIdentifier "second"),
             (EFun ((PIdentifier "x"),
                (EApplication ((EIdentifier "first"),
                   (EApplication (
                      (EApplication ((EIdentifier "( + )"), (EIdentifier "x"))),
                      (EConst (CInt 1))))
                   ))
                ))
             ))
          ])
      ] |}]
;;

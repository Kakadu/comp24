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
  [%expect {| [(NoRecDecl [(DDeclaration ((PIdentifier "n"), (EConst (CInt 5))))])] |}]
;;

let%expect_test _ =
  parse_with_print
    {| let n = 5

  let f x = x|};
  [%expect
    {|
      [(NoRecDecl [(DDeclaration ((PIdentifier "n"), (EConst (CInt 5))))]);
        (NoRecDecl
           [(DDeclaration ((PIdentifier "f"),
               (EFun ((PIdentifier "x"), (EIdentifier "x")))))
             ])
        ] |}]
;;

let%expect_test _ =
  parse_with_print {| let flag = true|};
  [%expect
    {|
    [(NoRecDecl [(DDeclaration ((PIdentifier "flag"), (EConst (CBool true))))])] |}]
;;

let%expect_test _ =
  parse_with_print {| let flag = false|};
  [%expect
    {|
    [(NoRecDecl [(DDeclaration ((PIdentifier "flag"), (EConst (CBool false))))])] |}]
;;

let%expect_test _ =
  parse_with_print {| let (h::tl) = lst|};
  [%expect
    {|
    [(NoRecDecl
        [(DDeclaration ((PCons ((PIdentifier "h"), (PIdentifier "tl"))),
            (EIdentifier "lst")))
          ])
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
    [(NoRecDecl
        [(DDeclaration ((PIdentifier "mymatch"),
            (EFun ((PIdentifier "x"),
               (EMatch ((EIdentifier "x"),
                  [((PConst (CInt 1)), (EConst (CBool true)));
                    (PAny, (EConst (CBool false)))]
                  ))
               ))
            ))
          ])
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
        let a = 4::[5; 6]|};
  [%expect
    {|
        [(NoRecDecl
            [(DDeclaration ((PIdentifier "a"),
                (ECons ((EConst (CInt 4)),
                   (ECons ((EConst (CInt 5)), (ECons ((EConst (CInt 6)), ENill))))))
                ))
              ])
          ]
    |}]
;;

let%expect_test _ =
  parse_with_print {| let fix f = let rec g x = f (g x) in g|};
  [%expect
    {|
    [(NoRecDecl
        [(DDeclaration ((PIdentifier "fix"),
            (EFun ((PIdentifier "f"),
               (ELetIn (Rec, (PIdentifier "g"),
                  (EFun ((PIdentifier "x"),
                     (EApplication ((EIdentifier "f"),
                        (EApplication ((EIdentifier "g"), (EIdentifier "x")))))
                     )),
                  (EIdentifier "g")))
               ))
            ))
          ])
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
        let add_one (x: int) = 1 + x|};
  [%expect
    {|
        [(NoRecDecl
            [(DDeclaration ((PIdentifier "add_one"),
                (EFun ((PConstraint ((PIdentifier "x"), AInt)),
                   (EApplication (
                      (EApplication ((EIdentifier "( + )"), (EConst (CInt 1)))),
                      (EIdentifier "x")))
                   ))
                ))
              ])
          ]
    |}]
;;

let%expect_test _ =
  parse_with_print
    {|
        let (+) a = 1 + a |};
  [%expect
    {|
        [(NoRecDecl
            [(DDeclaration ((PIdentifier "( + )"),
                (EFun ((PIdentifier "a"),
                   (EApplication (
                      (EApplication ((EIdentifier "( + )"), (EConst (CInt 1)))),
                      (EIdentifier "a")))
                   ))
                ))
              ])
          ]
    |}]
;;

let%expect_test _ =
  parse_with_print
    {|
        let f a b = b + a |};
  [%expect
    {|
        [(NoRecDecl
            [(DDeclaration ((PIdentifier "f"),
                (EFun ((PIdentifier "a"),
                   (EFun ((PIdentifier "b"),
                      (EApplication (
                         (EApplication ((EIdentifier "( + )"), (EIdentifier "b"))),
                         (EIdentifier "a")))
                      ))
                   ))
                ))
              ])
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
    [(RecDecl
        [(DDeclaration ((PIdentifier "first"),
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
          (DDeclaration ((PIdentifier "second"),
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

let%expect_test _ =
  parse_with_print
    {|
        let h::x::tl = 5::6::4::[]|};
  [%expect
    {|
    [(NoRecDecl
        [(DDeclaration (
            (PCons ((PIdentifier "h"),
               (PCons ((PIdentifier "x"), (PIdentifier "tl"))))),
            (ECons ((EConst (CInt 5)),
               (ECons ((EConst (CInt 6)), (ECons ((EConst (CInt 4)), ENill))))))
            ))
          ])
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
        let rec factorial n = if n < 1 then 1 else n * factorial (n - 1) |};
  [%expect
    {|
        [(RecDecl
            [(DDeclaration ((PIdentifier "factorial"),
                (EFun ((PIdentifier "n"),
                   (EIf (
                      (EApplication (
                         (EApplication ((EIdentifier "( < )"), (EIdentifier "n"))),
                         (EConst (CInt 1)))),
                      (EConst (CInt 1)),
                      (EApplication (
                         (EApplication ((EIdentifier "( * )"), (EIdentifier "n"))),
                         (EApplication ((EIdentifier "factorial"),
                            (EApplication (
                               (EApplication ((EIdentifier "( - )"),
                                  (EIdentifier "n"))),
                               (EConst (CInt 1))))
                            ))
                         ))
                      ))
                   ))
                ))
              ])
          ]
    |}]
;;

let%expect_test _ =
  parse_with_print {|let () = print_int ( fac_cps 4 ( fun print_int -> print_int ) )|};
  [%expect
    {|
      [(NoRecDecl
          [(DDeclaration (PUnit,
              (EApplication ((EIdentifier "print_int"),
                 (EApplication (
                    (EApplication ((EIdentifier "fac_cps"), (EConst (CInt 4)))),
                    (EFun ((PIdentifier "print_int"), (EIdentifier "print_int")))))
                 ))
              ))
            ])
        ]
  |}]
;;

let%expect_test _ =
  parse_with_print
    {|let rec fac_cps n k =
   if n=1 then k 1 else
   fac_cps (n-1) (fun p -> k (p*n))|};
  [%expect
    {|
      [(RecDecl
          [(DDeclaration ((PIdentifier "fac_cps"),
              (EFun ((PIdentifier "n"),
                 (EFun ((PIdentifier "k"),
                    (EIf (
                       (EApplication (
                          (EApplication ((EIdentifier "( = )"), (EIdentifier "n"))),
                          (EConst (CInt 1)))),
                       (EApplication ((EIdentifier "k"), (EConst (CInt 1)))),
                       (EApplication (
                          (EApplication ((EIdentifier "fac_cps"),
                             (EApplication (
                                (EApplication ((EIdentifier "( - )"),
                                   (EIdentifier "n"))),
                                (EConst (CInt 1))))
                             )),
                          (EFun ((PIdentifier "p"),
                             (EApplication ((EIdentifier "k"),
                                (EApplication (
                                   (EApplication ((EIdentifier "( * )"),
                                      (EIdentifier "p"))),
                                   (EIdentifier "n")))
                                ))
                             ))
                          ))
                       ))
                    ))
                 ))
              ))
            ])
        ]
  |}]
;;

let%expect_test _ =
  parse_with_print
    {| let test3 a b c =
  let a = print_int a in
  let b = print_int b in
  let c = print_int c in
  0|};
  [%expect
    {|
    [(NoRecDecl
        [(DDeclaration ((PIdentifier "test3"),
            (EFun ((PIdentifier "a"),
               (EFun ((PIdentifier "b"),
                  (EFun ((PIdentifier "c"),
                     (ELetIn (NoRec, (PIdentifier "a"),
                        (EApplication ((EIdentifier "print_int"),
                           (EIdentifier "a"))),
                        (ELetIn (NoRec, (PIdentifier "b"),
                           (EApplication ((EIdentifier "print_int"),
                              (EIdentifier "b"))),
                           (ELetIn (NoRec, (PIdentifier "c"),
                              (EApplication ((EIdentifier "print_int"),
                                 (EIdentifier "c"))),
                              (EConst (CInt 0))))
                           ))
                        ))
                     ))
                  ))
               ))
            ))
          ])
      ] |}]
;;

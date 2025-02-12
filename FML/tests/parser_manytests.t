  $ ./parser_runner.exe < manytests/typed/001fac.ml
  [(SingleDecl
      (DDeclaration (Rec, (PIdentifier "fac"),
         (EFun ((PIdentifier "n"),
            (EIf (
               (EApplication (
                  (EApplication ((EIdentifier "( <= )"), (EIdentifier "n"))),
                  (EConst (CInt 1)))),
               (EConst (CInt 1)),
               (EApplication (
                  (EApplication ((EIdentifier "( * )"), (EIdentifier "n"))),
                  (EApplication ((EIdentifier "fac"),
                     (EApplication (
                        (EApplication ((EIdentifier "( - )"), (EIdentifier "n")
                           )),
                        (EConst (CInt 1))))
                     ))
                  ))
               ))
            ))
         )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "main"),
          (ELetIn (NoRec, PUnit,
             (EApplication ((EIdentifier "print_int"),
                (EApplication ((EIdentifier "fac"), (EConst (CInt 4)))))),
             (EConst (CInt 0))))
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/002fac.ml
  [(SingleDecl
      (DDeclaration (Rec, (PIdentifier "fac_cps"),
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
         )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "main"),
          (ELetIn (NoRec, PUnit,
             (EApplication ((EIdentifier "print_int"),
                (EApplication (
                   (EApplication ((EIdentifier "fac_cps"), (EConst (CInt 4)))),
                   (EFun ((PIdentifier "print_int"), (EIdentifier "print_int")
                      ))
                   ))
                )),
             (EConst (CInt 0))))
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/003fib.ml
  Error: : no more choices

  $ ./parser_runner.exe < manytests/typed/004manyargs.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/005fix.ml
  [(SingleDecl
      (DDeclaration (Rec, (PIdentifier "fix"),
         (EFun ((PIdentifier "f"),
            (EFun ((PIdentifier "x"),
               (EApplication (
                  (EApplication ((EIdentifier "f"),
                     (EApplication ((EIdentifier "fix"), (EIdentifier "f"))))),
                  (EIdentifier "x")))
               ))
            ))
         )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "fac"),
          (EFun ((PIdentifier "self"),
             (EFun ((PIdentifier "n"),
                (EIf (
                   (EApplication (
                      (EApplication ((EIdentifier "( <= )"), (EIdentifier "n")
                         )),
                      (EConst (CInt 1)))),
                   (EConst (CInt 1)),
                   (EApplication (
                      (EApplication ((EIdentifier "( * )"), (EIdentifier "n"))),
                      (EApplication ((EIdentifier "self"),
                         (EApplication (
                            (EApplication ((EIdentifier "( - )"),
                               (EIdentifier "n"))),
                            (EConst (CInt 1))))
                         ))
                      ))
                   ))
                ))
             ))
          )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "main"),
          (ELetIn (NoRec, PUnit,
             (EApplication ((EIdentifier "print_int"),
                (EApplication (
                   (EApplication ((EIdentifier "fix"), (EIdentifier "fac"))),
                   (EConst (CInt 6))))
                )),
             (EConst (CInt 0))))
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/006partial.ml
  [(SingleDecl
      (DDeclaration (NoRec, (PIdentifier "foo"),
         (EFun ((PIdentifier "b"),
            (EIf ((EIdentifier "b"),
               (EFun ((PIdentifier "foo"),
                  (EApplication (
                     (EApplication ((EIdentifier "( + )"), (EIdentifier "foo")
                        )),
                     (EConst (CInt 2))))
                  )),
               (EFun ((PIdentifier "foo"),
                  (EApplication (
                     (EApplication ((EIdentifier "( * )"), (EIdentifier "foo")
                        )),
                     (EConst (CInt 10))))
                  ))
               ))
            ))
         )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "foo"),
          (EFun ((PIdentifier "x"),
             (EApplication (
                (EApplication ((EIdentifier "foo"), (EConst (CBool true)))),
                (EApplication (
                   (EApplication ((EIdentifier "foo"), (EConst (CBool false)))),
                   (EApplication (
                      (EApplication ((EIdentifier "foo"), (EConst (CBool true))
                         )),
                      (EApplication (
                         (EApplication ((EIdentifier "foo"),
                            (EConst (CBool false)))),
                         (EIdentifier "x")))
                      ))
                   ))
                ))
             ))
          )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "main"),
          (ELetIn (NoRec, PUnit,
             (EApplication ((EIdentifier "print_int"),
                (EApplication ((EIdentifier "foo"), (EConst (CInt 11)))))),
             (EConst (CInt 0))))
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/006partial2.ml
  Error: : no more choices
  $ ./parser_runner.exe < manytests/typed/006partial3.ml
  [(SingleDecl
      (DDeclaration (NoRec, (PIdentifier "foo"),
         (EFun ((PIdentifier "a"),
            (ELetIn (NoRec, PUnit,
               (EApplication ((EIdentifier "print_int"), (EIdentifier "a"))),
               (EFun ((PIdentifier "b"),
                  (ELetIn (NoRec, PUnit,
                     (EApplication ((EIdentifier "print_int"),
                        (EIdentifier "b"))),
                     (EFun ((PIdentifier "c"),
                        (EApplication ((EIdentifier "print_int"),
                           (EIdentifier "c")))
                        ))
                     ))
                  ))
               ))
            ))
         )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "main"),
          (ELetIn (NoRec, PUnit,
             (EApplication (
                (EApplication (
                   (EApplication ((EIdentifier "foo"), (EConst (CInt 4)))),
                   (EConst (CInt 8)))),
                (EConst (CInt 9)))),
             (EConst (CInt 0))))
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/007order.ml
  Error: : no more choices

  $ ./parser_runner.exe < manytests/typed/008ascription.ml
  Error: : no more choices

  $ ./parser_runner.exe < manytests/typed/009let_poly.ml
  [(SingleDecl
      (DDeclaration (NoRec, (PIdentifier "temp"),
         (ELetIn (NoRec, (PIdentifier "f"),
            (EFun ((PIdentifier "x"), (EIdentifier "x"))),
            (ETuple
               [(EApplication ((EIdentifier "f"), (EConst (CInt 1))));
                 (EApplication ((EIdentifier "f"), (EConst (CBool true))))])
            ))
         )))
    ]

  $ ./parser_runner.exe < manytests/typed/010sukharev.ml
  Error: : no more choices

  $ ./parser_runner.exe < manytests/typed/015tuples.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/016lists.ml
  Error: : end_of_input

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
  [(SingleDecl
      (DDeclaration (Rec, (PIdentifier "fib_acc"),
         (EFun ((PIdentifier "a"),
            (EFun ((PIdentifier "b"),
               (EFun ((PIdentifier "n"),
                  (EIf (
                     (EApplication (
                        (EApplication ((EIdentifier "( = )"), (EIdentifier "n")
                           )),
                        (EConst (CInt 1)))),
                     (EIdentifier "b"),
                     (ELetIn (NoRec, (PIdentifier "n"),
                        (EFun ((PConst (CInt 1)),
                           (EApplication (
                              (EApplication ((EIdentifier "( - )"),
                                 (EIdentifier "n"))),
                              (EConst (CInt 1))))
                           )),
                        (ELetIn (NoRec, (PIdentifier "ab"),
                           (EApplication (
                              (EApplication ((EIdentifier "( + )"),
                                 (EIdentifier "a"))),
                              (EIdentifier "b"))),
                           (EApplication (
                              (EApplication (
                                 (EApplication (
                                    (EApplication ((EIdentifier "fib_acc"),
                                       (EIdentifier "b"))),
                                    (EIdentifier "ab"))),
                                 (EIdentifier "n"))),
                              (EConst (CInt 1))))
                           ))
                        ))
                     ))
                  ))
               ))
            ))
         )));
    (SingleDecl
       (DDeclaration (Rec, (PIdentifier "fib"),
          (EFun ((PIdentifier "n"),
             (EIf (
                (EApplication (
                   (EApplication ((EIdentifier "( < )"), (EIdentifier "n"))),
                   (EConst (CInt 2)))),
                (EIdentifier "n"),
                (EApplication (
                   (EApplication ((EIdentifier "( + )"),
                      (EApplication ((EIdentifier "fib"),
                         (EApplication (
                            (EApplication ((EIdentifier "( - )"),
                               (EIdentifier "n"))),
                            (EConst (CInt 1))))
                         ))
                      )),
                   (EApplication ((EIdentifier "fib"),
                      (EApplication (
                         (EApplication ((EIdentifier "( - )"),
                            (EIdentifier "n"))),
                         (EConst (CInt 2))))
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
                   (EApplication (
                      (EApplication ((EIdentifier "fib_acc"), (EConst (CInt 0))
                         )),
                      (EConst (CInt 1)))),
                   (EConst (CInt 4))))
                )),
             (ELetIn (NoRec, PUnit,
                (EApplication ((EIdentifier "print_int"),
                   (EApplication ((EIdentifier "fib"), (EConst (CInt 4)))))),
                (EConst (CInt 0))))
             ))
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/004manyargs.ml
  [(SingleDecl
      (DDeclaration (NoRec, (PIdentifier "wrap"),
         (EFun ((PIdentifier "f"),
            (EIf (
               (EApplication (
                  (EApplication ((EIdentifier "( = )"), (EConst (CInt 1)))),
                  (EConst (CInt 1)))),
               (EIdentifier "f"), (EIdentifier "f")))
            ))
         )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "test"),
          (EFun ((PConst (CInt 3)),
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
          )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "test"),
          (EFun ((PConst (CInt 10)),
             (EFun ((PIdentifier "a"),
                (EFun ((PIdentifier "b"),
                   (EFun ((PIdentifier "c"),
                      (EFun ((PIdentifier "d"),
                         (EFun ((PIdentifier "e"),
                            (EFun ((PIdentifier "f"),
                               (EFun ((PIdentifier "g"),
                                  (EFun ((PIdentifier "h"),
                                     (EFun ((PIdentifier "i"),
                                        (EFun ((PIdentifier "j"),
                                           (EApplication (
                                              (EApplication (
                                                 (EIdentifier "( + )"),
                                                 (EApplication (
                                                    (EApplication (
                                                       (EIdentifier "( + )"),
                                                       (EApplication (
                                                          (EApplication (
                                                             (EIdentifier
                                                                "( + )"),
                                                             (EApplication (
                                                                (EApplication (
                                                                   (EIdentifier
                                                                      "( + )"),
                                                                   (EApplication (
                                                                      (
                                                                      EApplication (
                                                                      (EIdentifier
                                                                      "( + )"),
                                                                      (EApplication (
                                                                      (EApplication (
                                                                      (EIdentifier
                                                                      "( + )"),
                                                                      (EApplication (
                                                                      (EApplication (
                                                                      (EIdentifier
                                                                      "( + )"),
                                                                      (EApplication (
                                                                      (EApplication (
                                                                      (EIdentifier
                                                                      "( + )"),
                                                                      (EApplication (
                                                                      (EApplication (
                                                                      (EIdentifier
                                                                      "( + )"),
                                                                      (EIdentifier
                                                                      "a"))),
                                                                      (EIdentifier
                                                                      "b"))))),
                                                                      (EIdentifier
                                                                      "c"))))),
                                                                      (EIdentifier
                                                                      "d"))))),
                                                                      (EIdentifier
                                                                      "e"))))),
                                                                      (
                                                                      EIdentifier
                                                                      "f")))
                                                                   )),
                                                                (EIdentifier
                                                                   "g")
                                                                ))
                                                             )),
                                                          (EIdentifier "h")))
                                                       )),
                                                    (EIdentifier "i")))
                                                 )),
                                              (EIdentifier "j")))
                                           ))
                                        ))
                                     ))
                                  ))
                               ))
                            ))
                         ))
                      ))
                   ))
                ))
             ))
          )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "main"),
          (ELetIn (NoRec, (PIdentifier "rez"),
             (EApplication (
                (EApplication (
                   (EApplication (
                      (EApplication (
                         (EApplication (
                            (EApplication (
                               (EApplication (
                                  (EApplication (
                                     (EApplication (
                                        (EApplication (
                                           (EApplication (
                                              (EApplication (
                                                 (EIdentifier "wrap"),
                                                 (EIdentifier "test"))),
                                              (EConst (CInt 10)))),
                                           (EConst (CInt 1)))),
                                        (EConst (CInt 10)))),
                                     (EConst (CInt 100)))),
                                  (EConst (CInt 1000)))),
                               (EConst (CInt 10000)))),
                            (EConst (CInt 100000)))),
                         (EConst (CInt 1000000)))),
                      (EConst (CInt 10000000)))),
                   (EConst (CInt 100000000)))),
                (EConst (CInt 1000000000)))),
             (ELetIn (NoRec, PUnit,
                (EApplication ((EIdentifier "print_int"), (EIdentifier "rez"))),
                (ELetIn (NoRec, (PIdentifier "temp"),
                   (EFun ((PConst (CInt 2)),
                      (EApplication (
                         (EApplication (
                            (EApplication (
                               (EApplication (
                                  (EApplication ((EIdentifier "wrap"),
                                     (EIdentifier "test"))),
                                  (EConst (CInt 3)))),
                               (EConst (CInt 1)))),
                            (EConst (CInt 10)))),
                         (EConst (CInt 100))))
                      )),
                   (EConst (CInt 0))))
                ))
             ))
          )))
    ]

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
  [(SingleDecl
      (DDeclaration (NoRec, (PIdentifier "foo"),
         (EFun ((PIdentifier "a"),
            (EFun ((PIdentifier "b"),
               (EFun ((PIdentifier "c"),
                  (ELetIn (NoRec, PUnit,
                     (EApplication ((EIdentifier "print_int"),
                        (EIdentifier "a"))),
                     (ELetIn (NoRec, PUnit,
                        (EApplication ((EIdentifier "print_int"),
                           (EIdentifier "b"))),
                        (ELetIn (NoRec, PUnit,
                           (EApplication ((EIdentifier "print_int"),
                              (EIdentifier "c"))),
                           (EApplication (
                              (EApplication ((EIdentifier "( + )"),
                                 (EIdentifier "a"))),
                              (EApplication (
                                 (EApplication ((EIdentifier "( * )"),
                                    (EIdentifier "b"))),
                                 (EIdentifier "c")))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            ))
         )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "main"),
          (ELetIn (NoRec, (PIdentifier "foo"),
             (EApplication ((EIdentifier "foo"), (EConst (CInt 1)))),
             (ELetIn (NoRec, (PIdentifier "foo"),
                (EApplication ((EIdentifier "foo"), (EConst (CInt 2)))),
                (ELetIn (NoRec, (PIdentifier "foo"),
                   (EApplication ((EIdentifier "foo"), (EConst (CInt 3)))),
                   (ELetIn (NoRec, PUnit,
                      (EApplication ((EIdentifier "print_int"),
                         (EIdentifier "foo"))),
                      (EConst (CInt 0))))
                   ))
                ))
             ))
          )))
    ]
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
       (DDeclaration (NoRec, (PIdentifier "map"),
          (EFun ((PIdentifier "f"),
             (EFun ((PIdentifier "p"),
                (ELetIn (NoRec,
                   (PTuple [(PIdentifier "a"); (PIdentifier "b")]),
                   (EIdentifier "p"),
                   (ETuple
                      [(EApplication ((EIdentifier "f"), (EIdentifier "a")));
                        (EApplication ((EIdentifier "f"), (EIdentifier "b")))])
                   ))
                ))
             ))
          )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "fixpoly"),
          (EFun ((PIdentifier "l"),
             (EApplication (
                (EApplication ((EIdentifier "fix"),
                   (EFun ((PIdentifier "self"),
                      (EFun ((PIdentifier "l"),
                         (EApplication (
                            (EApplication ((EIdentifier "map"),
                               (EFun ((PIdentifier "li"),
                                  (EFun ((PIdentifier "x"),
                                     (EApplication (
                                        (EApplication ((EIdentifier "li"),
                                           (EApplication ((EIdentifier "self"),
                                              (EIdentifier "l")))
                                           )),
                                        (EIdentifier "x")))
                                     ))
                                  ))
                               )),
                            (EIdentifier "l")))
                         ))
                      ))
                   )),
                (EIdentifier "l")))
             ))
          )));
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "feven"),
          (EFun ((PIdentifier "p"),
             (EFun ((PIdentifier "n"),
                (ELetIn (NoRec,
                   (PTuple [(PIdentifier "e"); (PIdentifier "o")]),
                   (EIdentifier "p"),
                   (EIf (
                      (EApplication (
                         (EApplication ((EIdentifier "( == )"),
                            (EIdentifier "n"))),
                         (EConst (CInt 0)))),
                      (EConst (CInt 1)),
                      (EApplication ((EIdentifier "o"),
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
       (DDeclaration (NoRec, (PIdentifier "fodd"),
          (EFun ((PIdentifier "p"),
             (EFun ((PIdentifier "n"),
                (ELetIn (NoRec,
                   (PTuple [(PIdentifier "e"); (PIdentifier "o")]),
                   (EIdentifier "p"),
                   (EIf (
                      (EApplication (
                         (EApplication ((EIdentifier "( == )"),
                            (EIdentifier "n"))),
                         (EConst (CInt 0)))),
                      (EConst (CInt 0)),
                      (EApplication ((EIdentifier "e"),
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
       (DDeclaration (NoRec, (PIdentifier "tie"),
          (EApplication ((EIdentifier "fixpoly"),
             (ETuple [(EIdentifier "feven"); (EIdentifier "fodd")])))
          )));
    (MutableRecDecl
       [(DDeclaration (Rec, (PIdentifier "meven"),
           (EFun ((PIdentifier "n"),
              (EIf (
                 (EApplication (
                    (EApplication ((EIdentifier "( = )"), (EIdentifier "n"))),
                    (EConst (CInt 0)))),
                 (EConst (CInt 1)),
                 (EApplication ((EIdentifier "modd"),
                    (EApplication (
                       (EApplication ((EIdentifier "( - )"), (EIdentifier "n")
                          )),
                       (EConst (CInt 1))))
                    ))
                 ))
              ))
           ));
         (DDeclaration (NoRec, (PIdentifier "modd"),
            (EFun ((PIdentifier "n"),
               (EIf (
                  (EApplication (
                     (EApplication ((EIdentifier "( = )"), (EIdentifier "n"))),
                     (EConst (CInt 0)))),
                  (EConst (CInt 1)),
                  (EApplication ((EIdentifier "meven"),
                     (EApplication (
                        (EApplication ((EIdentifier "( - )"), (EIdentifier "n")
                           )),
                        (EConst (CInt 1))))
                     ))
                  ))
               ))
            ))
         ]);
    (SingleDecl
       (DDeclaration (NoRec, (PIdentifier "main"),
          (ELetIn (NoRec, PUnit,
             (EApplication ((EIdentifier "print_int"),
                (EApplication ((EIdentifier "modd"), (EConst (CInt 1)))))),
             (ELetIn (NoRec, PUnit,
                (EApplication ((EIdentifier "print_int"),
                   (EApplication ((EIdentifier "meven"), (EConst (CInt 2)))))),
                (ELetIn (NoRec,
                   (PTuple [(PIdentifier "even"); (PIdentifier "odd")]),
                   (EIdentifier "tie"),
                   (ELetIn (NoRec, PUnit,
                      (EApplication ((EIdentifier "print_int"),
                         (EApplication ((EIdentifier "odd"), (EConst (CInt 3))
                            ))
                         )),
                      (ELetIn (NoRec, PUnit,
                         (EApplication ((EIdentifier "print_int"),
                            (EApplication ((EIdentifier "even"),
                               (EConst (CInt 4))))
                            )),
                         (EConst (CInt 0))))
                      ))
                   ))
                ))
             ))
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/016lists.ml
  Error: : end_of_input

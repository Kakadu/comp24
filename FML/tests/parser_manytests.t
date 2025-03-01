  $ ./parser_runner.exe < manytests/typed/001fac.ml
  [(RecDecl
      [(DDeclaration ((PIdentifier "fac"),
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
                         (EApplication ((EIdentifier "( - )"),
                            (EIdentifier "n"))),
                         (EConst (CInt 1))))
                      ))
                   ))
                ))
             ))
          ))
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
           (ELetIn (NoRec, PUnit,
              (EApplication ((EIdentifier "print_int"),
                 (EApplication ((EIdentifier "fac"), (EConst (CInt 4)))))),
              (EConst (CInt 0))))
           ))
         ])
    ]

  $ ./parser_runner.exe < manytests/typed/002fac.ml
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
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
           (ELetIn (NoRec, PUnit,
              (EApplication ((EIdentifier "print_int"),
                 (EApplication (
                    (EApplication ((EIdentifier "fac_cps"), (EConst (CInt 4)))),
                    (EFun ((PIdentifier "print_int"), (EIdentifier "print_int")
                       ))
                    ))
                 )),
              (EConst (CInt 0))))
           ))
         ])
    ]

  $ ./parser_runner.exe < manytests/typed/003fib.ml
  [(RecDecl
      [(DDeclaration ((PIdentifier "fib_acc"),
          (EFun ((PIdentifier "a"),
             (EFun ((PIdentifier "b"),
                (EFun ((PIdentifier "n"),
                   (EIf (
                      (EApplication (
                         (EApplication ((EIdentifier "( = )"),
                            (EIdentifier "n"))),
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
          ))
        ]);
    (RecDecl
       [(DDeclaration ((PIdentifier "fib"),
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
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
           (ELetIn (NoRec, PUnit,
              (EApplication ((EIdentifier "print_int"),
                 (EApplication (
                    (EApplication (
                       (EApplication ((EIdentifier "fib_acc"),
                          (EConst (CInt 0)))),
                       (EConst (CInt 1)))),
                    (EConst (CInt 4))))
                 )),
              (ELetIn (NoRec, PUnit,
                 (EApplication ((EIdentifier "print_int"),
                    (EApplication ((EIdentifier "fib"), (EConst (CInt 4)))))),
                 (EConst (CInt 0))))
              ))
           ))
         ])
    ]

  $ ./parser_runner.exe < manytests/typed/004manyargs.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "wrap"),
          (EFun ((PIdentifier "f"),
             (EIf (
                (EApplication (
                   (EApplication ((EIdentifier "( = )"), (EConst (CInt 1)))),
                   (EConst (CInt 1)))),
                (EIdentifier "f"), (EIdentifier "f")))
             ))
          ))
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "test"),
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
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "test"),
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
                                                                      (EIdentifier
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
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
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
                 (EApplication ((EIdentifier "print_int"), (EIdentifier "rez")
                    )),
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
           ))
         ])
    ]

  $ ./parser_runner.exe < manytests/typed/005fix.ml
  [(RecDecl
      [(DDeclaration ((PIdentifier "fix"),
          (EFun ((PIdentifier "f"),
             (EFun ((PIdentifier "x"),
                (EApplication (
                   (EApplication ((EIdentifier "f"),
                      (EApplication ((EIdentifier "fix"), (EIdentifier "f"))))),
                   (EIdentifier "x")))
                ))
             ))
          ))
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "fac"),
           (EFun ((PIdentifier "self"),
              (EFun ((PIdentifier "n"),
                 (EIf (
                    (EApplication (
                       (EApplication ((EIdentifier "( <= )"), (EIdentifier "n")
                          )),
                       (EConst (CInt 1)))),
                    (EConst (CInt 1)),
                    (EApplication (
                       (EApplication ((EIdentifier "( * )"), (EIdentifier "n")
                          )),
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
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
           (ELetIn (NoRec, PUnit,
              (EApplication ((EIdentifier "print_int"),
                 (EApplication (
                    (EApplication ((EIdentifier "fix"), (EIdentifier "fac"))),
                    (EConst (CInt 6))))
                 )),
              (EConst (CInt 0))))
           ))
         ])
    ]

  $ ./parser_runner.exe < manytests/typed/006partial.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "foo"),
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
          ))
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "foo"),
           (EFun ((PIdentifier "x"),
              (EApplication (
                 (EApplication ((EIdentifier "foo"), (EConst (CBool true)))),
                 (EApplication (
                    (EApplication ((EIdentifier "foo"), (EConst (CBool false))
                       )),
                    (EApplication (
                       (EApplication ((EIdentifier "foo"),
                          (EConst (CBool true)))),
                       (EApplication (
                          (EApplication ((EIdentifier "foo"),
                             (EConst (CBool false)))),
                          (EIdentifier "x")))
                       ))
                    ))
                 ))
              ))
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
           (ELetIn (NoRec, PUnit,
              (EApplication ((EIdentifier "print_int"),
                 (EApplication ((EIdentifier "foo"), (EConst (CInt 11)))))),
              (EConst (CInt 0))))
           ))
         ])
    ]

  $ ./parser_runner.exe < manytests/typed/006partial2.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "foo"),
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
          ))
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
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
           ))
         ])
    ]
  $ ./parser_runner.exe < manytests/typed/006partial3.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "foo"),
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
          ))
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
           (ELetIn (NoRec, PUnit,
              (EApplication (
                 (EApplication (
                    (EApplication ((EIdentifier "foo"), (EConst (CInt 4)))),
                    (EConst (CInt 8)))),
                 (EConst (CInt 9)))),
              (EConst (CInt 0))))
           ))
         ])
    ]

  $ ./parser_runner.exe < manytests/typed/008ascription.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "addi"),
          (EFun ((PIdentifier "f"),
             (EFun ((PIdentifier "g"),
                (EFun ((PIdentifier "x"),
                   (EConstraint (
                      (EApplication (
                         (EApplication ((EIdentifier "f"), (EIdentifier "x"))),
                         (EConstraint (
                            (EApplication ((EIdentifier "g"), (EIdentifier "x")
                               )),
                            ABool))
                         )),
                      AInt))
                   ))
                ))
             ))
          ))
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
           (ELetIn (NoRec, PUnit,
              (EApplication ((EIdentifier "print_int"),
                 (EApplication (
                    (EApplication (
                       (EApplication ((EIdentifier "addi"),
                          (EFun ((PIdentifier "x"),
                             (EFun ((PIdentifier "b"),
                                (EIf ((EIdentifier "b"),
                                   (EApplication (
                                      (EApplication ((EIdentifier "( + )"),
                                         (EIdentifier "x"))),
                                      (EConst (CInt 1)))),
                                   (EApplication (
                                      (EApplication ((EIdentifier "( * )"),
                                         (EIdentifier "x"))),
                                      (EConst (CInt 2))))
                                   ))
                                ))
                             ))
                          )),
                       (EFun ((PIdentifier "_start"),
                          (EApplication (
                             (EApplication ((EIdentifier "( = )"),
                                (EApplication (
                                   (EApplication ((EIdentifier "( / )"),
                                      (EIdentifier "_start"))),
                                   (EConst (CInt 2))))
                                )),
                             (EConst (CInt 0))))
                          ))
                       )),
                    (EConst (CInt 4))))
                 )),
              (EConst (CInt 0))))
           ))
         ])
    ]

  $ ./parser_runner.exe < manytests/typed/009let_poly.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "temp"),
          (ELetIn (NoRec, (PIdentifier "f"),
             (EFun ((PIdentifier "x"), (EIdentifier "x"))),
             (ETuple
                [(EApplication ((EIdentifier "f"), (EConst (CInt 1))));
                  (EApplication ((EIdentifier "f"), (EConst (CBool true))))])
             ))
          ))
        ])
    ]

  $ ./parser_runner.exe < manytests/typed/015tuples.ml
  [(RecDecl
      [(DDeclaration ((PIdentifier "fix"),
          (EFun ((PIdentifier "f"),
             (EFun ((PIdentifier "x"),
                (EApplication (
                   (EApplication ((EIdentifier "f"),
                      (EApplication ((EIdentifier "fix"), (EIdentifier "f"))))),
                   (EIdentifier "x")))
                ))
             ))
          ))
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "map"),
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
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "fixpoly"),
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
                                            (EApplication (
                                               (EIdentifier "self"),
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
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "feven"),
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
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "fodd"),
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
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "tie"),
           (EApplication ((EIdentifier "fixpoly"),
              (ETuple [(EIdentifier "feven"); (EIdentifier "fodd")])))
           ))
         ]);
    (RecDecl
       [(DDeclaration ((PIdentifier "meven"),
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
         (DDeclaration ((PIdentifier "modd"),
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
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
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
           ))
         ])
    ]

  $ ./parser_runner.exe < manytests/typed/016lists.ml
  [(RecDecl
      [(DDeclaration ((PIdentifier "length"),
          (EFun ((PIdentifier "xs"),
             (EMatch ((EIdentifier "xs"),
                [(PNill, (EConst (CInt 0)));
                  ((PCons ((PIdentifier "h"), (PIdentifier "tl"))),
                   (EApplication (
                      (EApplication ((EIdentifier "( + )"), (EConst (CInt 1)))),
                      (EApplication ((EIdentifier "length"), (EIdentifier "tl")
                         ))
                      )))
                  ]
                ))
             ))
          ))
        ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "length_tail"),
           (ELetIn (Rec, (PIdentifier "helper"),
              (EFun ((PIdentifier "acc"),
                 (EFun ((PIdentifier "xs"),
                    (EMatch ((EIdentifier "xs"),
                       [(PNill, (EIdentifier "acc"));
                         ((PCons ((PIdentifier "h"), (PIdentifier "tl"))),
                          (EApplication (
                             (EApplication ((EIdentifier "helper"),
                                (EApplication (
                                   (EApplication ((EIdentifier "( + )"),
                                      (EIdentifier "acc"))),
                                   (EConst (CInt 1))))
                                )),
                             (EIdentifier "tl"))))
                         ]
                       ))
                    ))
                 )),
              (EApplication ((EIdentifier "helper"), (EConst (CInt 0))))))
           ))
         ]);
    (RecDecl
       [(DDeclaration ((PIdentifier "map"),
           (EFun ((PIdentifier "f"),
              (EFun ((PIdentifier "xs"),
                 (EMatch ((EIdentifier "xs"),
                    [(PNill, ENill);
                      ((PCons ((PIdentifier "a"), PNill)),
                       (ECons (
                          (EApplication ((EIdentifier "f"), (EIdentifier "a"))),
                          ENill)));
                      ((PCons ((PIdentifier "a"),
                          (PCons ((PIdentifier "b"), PNill)))),
                       (ECons (
                          (EApplication ((EIdentifier "f"), (EIdentifier "a"))),
                          (ECons (
                             (EApplication ((EIdentifier "f"),
                                (EIdentifier "b"))),
                             ENill))
                          )));
                      ((PCons ((PIdentifier "a"),
                          (PCons ((PIdentifier "b"),
                             (PCons ((PIdentifier "c"), PNill))))
                          )),
                       (ECons (
                          (EApplication ((EIdentifier "f"), (EIdentifier "a"))),
                          (ECons (
                             (EApplication ((EIdentifier "f"),
                                (EIdentifier "b"))),
                             (ECons (
                                (EApplication ((EIdentifier "f"),
                                   (EIdentifier "c"))),
                                ENill))
                             ))
                          )));
                      ((PCons ((PIdentifier "a"),
                          (PCons ((PIdentifier "b"),
                             (PCons ((PIdentifier "c"),
                                (PCons ((PIdentifier "d"), (PIdentifier "tl")))
                                ))
                             ))
                          )),
                       (ECons (
                          (EApplication ((EIdentifier "f"), (EIdentifier "a"))),
                          (ECons (
                             (EApplication ((EIdentifier "f"),
                                (EIdentifier "b"))),
                             (ECons (
                                (EApplication ((EIdentifier "f"),
                                   (EIdentifier "c"))),
                                (ECons (
                                   (EApplication ((EIdentifier "f"),
                                      (EIdentifier "d"))),
                                   (EApplication (
                                      (EApplication ((EIdentifier "map"),
                                         (EIdentifier "f"))),
                                      (EIdentifier "tl")))
                                   ))
                                ))
                             ))
                          )))
                      ]
                    ))
                 ))
              ))
           ))
         ]);
    (RecDecl
       [(DDeclaration ((PIdentifier "append"),
           (EFun ((PIdentifier "xs"),
              (EFun ((PIdentifier "ys"),
                 (EMatch ((EIdentifier "xs"),
                    [(PNill, (EIdentifier "ys"));
                      ((PCons ((PIdentifier "x"), (PIdentifier "xs"))),
                       (ECons ((EIdentifier "x"),
                          (EApplication (
                             (EApplication ((EIdentifier "append"),
                                (EIdentifier "xs"))),
                             (EIdentifier "ys")))
                          )))
                      ]
                    ))
                 ))
              ))
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "concat"),
           (ELetIn (Rec, (PIdentifier "helper"),
              (EFun ((PIdentifier "xs"),
                 (EMatch ((EIdentifier "xs"),
                    [(PNill, ENill);
                      ((PCons ((PIdentifier "h"), (PIdentifier "tl"))),
                       (EApplication (
                          (EApplication ((EIdentifier "append"),
                             (EIdentifier "h"))),
                          (EApplication ((EIdentifier "helper"),
                             (EIdentifier "tl")))
                          )))
                      ]
                    ))
                 )),
              (EIdentifier "helper")))
           ))
         ]);
    (RecDecl
       [(DDeclaration ((PIdentifier "iter"),
           (EFun ((PIdentifier "f"),
              (EFun ((PIdentifier "xs"),
                 (EMatch ((EIdentifier "xs"),
                    [(PNill, EUnit);
                      ((PCons ((PIdentifier "h"), (PIdentifier "tl"))),
                       (ELetIn (NoRec, PUnit,
                          (EApplication ((EIdentifier "f"), (EIdentifier "h"))),
                          (EApplication (
                             (EApplication ((EIdentifier "iter"),
                                (EIdentifier "f"))),
                             (EIdentifier "tl")))
                          )))
                      ]
                    ))
                 ))
              ))
           ))
         ]);
    (RecDecl
       [(DDeclaration ((PIdentifier "cartesian"),
           (EFun ((PIdentifier "xs"),
              (EFun ((PIdentifier "ys"),
                 (EMatch ((EIdentifier "xs"),
                    [(PNill, ENill);
                      ((PCons ((PIdentifier "h"), (PIdentifier "tl"))),
                       (EApplication (
                          (EApplication ((EIdentifier "append"),
                             (EApplication (
                                (EApplication ((EIdentifier "map"),
                                   (EFun ((PIdentifier "a"),
                                      (ETuple
                                         [(EIdentifier "h"); (EIdentifier "a")])
                                      ))
                                   )),
                                (EIdentifier "ys")))
                             )),
                          (EApplication (
                             (EApplication ((EIdentifier "cartesian"),
                                (EIdentifier "tl"))),
                             (EIdentifier "ys")))
                          )))
                      ]
                    ))
                 ))
              ))
           ))
         ]);
    (NoRecDecl
       [(DDeclaration ((PIdentifier "main"),
           (ELetIn (NoRec, PUnit,
              (EApplication (
                 (EApplication ((EIdentifier "iter"), (EIdentifier "print_int")
                    )),
                 (ECons ((EConst (CInt 1)),
                    (ECons ((EConst (CInt 2)),
                       (ECons ((EConst (CInt 3)), ENill))))
                    ))
                 )),
              (ELetIn (NoRec, PUnit,
                 (EApplication ((EIdentifier "print_int"),
                    (EApplication ((EIdentifier "length"),
                       (EApplication (
                          (EApplication ((EIdentifier "cartesian"),
                             (ECons ((EConst (CInt 1)),
                                (ECons ((EConst (CInt 2)), ENill))))
                             )),
                          (ECons ((EConst (CInt 1)),
                             (ECons ((EConst (CInt 2)),
                                (ECons ((EConst (CInt 3)),
                                   (ECons ((EConst (CInt 4)), ENill))))
                                ))
                             ))
                          ))
                       ))
                    )),
                 (EConst (CInt 0))))
              ))
           ))
         ])
    ]
 
  $ ./parser_runner.exe < manytests/do_not_type/001.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "recfac"),
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
  $ ./parser_runner.exe < manytests/do_not_type/002if.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "main"),
          (EIf ((EConst (CBool true)), (EConst (CInt 1)),
             (EConst (CBool false))))
          ))
        ])
    ]
  $ ./parser_runner.exe < manytests/do_not_type/003occurs.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "fix"),
          (EFun ((PIdentifier "f"),
             (EApplication (
                (EFun ((PIdentifier "x"),
                   (EApplication ((EIdentifier "f"),
                      (EFun ((PIdentifier "f"),
                         (EApplication (
                            (EApplication ((EIdentifier "x"), (EIdentifier "x")
                               )),
                            (EIdentifier "f")))
                         ))
                      ))
                   )),
                (EFun ((PIdentifier "x"),
                   (EApplication ((EIdentifier "f"),
                      (EFun ((PIdentifier "f"),
                         (EApplication (
                            (EApplication ((EIdentifier "x"), (EIdentifier "x")
                               )),
                            (EIdentifier "f")))
                         ))
                      ))
                   ))
                ))
             ))
          ))
        ])
    ]

  $ ./parser_runner.exe < manytests/do_not_type/004let_poly.ml
  [(NoRecDecl
      [(DDeclaration ((PIdentifier "temp"),
          (EApplication (
             (EFun ((PIdentifier "f"),
                (ETuple
                   [(EApplication ((EIdentifier "f"), (EConst (CInt 1))));
                     (EApplication ((EIdentifier "f"), (EConst (CBool true))))])
                )),
             (EFun ((PIdentifier "x"), (EIdentifier "x")))))
          ))
        ])
    ]

  $ ./parser_runner.exe < manytests/do_not_type/015tuples.ml
  [(RecDecl
      [(DDeclaration ((PTuple [(PIdentifier "a"); (PIdentifier "b")]),
          (ETuple [(EIdentifier "a"); (EIdentifier "b")])))
        ])
    ]



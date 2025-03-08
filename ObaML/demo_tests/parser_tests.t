  $ ./parser_tests.exe < manytests/typed/001fac.ml
  [(SILet (Recursive,
      [((PVar (Id "fac")),
        (EFun ([(PVar (Id "n"))],
           (EIf (
              (EApp ((EApp ((EVar (Id "( <= )")), (EVar (Id "n")))),
                 (EConst (CInt 1)))),
              (EConst (CInt 1)),
              (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "n")))),
                 (EApp ((EVar (Id "fac")),
                    (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                       (EConst (CInt 1))))
                    ))
                 ))
              ))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PConst CUnit),
             (EApp ((EVar (Id "print_int")),
                (EApp ((EVar (Id "fac")), (EConst (CInt 4))))))),
            (EConst (CInt 0)))))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/002fac.ml
  [(SILet (Recursive,
      [((PVar (Id "fac_cps")),
        (EFun ([(PVar (Id "n")); (PVar (Id "k"))],
           (EIf (
              (EApp ((EApp ((EVar (Id "( = )")), (EVar (Id "n")))),
                 (EConst (CInt 1)))),
              (EApp ((EVar (Id "k")), (EConst (CInt 1)))),
              (EApp (
                 (EApp ((EVar (Id "fac_cps")),
                    (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                       (EConst (CInt 1))))
                    )),
                 (EFun ([(PVar (Id "p"))],
                    (EApp ((EVar (Id "k")),
                       (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "p")))),
                          (EVar (Id "n"))))
                       ))
                    ))
                 ))
              ))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PConst CUnit),
             (EApp ((EVar (Id "print_int")),
                (EApp ((EApp ((EVar (Id "fac_cps")), (EConst (CInt 4)))),
                   (EFun ([(PVar (Id "print_int"))], (EVar (Id "print_int"))))
                   ))
                ))),
            (EConst (CInt 0)))))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/003fib.ml
  [(SILet (Recursive,
      [((PVar (Id "fib_acc")),
        (EFun ([(PVar (Id "a")); (PVar (Id "b")); (PVar (Id "n"))],
           (EIf (
              (EApp ((EApp ((EVar (Id "( = )")), (EVar (Id "n")))),
                 (EConst (CInt 1)))),
              (EVar (Id "b")),
              (ELet (Nonrecursive,
                 ((PVar (Id "n1")),
                  (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                     (EConst (CInt 1))))),
                 (ELet (Nonrecursive,
                    ((PVar (Id "ab")),
                     (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "a")))),
                        (EVar (Id "b"))))),
                    (EApp (
                       (EApp ((EApp ((EVar (Id "fib_acc")), (EVar (Id "b")))),
                          (EVar (Id "ab")))),
                       (EVar (Id "n1"))))
                    ))
                 ))
              ))
           )))
        ]
      ));
    (SILet (Recursive,
       [((PVar (Id "fib")),
         (EFun ([(PVar (Id "n"))],
            (EIf (
               (EApp ((EApp ((EVar (Id "( < )")), (EVar (Id "n")))),
                  (EConst (CInt 2)))),
               (EVar (Id "n")),
               (EApp (
                  (EApp ((EVar (Id "( + )")),
                     (EApp ((EVar (Id "fib")),
                        (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                           (EConst (CInt 1))))
                        ))
                     )),
                  (EApp ((EVar (Id "fib")),
                     (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                        (EConst (CInt 2))))
                     ))
                  ))
               ))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PConst CUnit),
             (EApp ((EVar (Id "print_int")),
                (EApp (
                   (EApp ((EApp ((EVar (Id "fib_acc")), (EConst (CInt 0)))),
                      (EConst (CInt 1)))),
                   (EConst (CInt 4))))
                ))),
            (ELet (Nonrecursive,
               ((PConst CUnit),
                (EApp ((EVar (Id "print_int")),
                   (EApp ((EVar (Id "fib")), (EConst (CInt 4))))))),
               (EConst (CInt 0))))
            )))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/004manyargs.ml
  [(SILet (Nonrecursive,
      [((PVar (Id "wrap")),
        (EFun ([(PVar (Id "f"))],
           (EIf (
              (EApp ((EApp ((EVar (Id "( = )")), (EConst (CInt 1)))),
                 (EConst (CInt 1)))),
              (EVar (Id "f")), (EVar (Id "f"))))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "test3")),
         (EFun ([(PVar (Id "a")); (PVar (Id "b")); (PVar (Id "c"))],
            (ELet (Nonrecursive,
               ((PVar (Id "a")),
                (EApp ((EVar (Id "print_int")), (EVar (Id "a"))))),
               (ELet (Nonrecursive,
                  ((PVar (Id "b")),
                   (EApp ((EVar (Id "print_int")), (EVar (Id "b"))))),
                  (ELet (Nonrecursive,
                     ((PVar (Id "c")),
                      (EApp ((EVar (Id "print_int")), (EVar (Id "c"))))),
                     (EConst (CInt 0))))
                  ))
               ))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "test10")),
         (EFun (
            [(PVar (Id "a")); (PVar (Id "b")); (PVar (Id "c"));
              (PVar (Id "d")); (PVar (Id "e")); (PVar (Id "f"));
              (PVar (Id "g")); (PVar (Id "h")); (PVar (Id "i"));
              (PVar (Id "j"))],
            (EApp (
               (EApp ((EVar (Id "( + )")),
                  (EApp (
                     (EApp ((EVar (Id "( + )")),
                        (EApp (
                           (EApp ((EVar (Id "( + )")),
                              (EApp (
                                 (EApp ((EVar (Id "( + )")),
                                    (EApp (
                                       (EApp ((EVar (Id "( + )")),
                                          (EApp (
                                             (EApp ((EVar (Id "( + )")),
                                                (EApp (
                                                   (EApp ((EVar (Id "( + )")),
                                                      (EApp (
                                                         (EApp (
                                                            (EVar (Id "( + )")),
                                                            (EApp (
                                                               (EApp (
                                                                  (EVar
                                                                     (Id
                                                                      "( + )")),
                                                                  (EVar
                                                                     (Id "a"))
                                                                  )),
                                                               (EVar (Id "b"))
                                                               ))
                                                            )),
                                                         (EVar (Id "c"))))
                                                      )),
                                                   (EVar (Id "d"))))
                                                )),
                                             (EVar (Id "e"))))
                                          )),
                                       (EVar (Id "f"))))
                                    )),
                                 (EVar (Id "g"))))
                              )),
                           (EVar (Id "h"))))
                        )),
                     (EVar (Id "i"))))
                  )),
               (EVar (Id "j"))))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PVar (Id "rez")),
             (EApp (
                (EApp (
                   (EApp (
                      (EApp (
                         (EApp (
                            (EApp (
                               (EApp (
                                  (EApp (
                                     (EApp (
                                        (EApp (
                                           (EApp ((EVar (Id "wrap")),
                                              (EVar (Id "test10")))),
                                           (EConst (CInt 1)))),
                                        (EConst (CInt 10)))),
                                     (EConst (CInt 100)))),
                                  (EConst (CInt 1000)))),
                               (EConst (CInt 10000)))),
                            (EConst (CInt 100000)))),
                         (EConst (CInt 1000000)))),
                      (EConst (CInt 10000000)))),
                   (EConst (CInt 100000000)))),
                (EConst (CInt 1000000000))))),
            (ELet (Nonrecursive,
               ((PConst CUnit),
                (EApp ((EVar (Id "print_int")), (EVar (Id "rez"))))),
               (ELet (Nonrecursive,
                  ((PVar (Id "temp2")),
                   (EApp (
                      (EApp (
                         (EApp (
                            (EApp ((EVar (Id "wrap")), (EVar (Id "test3")))),
                            (EConst (CInt 1)))),
                         (EConst (CInt 10)))),
                      (EConst (CInt 100))))),
                  (EConst (CInt 0))))
               ))
            )))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/005fix.ml
  [(SILet (Recursive,
      [((PVar (Id "fix")),
        (EFun ([(PVar (Id "f")); (PVar (Id "x"))],
           (EApp (
              (EApp ((EVar (Id "f")),
                 (EApp ((EVar (Id "fix")), (EVar (Id "f")))))),
              (EVar (Id "x"))))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "fac")),
         (EFun ([(PVar (Id "self")); (PVar (Id "n"))],
            (EIf (
               (EApp ((EApp ((EVar (Id "( <= )")), (EVar (Id "n")))),
                  (EConst (CInt 1)))),
               (EConst (CInt 1)),
               (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "n")))),
                  (EApp ((EVar (Id "self")),
                     (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                        (EConst (CInt 1))))
                     ))
                  ))
               ))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PConst CUnit),
             (EApp ((EVar (Id "print_int")),
                (EApp ((EApp ((EVar (Id "fix")), (EVar (Id "fac")))),
                   (EConst (CInt 6))))
                ))),
            (EConst (CInt 0)))))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/006partial.ml
  [(SILet (Nonrecursive,
      [((PVar (Id "foo")),
        (EFun ([(PVar (Id "b"))],
           (EIf ((EVar (Id "b")),
              (EFun ([(PVar (Id "foo"))],
                 (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "foo")))),
                    (EConst (CInt 2))))
                 )),
              (EFun ([(PVar (Id "foo"))],
                 (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "foo")))),
                    (EConst (CInt 10))))
                 ))
              ))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "foo")),
         (EFun ([(PVar (Id "x"))],
            (EApp ((EApp ((EVar (Id "foo")), (EConst (CBool true)))),
               (EApp ((EApp ((EVar (Id "foo")), (EConst (CBool false)))),
                  (EApp ((EApp ((EVar (Id "foo")), (EConst (CBool true)))),
                     (EApp ((EApp ((EVar (Id "foo")), (EConst (CBool false)))),
                        (EVar (Id "x"))))
                     ))
                  ))
               ))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PConst CUnit),
             (EApp ((EVar (Id "print_int")),
                (EApp ((EVar (Id "foo")), (EConst (CInt 11))))))),
            (EConst (CInt 0)))))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/006partial2.ml
  [(SILet (Nonrecursive,
      [((PVar (Id "foo")),
        (EFun ([(PVar (Id "a")); (PVar (Id "b")); (PVar (Id "c"))],
           (ELet (Nonrecursive,
              ((PConst CUnit),
               (EApp ((EVar (Id "print_int")), (EVar (Id "a"))))),
              (ELet (Nonrecursive,
                 ((PConst CUnit),
                  (EApp ((EVar (Id "print_int")), (EVar (Id "b"))))),
                 (ELet (Nonrecursive,
                    ((PConst CUnit),
                     (EApp ((EVar (Id "print_int")), (EVar (Id "c"))))),
                    (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "a")))),
                       (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "b")))),
                          (EVar (Id "c"))))
                       ))
                    ))
                 ))
              ))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PVar (Id "foo")), (EApp ((EVar (Id "foo")), (EConst (CInt 1))))),
            (ELet (Nonrecursive,
               ((PVar (Id "foo")),
                (EApp ((EVar (Id "foo")), (EConst (CInt 2))))),
               (ELet (Nonrecursive,
                  ((PVar (Id "foo")),
                   (EApp ((EVar (Id "foo")), (EConst (CInt 3))))),
                  (ELet (Nonrecursive,
                     ((PConst CUnit),
                      (EApp ((EVar (Id "print_int")), (EVar (Id "foo"))))),
                     (EConst (CInt 0))))
                  ))
               ))
            )))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/006partial3.ml
  [(SILet (Nonrecursive,
      [((PVar (Id "foo")),
        (EFun ([(PVar (Id "a"))],
           (ELet (Nonrecursive,
              ((PConst CUnit),
               (EApp ((EVar (Id "print_int")), (EVar (Id "a"))))),
              (EFun ([(PVar (Id "b"))],
                 (ELet (Nonrecursive,
                    ((PConst CUnit),
                     (EApp ((EVar (Id "print_int")), (EVar (Id "b"))))),
                    (EFun ([(PVar (Id "c"))],
                       (EApp ((EVar (Id "print_int")), (EVar (Id "c"))))))
                    ))
                 ))
              ))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PConst CUnit),
             (EApp (
                (EApp ((EApp ((EVar (Id "foo")), (EConst (CInt 4)))),
                   (EConst (CInt 8)))),
                (EConst (CInt 9))))),
            (EConst (CInt 0)))))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/007order.ml
  [(SILet (Nonrecursive,
      [((PVar (Id "_start")),
        (EFun (
           [(PConst CUnit); (PConst CUnit); (PVar (Id "a")); (PConst CUnit);
             (PVar (Id "b")); (PVar (Id "_c")); (PConst CUnit);
             (PVar (Id "d")); (PVar (Id "__"))],
           (ELet (Nonrecursive,
              ((PConst CUnit),
               (EApp ((EVar (Id "print_int")),
                  (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "a")))),
                     (EVar (Id "b"))))
                  ))),
              (ELet (Nonrecursive,
                 ((PConst CUnit),
                  (EApp ((EVar (Id "print_int")), (EVar (Id "__"))))),
                 (EApp (
                    (EApp ((EVar (Id "( + )")),
                       (EApp (
                          (EApp ((EVar (Id "( / )")),
                             (EApp (
                                (EApp ((EVar (Id "( * )")), (EVar (Id "a")))),
                                (EVar (Id "b"))))
                             )),
                          (EVar (Id "_c"))))
                       )),
                    (EVar (Id "d"))))
                 ))
              ))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (EApp ((EVar (Id "print_int")),
            (EApp (
               (EApp (
                  (EApp (
                     (EApp (
                        (EApp (
                           (EApp (
                              (EApp (
                                 (EApp (
                                    (EApp ((EVar (Id "_start")),
                                       (EApp ((EVar (Id "print_int")),
                                          (EConst (CInt 1))))
                                       )),
                                    (EApp ((EVar (Id "print_int")),
                                       (EConst (CInt 2))))
                                    )),
                                 (EConst (CInt 3)))),
                              (EApp ((EVar (Id "print_int")), (EConst (CInt 4))
                                 ))
                              )),
                           (EConst (CInt 100)))),
                        (EConst (CInt 1000)))),
                     (EApp ((EVar (Id "print_int")),
                        (EApp ((EVar (Id "( ~- )")), (EConst (CInt 1))))))
                     )),
                  (EConst (CInt 10000)))),
               (EApp ((EVar (Id "( ~- )")), (EConst (CInt 555555))))))
            )))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/008ascription.ml
  [(SILet (Nonrecursive,
      [((PVar (Id "addi")),
        (EFun ([(PVar (Id "f")); (PVar (Id "g")); (PVar (Id "x"))],
           (EType (
              (EApp ((EApp ((EVar (Id "f")), (EVar (Id "x")))),
                 (EType ((EApp ((EVar (Id "g")), (EVar (Id "x")))), TBool)))),
              TInt))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PConst CUnit),
             (EApp ((EVar (Id "print_int")),
                (EApp (
                   (EApp (
                      (EApp ((EVar (Id "addi")),
                         (EFun ([(PVar (Id "x")); (PVar (Id "b"))],
                            (EIf ((EVar (Id "b")),
                               (EApp (
                                  (EApp ((EVar (Id "( + )")), (EVar (Id "x")))),
                                  (EConst (CInt 1)))),
                               (EApp (
                                  (EApp ((EVar (Id "( * )")), (EVar (Id "x")))),
                                  (EConst (CInt 2))))
                               ))
                            ))
                         )),
                      (EFun ([(PVar (Id "_start"))],
                         (EApp (
                            (EApp ((EVar (Id "( = )")),
                               (EApp (
                                  (EApp ((EVar (Id "( / )")),
                                     (EVar (Id "_start")))),
                                  (EConst (CInt 2))))
                               )),
                            (EConst (CInt 0))))
                         ))
                      )),
                   (EConst (CInt 4))))
                ))),
            (EConst (CInt 0)))))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/009let_poly.ml
  [(SILet (Nonrecursive,
      [((PVar (Id "temp")),
        (ELet (Nonrecursive,
           ((PVar (Id "f")), (EFun ([(PVar (Id "x"))], (EVar (Id "x"))))),
           (ETuple
              [(EApp ((EVar (Id "f")), (EConst (CInt 1))));
                (EApp ((EVar (Id "f")), (EConst (CBool true))))])
           )))
        ]
      ))
    ]

  $ ./parser_tests.exe < manytests/typed/015tuples.ml
  [(SILet (Recursive,
      [((PVar (Id "fix")),
        (EFun ([(PVar (Id "f")); (PVar (Id "x"))],
           (EApp (
              (EApp ((EVar (Id "f")),
                 (EApp ((EVar (Id "fix")), (EVar (Id "f")))))),
              (EVar (Id "x"))))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "map")),
         (EFun ([(PVar (Id "f")); (PVar (Id "p"))],
            (ELet (Nonrecursive,
               ((PTuple [(PVar (Id "a")); (PVar (Id "b"))]), (EVar (Id "p"))),
               (ETuple
                  [(EApp ((EVar (Id "f")), (EVar (Id "a"))));
                    (EApp ((EVar (Id "f")), (EVar (Id "b"))))])
               ))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "fixpoly")),
         (EFun ([(PVar (Id "l"))],
            (EApp (
               (EApp ((EVar (Id "fix")),
                  (EFun ([(PVar (Id "self")); (PVar (Id "l"))],
                     (EApp (
                        (EApp ((EVar (Id "map")),
                           (EFun ([(PVar (Id "li")); (PVar (Id "x"))],
                              (EApp (
                                 (EApp ((EVar (Id "li")),
                                    (EApp ((EVar (Id "self")), (EVar (Id "l"))
                                       ))
                                    )),
                                 (EVar (Id "x"))))
                              ))
                           )),
                        (EVar (Id "l"))))
                     ))
                  )),
               (EVar (Id "l"))))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "feven")),
         (EFun ([(PVar (Id "p")); (PVar (Id "n"))],
            (ELet (Nonrecursive,
               ((PTuple [(PVar (Id "e")); (PVar (Id "o"))]), (EVar (Id "p"))),
               (EIf (
                  (EApp ((EApp ((EVar (Id "( = )")), (EVar (Id "n")))),
                     (EConst (CInt 0)))),
                  (EConst (CInt 1)),
                  (EApp ((EVar (Id "o")),
                     (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                        (EConst (CInt 1))))
                     ))
                  ))
               ))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "fodd")),
         (EFun ([(PVar (Id "p")); (PVar (Id "n"))],
            (ELet (Nonrecursive,
               ((PTuple [(PVar (Id "e")); (PVar (Id "o"))]), (EVar (Id "p"))),
               (EIf (
                  (EApp ((EApp ((EVar (Id "( = )")), (EVar (Id "n")))),
                     (EConst (CInt 0)))),
                  (EConst (CInt 0)),
                  (EApp ((EVar (Id "e")),
                     (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                        (EConst (CInt 1))))
                     ))
                  ))
               ))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "tie")),
         (EApp ((EVar (Id "fixpoly")),
            (ETuple [(EVar (Id "feven")); (EVar (Id "fodd"))]))))
         ]
       ));
    (SILet (Recursive,
       [((PVar (Id "meven")),
         (EFun ([(PVar (Id "n"))],
            (EIf (
               (EApp ((EApp ((EVar (Id "( = )")), (EVar (Id "n")))),
                  (EConst (CInt 0)))),
               (EConst (CInt 1)),
               (EApp ((EVar (Id "modd")),
                  (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                     (EConst (CInt 1))))
                  ))
               ))
            )));
         ((PVar (Id "modd")),
          (EFun ([(PVar (Id "n"))],
             (EIf (
                (EApp ((EApp ((EVar (Id "( = )")), (EVar (Id "n")))),
                   (EConst (CInt 0)))),
                (EConst (CInt 1)),
                (EApp ((EVar (Id "meven")),
                   (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                      (EConst (CInt 1))))
                   ))
                ))
             )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PConst CUnit),
             (EApp ((EVar (Id "print_int")),
                (EApp ((EVar (Id "modd")), (EConst (CInt 1))))))),
            (ELet (Nonrecursive,
               ((PConst CUnit),
                (EApp ((EVar (Id "print_int")),
                   (EApp ((EVar (Id "meven")), (EConst (CInt 2))))))),
               (ELet (Nonrecursive,
                  ((PTuple [(PVar (Id "even")); (PVar (Id "odd"))]),
                   (EVar (Id "tie"))),
                  (ELet (Nonrecursive,
                     ((PConst CUnit),
                      (EApp ((EVar (Id "print_int")),
                         (EApp ((EVar (Id "odd")), (EConst (CInt 3))))))),
                     (ELet (Nonrecursive,
                        ((PConst CUnit),
                         (EApp ((EVar (Id "print_int")),
                            (EApp ((EVar (Id "even")), (EConst (CInt 4))))))),
                        (EConst (CInt 0))))
                     ))
                  ))
               ))
            )))
         ]
       ))
    ]

  $ ./parser_tests.exe < manytests/typed/016lists.ml
  [(SILet (Recursive,
      [((PVar (Id "length")),
        (EFun ([(PVar (Id "xs"))],
           (EMatch ((EVar (Id "xs")),
              [((PConst CEmptyList), (EConst (CInt 0)));
                ((PCons ((PVar (Id "h")), (PVar (Id "tl")))),
                 (EApp ((EApp ((EVar (Id "( + )")), (EConst (CInt 1)))),
                    (EApp ((EVar (Id "length")), (EVar (Id "tl")))))))
                ]
              ))
           )))
        ]
      ));
    (SILet (Nonrecursive,
       [((PVar (Id "length_tail")),
         (ELet (Recursive,
            ((PVar (Id "helper")),
             (EFun ([(PVar (Id "acc")); (PVar (Id "xs"))],
                (EMatch ((EVar (Id "xs")),
                   [((PConst CEmptyList), (EVar (Id "acc")));
                     ((PCons ((PVar (Id "h")), (PVar (Id "tl")))),
                      (EApp (
                         (EApp ((EVar (Id "helper")),
                            (EApp (
                               (EApp ((EVar (Id "( + )")), (EVar (Id "acc")))),
                               (EConst (CInt 1))))
                            )),
                         (EVar (Id "tl")))))
                     ]
                   ))
                ))),
            (EApp ((EVar (Id "helper")), (EConst (CInt 0)))))))
         ]
       ));
    (SILet (Recursive,
       [((PVar (Id "map")),
         (EFun ([(PVar (Id "f")); (PVar (Id "xs"))],
            (EMatch ((EVar (Id "xs")),
               [((PConst CEmptyList), (EConst CEmptyList));
                 ((PCons ((PVar (Id "a")), (PConst CEmptyList))),
                  (ECons ((EApp ((EVar (Id "f")), (EVar (Id "a")))),
                     (EConst CEmptyList))));
                 ((PCons ((PVar (Id "a")),
                     (PCons ((PVar (Id "b")), (PConst CEmptyList))))),
                  (ECons ((EApp ((EVar (Id "f")), (EVar (Id "a")))),
                     (ECons ((EApp ((EVar (Id "f")), (EVar (Id "b")))),
                        (EConst CEmptyList)))
                     )));
                 ((PCons ((PVar (Id "a")),
                     (PCons ((PVar (Id "b")),
                        (PCons ((PVar (Id "c")), (PConst CEmptyList)))))
                     )),
                  (ECons ((EApp ((EVar (Id "f")), (EVar (Id "a")))),
                     (ECons ((EApp ((EVar (Id "f")), (EVar (Id "b")))),
                        (ECons ((EApp ((EVar (Id "f")), (EVar (Id "c")))),
                           (EConst CEmptyList)))
                        ))
                     )));
                 ((PCons ((PVar (Id "a")),
                     (PCons ((PVar (Id "b")),
                        (PCons ((PVar (Id "c")),
                           (PCons ((PVar (Id "d")), (PVar (Id "tl"))))))
                        ))
                     )),
                  (ECons ((EApp ((EVar (Id "f")), (EVar (Id "a")))),
                     (ECons ((EApp ((EVar (Id "f")), (EVar (Id "b")))),
                        (ECons ((EApp ((EVar (Id "f")), (EVar (Id "c")))),
                           (ECons ((EApp ((EVar (Id "f")), (EVar (Id "d")))),
                              (EApp (
                                 (EApp ((EVar (Id "map")), (EVar (Id "f")))),
                                 (EVar (Id "tl"))))
                              ))
                           ))
                        ))
                     )))
                 ]
               ))
            )))
         ]
       ));
    (SILet (Recursive,
       [((PVar (Id "append")),
         (EFun ([(PVar (Id "xs")); (PVar (Id "ys"))],
            (EMatch ((EVar (Id "xs")),
               [((PConst CEmptyList), (EVar (Id "ys")));
                 ((PCons ((PVar (Id "x")), (PVar (Id "xs")))),
                  (ECons ((EVar (Id "x")),
                     (EApp ((EApp ((EVar (Id "append")), (EVar (Id "xs")))),
                        (EVar (Id "ys"))))
                     )))
                 ]
               ))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "concat")),
         (ELet (Recursive,
            ((PVar (Id "helper")),
             (EFun ([(PVar (Id "xs"))],
                (EMatch ((EVar (Id "xs")),
                   [((PConst CEmptyList), (EConst CEmptyList));
                     ((PCons ((PVar (Id "h")), (PVar (Id "tl")))),
                      (EApp ((EApp ((EVar (Id "append")), (EVar (Id "h")))),
                         (EApp ((EVar (Id "helper")), (EVar (Id "tl")))))))
                     ]
                   ))
                ))),
            (EVar (Id "helper")))))
         ]
       ));
    (SILet (Recursive,
       [((PVar (Id "iter")),
         (EFun ([(PVar (Id "f")); (PVar (Id "xs"))],
            (EMatch ((EVar (Id "xs")),
               [((PConst CEmptyList), (EConst CUnit));
                 ((PCons ((PVar (Id "h")), (PVar (Id "tl")))),
                  (ELet (Nonrecursive,
                     ((PConst CUnit), (EApp ((EVar (Id "f")), (EVar (Id "h"))))),
                     (EApp ((EApp ((EVar (Id "iter")), (EVar (Id "f")))),
                        (EVar (Id "tl"))))
                     )))
                 ]
               ))
            )))
         ]
       ));
    (SILet (Recursive,
       [((PVar (Id "cartesian")),
         (EFun ([(PVar (Id "xs")); (PVar (Id "ys"))],
            (EMatch ((EVar (Id "xs")),
               [((PConst CEmptyList), (EConst CEmptyList));
                 ((PCons ((PVar (Id "h")), (PVar (Id "tl")))),
                  (EApp (
                     (EApp ((EVar (Id "append")),
                        (EApp (
                           (EApp ((EVar (Id "map")),
                              (EFun ([(PVar (Id "a"))],
                                 (ETuple [(EVar (Id "h")); (EVar (Id "a"))])))
                              )),
                           (EVar (Id "ys"))))
                        )),
                     (EApp ((EApp ((EVar (Id "cartesian")), (EVar (Id "tl")))),
                        (EVar (Id "ys"))))
                     )))
                 ]
               ))
            )))
         ]
       ));
    (SILet (Nonrecursive,
       [((PVar (Id "main")),
         (ELet (Nonrecursive,
            ((PConst CUnit),
             (EApp ((EApp ((EVar (Id "iter")), (EVar (Id "print_int")))),
                (ECons ((EConst (CInt 1)),
                   (ECons ((EConst (CInt 2)),
                      (ECons ((EConst (CInt 3)), (EConst CEmptyList)))))
                   ))
                ))),
            (ELet (Nonrecursive,
               ((PConst CUnit),
                (EApp ((EVar (Id "print_int")),
                   (EApp ((EVar (Id "length")),
                      (EApp (
                         (EApp ((EVar (Id "cartesian")),
                            (ECons ((EConst (CInt 1)),
                               (ECons ((EConst (CInt 2)), (EConst CEmptyList)))
                               ))
                            )),
                         (ECons ((EConst (CInt 1)),
                            (ECons ((EConst (CInt 2)),
                               (ECons ((EConst (CInt 3)),
                                  (ECons ((EConst (CInt 4)),
                                     (EConst CEmptyList)))
                                  ))
                               ))
                            ))
                         ))
                      ))
                   ))),
               (EConst (CInt 0))))
            )))
         ]
       ))
    ]


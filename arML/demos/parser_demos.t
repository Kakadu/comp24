MANYTESTS
  $ ./start_parser_demos.exe < manytests/do_not_type/001.ml
  [(DOrdinary (
      ((PVar (Id "recfac")),
       (EFun (((PVar (Id "n")), []),
          (EIfThenElse (
             (EApplication ((EIdentifier (Id "( <= )")),
                (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
             (EConstant (CInt 1)),
             (Some (EApplication ((EIdentifier (Id "( * )")),
                      (EIdentifier (Id "n")),
                      [(EApplication ((EIdentifier (Id "fac")),
                          (EApplication ((EIdentifier (Id "( - )")),
                             (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                          []))
                        ]
                      )))
             ))
          ))),
      []))
    ]

  $ ./start_parser_demos.exe < manytests/do_not_type/002if.ml
  [(DOrdinary (
      ((PVar (Id "main")),
       (EIfThenElse ((EConstant (CBool true)), (EConstant (CInt 1)),
          (Some (EConstant (CBool false)))))),
      []))
    ]

  $ ./start_parser_demos.exe < manytests/do_not_type/003occurs.ml
  [(DOrdinary (
      ((PVar (Id "fix")),
       (EFun (((PVar (Id "f")), []),
          (EApplication (
             (EFun (((PVar (Id "x")), []),
                (EApplication ((EIdentifier (Id "f")),
                   (EFun (((PVar (Id "f")), []),
                      (EApplication ((EIdentifier (Id "x")),
                         (EIdentifier (Id "x")), [(EIdentifier (Id "f"))]))
                      )),
                   []))
                )),
             (EFun (((PVar (Id "x")), []),
                (EApplication ((EIdentifier (Id "f")),
                   (EFun (((PVar (Id "f")), []),
                      (EApplication ((EIdentifier (Id "x")),
                         (EIdentifier (Id "x")), [(EIdentifier (Id "f"))]))
                      )),
                   []))
                )),
             []))
          ))),
      []))
    ]

  $ ./start_parser_demos.exe < manytests/do_not_type/004let_poly.ml
  [(DOrdinary (
      ((PVar (Id "temp")),
       (EApplication (
          (EFun (((PVar (Id "f")), []),
             (ETuple (
                (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 1)), 
                   [])),
                (EApplication ((EIdentifier (Id "f")),
                   (EConstant (CBool true)), [])),
                []))
             )),
          (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x")))), []))),
      []))
    ]

  $ ./start_parser_demos.exe < manytests/do_not_type/015tuples.ml
  [(DRecursive (
      ((PTuple ((PVar (Id "a")), (PVar (Id "b")), [])),
       (ETuple ((EIdentifier (Id "a")), (EIdentifier (Id "b")), []))),
      []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/001fac.ml
  [(DRecursive (
      ((PVar (Id "fac")),
       (EFun (((PVar (Id "n")), []),
          (EIfThenElse (
             (EApplication ((EIdentifier (Id "( <= )")),
                (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
             (EConstant (CInt 1)),
             (Some (EApplication ((EIdentifier (Id "( * )")),
                      (EIdentifier (Id "n")),
                      [(EApplication ((EIdentifier (Id "fac")),
                          (EApplication ((EIdentifier (Id "( - )")),
                             (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                          []))
                        ]
                      )))
             ))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PConst CUnit),
            (EApplication ((EIdentifier (Id "print_int")),
               (EApplication ((EIdentifier (Id "fac")), (EConstant (CInt 4)),
                  [])),
               []))),
           [], (EConstant (CInt 0))))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/002fac.ml
  [(DRecursive (
      ((PVar (Id "fac_cps")),
       (EFun (((PVar (Id "n")), [(PVar (Id "k"))]),
          (EIfThenElse (
             (EApplication ((EIdentifier (Id "( = )")), (EIdentifier (Id "n")),
                [(EConstant (CInt 1))])),
             (EApplication ((EIdentifier (Id "k")), (EConstant (CInt 1)), [])),
             (Some (EApplication ((EIdentifier (Id "fac_cps")),
                      (EApplication ((EIdentifier (Id "( - )")),
                         (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                      [(EFun (((PVar (Id "p")), []),
                          (EApplication ((EIdentifier (Id "k")),
                             (EApplication ((EIdentifier (Id "( * )")),
                                (EIdentifier (Id "p")),
                                [(EIdentifier (Id "n"))])),
                             []))
                          ))
                        ]
                      )))
             ))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PConst CUnit),
            (EApplication ((EIdentifier (Id "print_int")),
               (EApplication ((EIdentifier (Id "fac_cps")),
                  (EConstant (CInt 4)),
                  [(EFun (((PVar (Id "print_int")), []),
                      (EIdentifier (Id "print_int"))))
                    ]
                  )),
               []))),
           [], (EConstant (CInt 0))))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/003fib.ml
  [(DRecursive (
      ((PVar (Id "fib_acc")),
       (EFun (((PVar (Id "a")), [(PVar (Id "b")); (PVar (Id "n"))]),
          (EIfThenElse (
             (EApplication ((EIdentifier (Id "( = )")), (EIdentifier (Id "n")),
                [(EConstant (CInt 1))])),
             (EIdentifier (Id "b")),
             (Some (ELetIn (
                      ((PVar (Id "n1")),
                       (EApplication ((EIdentifier (Id "( - )")),
                          (EIdentifier (Id "n")), [(EConstant (CInt 1))]))),
                      [],
                      (ELetIn (
                         ((PVar (Id "ab")),
                          (EApplication ((EIdentifier (Id "( + )")),
                             (EIdentifier (Id "a")), [(EIdentifier (Id "b"))]))),
                         [],
                         (EApplication ((EIdentifier (Id "fib_acc")),
                            (EIdentifier (Id "b")),
                            [(EIdentifier (Id "ab")); (EIdentifier (Id "n1"))]
                            ))
                         ))
                      )))
             ))
          ))),
      []));
    (DRecursive (
       ((PVar (Id "fib")),
        (EFun (((PVar (Id "n")), []),
           (EIfThenElse (
              (EApplication ((EIdentifier (Id "( < )")),
                 (EIdentifier (Id "n")), [(EConstant (CInt 2))])),
              (EIdentifier (Id "n")),
              (Some (EApplication ((EIdentifier (Id "( + )")),
                       (EApplication ((EIdentifier (Id "fib")),
                          (EApplication ((EIdentifier (Id "( - )")),
                             (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                          [])),
                       [(EApplication ((EIdentifier (Id "fib")),
                           (EApplication ((EIdentifier (Id "( - )")),
                              (EIdentifier (Id "n")), [(EConstant (CInt 2))])),
                           []))
                         ]
                       )))
              ))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PConst CUnit),
            (EApplication ((EIdentifier (Id "print_int")),
               (EApplication ((EIdentifier (Id "fib_acc")),
                  (EConstant (CInt 0)),
                  [(EConstant (CInt 1)); (EConstant (CInt 4))])),
               []))),
           [],
           (ELetIn (
              ((PConst CUnit),
               (EApplication ((EIdentifier (Id "print_int")),
                  (EApplication ((EIdentifier (Id "fib")),
                     (EConstant (CInt 4)), [])),
                  []))),
              [], (EConstant (CInt 0))))
           ))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/004manyargs.ml
  [(DOrdinary (
      ((PVar (Id "wrap")),
       (EFun (((PVar (Id "f")), []),
          (EIfThenElse (
             (EApplication ((EIdentifier (Id "( = )")), (EConstant (CInt 1)),
                [(EConstant (CInt 1))])),
             (EIdentifier (Id "f")), (Some (EIdentifier (Id "f")))))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "test3")),
        (EFun (((PVar (Id "a")), [(PVar (Id "b")); (PVar (Id "c"))]),
           (ELetIn (
              ((PVar (Id "a")),
               (EApplication ((EIdentifier (Id "print_int")),
                  (EIdentifier (Id "a")), []))),
              [],
              (ELetIn (
                 ((PVar (Id "b")),
                  (EApplication ((EIdentifier (Id "print_int")),
                     (EIdentifier (Id "b")), []))),
                 [],
                 (ELetIn (
                    ((PVar (Id "c")),
                     (EApplication ((EIdentifier (Id "print_int")),
                        (EIdentifier (Id "c")), []))),
                    [], (EConstant (CInt 0))))
                 ))
              ))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "test10")),
        (EFun (
           ((PVar (Id "a")),
            [(PVar (Id "b")); (PVar (Id "c")); (PVar (Id "d"));
              (PVar (Id "e")); (PVar (Id "f")); (PVar (Id "g"));
              (PVar (Id "h")); (PVar (Id "i")); (PVar (Id "j"))]),
           (EApplication ((EIdentifier (Id "( + )")),
              (EApplication ((EIdentifier (Id "( + )")),
                 (EApplication ((EIdentifier (Id "( + )")),
                    (EApplication ((EIdentifier (Id "( + )")),
                       (EApplication ((EIdentifier (Id "( + )")),
                          (EApplication ((EIdentifier (Id "( + )")),
                             (EApplication ((EIdentifier (Id "( + )")),
                                (EApplication ((EIdentifier (Id "( + )")),
                                   (EApplication ((EIdentifier (Id "( + )")),
                                      (EIdentifier (Id "a")),
                                      [(EIdentifier (Id "b"))])),
                                   [(EIdentifier (Id "c"))])),
                                [(EIdentifier (Id "d"))])),
                             [(EIdentifier (Id "e"))])),
                          [(EIdentifier (Id "f"))])),
                       [(EIdentifier (Id "g"))])),
                    [(EIdentifier (Id "h"))])),
                 [(EIdentifier (Id "i"))])),
              [(EIdentifier (Id "j"))]))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PVar (Id "rez")),
            (EApplication ((EIdentifier (Id "wrap")),
               (EIdentifier (Id "test10")),
               [(EConstant (CInt 1)); (EConstant (CInt 10));
                 (EConstant (CInt 100)); (EConstant (CInt 1000));
                 (EConstant (CInt 10000)); (EConstant (CInt 100000));
                 (EConstant (CInt 1000000)); (EConstant (CInt 10000000));
                 (EConstant (CInt 100000000)); (EConstant (CInt 1000000000))]
               ))),
           [],
           (ELetIn (
              ((PConst CUnit),
               (EApplication ((EIdentifier (Id "print_int")),
                  (EIdentifier (Id "rez")), []))),
              [],
              (ELetIn (
                 ((PVar (Id "temp2")),
                  (EApplication ((EIdentifier (Id "wrap")),
                     (EIdentifier (Id "test3")),
                     [(EConstant (CInt 1)); (EConstant (CInt 10));
                       (EConstant (CInt 100))]
                     ))),
                 [], (EConstant (CInt 0))))
              ))
           ))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/005fix.ml
  [(DRecursive (
      ((PVar (Id "fix")),
       (EFun (((PVar (Id "f")), [(PVar (Id "x"))]),
          (EApplication ((EIdentifier (Id "f")),
             (EApplication ((EIdentifier (Id "fix")), (EIdentifier (Id "f")),
                [])),
             [(EIdentifier (Id "x"))]))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "fac")),
        (EFun (((PVar (Id "self")), [(PVar (Id "n"))]),
           (EIfThenElse (
              (EApplication ((EIdentifier (Id "( <= )")),
                 (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
              (EConstant (CInt 1)),
              (Some (EApplication ((EIdentifier (Id "( * )")),
                       (EIdentifier (Id "n")),
                       [(EApplication ((EIdentifier (Id "self")),
                           (EApplication ((EIdentifier (Id "( - )")),
                              (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                           []))
                         ]
                       )))
              ))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PConst CUnit),
            (EApplication ((EIdentifier (Id "print_int")),
               (EApplication ((EIdentifier (Id "fix")),
                  (EIdentifier (Id "fac")), [(EConstant (CInt 6))])),
               []))),
           [], (EConstant (CInt 0))))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/006partial.ml
  [(DOrdinary (
      ((PVar (Id "foo")),
       (EFun (((PVar (Id "b")), []),
          (EIfThenElse ((EIdentifier (Id "b")),
             (EFun (((PVar (Id "foo")), []),
                (EApplication ((EIdentifier (Id "( + )")),
                   (EIdentifier (Id "foo")), [(EConstant (CInt 2))]))
                )),
             (Some (EFun (((PVar (Id "foo")), []),
                      (EApplication ((EIdentifier (Id "( * )")),
                         (EIdentifier (Id "foo")), [(EConstant (CInt 10))]))
                      )))
             ))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "foo")),
        (EFun (((PVar (Id "x")), []),
           (EApplication ((EIdentifier (Id "foo")), (EConstant (CBool true)),
              [(EApplication ((EIdentifier (Id "foo")),
                  (EConstant (CBool false)),
                  [(EApplication ((EIdentifier (Id "foo")),
                      (EConstant (CBool true)),
                      [(EApplication ((EIdentifier (Id "foo")),
                          (EConstant (CBool false)), [(EIdentifier (Id "x"))]))
                        ]
                      ))
                    ]
                  ))
                ]
              ))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PConst CUnit),
            (EApplication ((EIdentifier (Id "print_int")),
               (EApplication ((EIdentifier (Id "foo")), (EConstant (CInt 11)),
                  [])),
               []))),
           [], (EConstant (CInt 0))))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/006partial2.ml
  [(DOrdinary (
      ((PVar (Id "foo")),
       (EFun (((PVar (Id "a")), [(PVar (Id "b")); (PVar (Id "c"))]),
          (ELetIn (
             ((PConst CUnit),
              (EApplication ((EIdentifier (Id "print_int")),
                 (EIdentifier (Id "a")), []))),
             [],
             (ELetIn (
                ((PConst CUnit),
                 (EApplication ((EIdentifier (Id "print_int")),
                    (EIdentifier (Id "b")), []))),
                [],
                (ELetIn (
                   ((PConst CUnit),
                    (EApplication ((EIdentifier (Id "print_int")),
                       (EIdentifier (Id "c")), []))),
                   [],
                   (EApplication ((EIdentifier (Id "( + )")),
                      (EIdentifier (Id "a")),
                      [(EApplication ((EIdentifier (Id "( * )")),
                          (EIdentifier (Id "b")), [(EIdentifier (Id "c"))]))
                        ]
                      ))
                   ))
                ))
             ))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PVar (Id "foo")),
            (EApplication ((EIdentifier (Id "foo")), (EConstant (CInt 1)), []))),
           [],
           (ELetIn (
              ((PVar (Id "foo")),
               (EApplication ((EIdentifier (Id "foo")), (EConstant (CInt 2)),
                  []))),
              [],
              (ELetIn (
                 ((PVar (Id "foo")),
                  (EApplication ((EIdentifier (Id "foo")),
                     (EConstant (CInt 3)), []))),
                 [],
                 (ELetIn (
                    ((PConst CUnit),
                     (EApplication ((EIdentifier (Id "print_int")),
                        (EIdentifier (Id "foo")), []))),
                    [], (EConstant (CInt 0))))
                 ))
              ))
           ))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/006partial3.ml
  [(DOrdinary (
      ((PVar (Id "foo")),
       (EFun (((PVar (Id "a")), []),
          (ELetIn (
             ((PConst CUnit),
              (EApplication ((EIdentifier (Id "print_int")),
                 (EIdentifier (Id "a")), []))),
             [],
             (EFun (((PVar (Id "b")), []),
                (ELetIn (
                   ((PConst CUnit),
                    (EApplication ((EIdentifier (Id "print_int")),
                       (EIdentifier (Id "b")), []))),
                   [],
                   (EFun (((PVar (Id "c")), []),
                      (EApplication ((EIdentifier (Id "print_int")),
                         (EIdentifier (Id "c")), []))
                      ))
                   ))
                ))
             ))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PConst CUnit),
            (EApplication ((EIdentifier (Id "foo")), (EConstant (CInt 4)),
               [(EConstant (CInt 8)); (EConstant (CInt 9))]))),
           [], (EConstant (CInt 0))))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/007order.ml
  [(DOrdinary (
      ((PVar (Id "_start")),
       (EFun (
          ((PConst CUnit),
           [(PConst CUnit); (PVar (Id "a")); (PConst CUnit); (PVar (Id "b"));
             (PVar (Id "_c")); (PConst CUnit); (PVar (Id "d"));
             (PVar (Id "__"))]),
          (ELetIn (
             ((PConst CUnit),
              (EApplication ((EIdentifier (Id "print_int")),
                 (EApplication ((EIdentifier (Id "( + )")),
                    (EIdentifier (Id "a")), [(EIdentifier (Id "b"))])),
                 []))),
             [],
             (ELetIn (
                ((PConst CUnit),
                 (EApplication ((EIdentifier (Id "print_int")),
                    (EIdentifier (Id "__")), []))),
                [],
                (EApplication ((EIdentifier (Id "( + )")),
                   (EApplication ((EIdentifier (Id "( / )")),
                      (EApplication ((EIdentifier (Id "( * )")),
                         (EIdentifier (Id "a")), [(EIdentifier (Id "b"))])),
                      [(EIdentifier (Id "_c"))])),
                   [(EIdentifier (Id "d"))]))
                ))
             ))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "main")),
        (EApplication ((EIdentifier (Id "print_int")),
           (EApplication ((EIdentifier (Id "_start")),
              (EApplication ((EIdentifier (Id "print_int")),
                 (EConstant (CInt 1)), [])),
              [(EApplication ((EIdentifier (Id "print_int")),
                  (EConstant (CInt 2)), []));
                (EConstant (CInt 3));
                (EApplication ((EIdentifier (Id "print_int")),
                   (EConstant (CInt 4)), []));
                (EConstant (CInt 100)); (EConstant (CInt 1000));
                (EApplication ((EIdentifier (Id "print_int")),
                   (EApplication ((EIdentifier (Id "U-")),
                      (EConstant (CInt 1)), [])),
                   []));
                (EConstant (CInt 10000));
                (EApplication ((EIdentifier (Id "U-")),
                   (EConstant (CInt 555555)), []))
                ]
              )),
           []))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/008ascription.ml
  [(DOrdinary (
      ((PVar (Id "addi")),
       (EFun (((PVar (Id "f")), [(PVar (Id "g")); (PVar (Id "x"))]),
          (ETyped (
             (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "x")),
                [(ETyped (
                    (EApplication ((EIdentifier (Id "g")),
                       (EIdentifier (Id "x")), [])),
                    (TDGround GTDBool)))
                  ]
                )),
             (TDGround GTDInt)))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PConst CUnit),
            (EApplication ((EIdentifier (Id "print_int")),
               (EApplication ((EIdentifier (Id "addi")),
                  (EFun (((PVar (Id "x")), [(PVar (Id "b"))]),
                     (EIfThenElse ((EIdentifier (Id "b")),
                        (EApplication ((EIdentifier (Id "( + )")),
                           (EIdentifier (Id "x")), [(EConstant (CInt 1))])),
                        (Some (EApplication ((EIdentifier (Id "( * )")),
                                 (EIdentifier (Id "x")), [(EConstant (CInt 2))]
                                 )))
                        ))
                     )),
                  [(EFun (((PVar (Id "_start")), []),
                      (EApplication ((EIdentifier (Id "( = )")),
                         (EApplication ((EIdentifier (Id "( / )")),
                            (EIdentifier (Id "_start")), [(EConstant (CInt 2))]
                            )),
                         [(EConstant (CInt 0))]))
                      ));
                    (EConstant (CInt 4))]
                  )),
               []))),
           [], (EConstant (CInt 0))))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/009let_poly.ml
  [(DOrdinary (
      ((PVar (Id "temp")),
       (ELetIn (
          ((PVar (Id "f")),
           (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
          [],
          (ETuple (
             (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 1)), [])),
             (EApplication ((EIdentifier (Id "f")), (EConstant (CBool true)),
                [])),
             []))
          ))),
      []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/015tuples.ml
  [(DRecursive (
      ((PVar (Id "fix")),
       (EFun (((PVar (Id "f")), [(PVar (Id "x"))]),
          (EApplication ((EIdentifier (Id "f")),
             (EApplication ((EIdentifier (Id "fix")), (EIdentifier (Id "f")),
                [])),
             [(EIdentifier (Id "x"))]))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "map")),
        (EFun (((PVar (Id "f")), [(PVar (Id "p"))]),
           (ELetIn (
              ((PTuple ((PVar (Id "a")), (PVar (Id "b")), [])),
               (EIdentifier (Id "p"))),
              [],
              (ETuple (
                 (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "a")),
                    [])),
                 (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "b")),
                    [])),
                 []))
              ))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "fixpoly")),
        (EFun (((PVar (Id "l")), []),
           (EApplication ((EIdentifier (Id "fix")),
              (EFun (((PVar (Id "self")), [(PVar (Id "l"))]),
                 (EApplication ((EIdentifier (Id "map")),
                    (EFun (((PVar (Id "li")), [(PVar (Id "x"))]),
                       (EApplication ((EIdentifier (Id "li")),
                          (EApplication ((EIdentifier (Id "self")),
                             (EIdentifier (Id "l")), [])),
                          [(EIdentifier (Id "x"))]))
                       )),
                    [(EIdentifier (Id "l"))]))
                 )),
              [(EIdentifier (Id "l"))]))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "feven")),
        (EFun (((PVar (Id "p")), [(PVar (Id "n"))]),
           (ELetIn (
              ((PTuple ((PVar (Id "e")), (PVar (Id "o")), [])),
               (EIdentifier (Id "p"))),
              [],
              (EIfThenElse (
                 (EApplication ((EIdentifier (Id "( = )")),
                    (EIdentifier (Id "n")), [(EConstant (CInt 0))])),
                 (EConstant (CInt 1)),
                 (Some (EApplication ((EIdentifier (Id "o")),
                          (EApplication ((EIdentifier (Id "( - )")),
                             (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                          [])))
                 ))
              ))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "fodd")),
        (EFun (((PVar (Id "p")), [(PVar (Id "n"))]),
           (ELetIn (
              ((PTuple ((PVar (Id "e")), (PVar (Id "o")), [])),
               (EIdentifier (Id "p"))),
              [],
              (EIfThenElse (
                 (EApplication ((EIdentifier (Id "( = )")),
                    (EIdentifier (Id "n")), [(EConstant (CInt 0))])),
                 (EConstant (CInt 0)),
                 (Some (EApplication ((EIdentifier (Id "e")),
                          (EApplication ((EIdentifier (Id "( - )")),
                             (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                          [])))
                 ))
              ))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "tie")),
        (EApplication ((EIdentifier (Id "fixpoly")),
           (ETuple ((EIdentifier (Id "feven")), (EIdentifier (Id "fodd")), [])),
           []))),
       []));
    (DRecursive (
       ((PVar (Id "meven")),
        (EFun (((PVar (Id "n")), []),
           (EIfThenElse (
              (EApplication ((EIdentifier (Id "( = )")),
                 (EIdentifier (Id "n")), [(EConstant (CInt 0))])),
              (EConstant (CInt 1)),
              (Some (EApplication ((EIdentifier (Id "modd")),
                       (EApplication ((EIdentifier (Id "( - )")),
                          (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                       [])))
              ))
           ))),
       [((PVar (Id "modd")),
         (EFun (((PVar (Id "n")), []),
            (EIfThenElse (
               (EApplication ((EIdentifier (Id "( = )")),
                  (EIdentifier (Id "n")), [(EConstant (CInt 0))])),
               (EConstant (CInt 1)),
               (Some (EApplication ((EIdentifier (Id "meven")),
                        (EApplication ((EIdentifier (Id "( - )")),
                           (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                        [])))
               ))
            )))
         ]
       ));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PConst CUnit),
            (EApplication ((EIdentifier (Id "print_int")),
               (EApplication ((EIdentifier (Id "modd")), (EConstant (CInt 1)),
                  [])),
               []))),
           [],
           (ELetIn (
              ((PConst CUnit),
               (EApplication ((EIdentifier (Id "print_int")),
                  (EApplication ((EIdentifier (Id "meven")),
                     (EConstant (CInt 2)), [])),
                  []))),
              [],
              (ELetIn (
                 ((PTuple ((PVar (Id "even")), (PVar (Id "odd")), [])),
                  (EIdentifier (Id "tie"))),
                 [],
                 (ELetIn (
                    ((PConst CUnit),
                     (EApplication ((EIdentifier (Id "print_int")),
                        (EApplication ((EIdentifier (Id "odd")),
                           (EConstant (CInt 3)), [])),
                        []))),
                    [],
                    (ELetIn (
                       ((PConst CUnit),
                        (EApplication ((EIdentifier (Id "print_int")),
                           (EApplication ((EIdentifier (Id "even")),
                              (EConstant (CInt 4)), [])),
                           []))),
                       [], (EConstant (CInt 0))))
                    ))
                 ))
              ))
           ))),
       []))
    ]

  $ ./start_parser_demos.exe < manytests/typed/016lists.ml
  [(DRecursive (
      ((PVar (Id "length")),
       (EFun (((PVar (Id "xs")), []),
          (EMatchWith ((EIdentifier (Id "xs")), (PNill, (EConstant (CInt 0))),
             [((PListConstructor ((PVar (Id "h")), (PVar (Id "tl")))),
               (EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 1)),
                  [(EApplication ((EIdentifier (Id "length")),
                      (EIdentifier (Id "tl")), []))
                    ]
                  )))
               ]
             ))
          ))),
      []));
    (DOrdinary (
       ((PVar (Id "length_tail")),
        (ERecLetIn (
           ((PVar (Id "helper")),
            (EFun (((PVar (Id "acc")), [(PVar (Id "xs"))]),
               (EMatchWith ((EIdentifier (Id "xs")),
                  (PNill, (EIdentifier (Id "acc"))),
                  [((PListConstructor ((PVar (Id "h")), (PVar (Id "tl")))),
                    (EApplication ((EIdentifier (Id "helper")),
                       (EApplication ((EIdentifier (Id "( + )")),
                          (EIdentifier (Id "acc")), [(EConstant (CInt 1))])),
                       [(EIdentifier (Id "tl"))])))
                    ]
                  ))
               ))),
           [],
           (EApplication ((EIdentifier (Id "helper")), (EConstant (CInt 0)), 
              []))
           ))),
       []));
    (DRecursive (
       ((PVar (Id "map")),
        (EFun (((PVar (Id "f")), [(PVar (Id "xs"))]),
           (EMatchWith ((EIdentifier (Id "xs")), (PNill, EEmptyList),
              [((PListConstructor ((PVar (Id "a")), PNill)),
                (EListConstructor (
                   (EApplication ((EIdentifier (Id "f")),
                      (EIdentifier (Id "a")), [])),
                   EEmptyList)));
                ((PListConstructor ((PVar (Id "a")),
                    (PListConstructor ((PVar (Id "b")), PNill)))),
                 (EListConstructor (
                    (EApplication ((EIdentifier (Id "f")),
                       (EIdentifier (Id "a")), [])),
                    (EListConstructor (
                       (EApplication ((EIdentifier (Id "f")),
                          (EIdentifier (Id "b")), [])),
                       EEmptyList))
                    )));
                ((PListConstructor ((PVar (Id "a")),
                    (PListConstructor ((PVar (Id "b")),
                       (PListConstructor ((PVar (Id "c")), PNill))))
                    )),
                 (EListConstructor (
                    (EApplication ((EIdentifier (Id "f")),
                       (EIdentifier (Id "a")), [])),
                    (EListConstructor (
                       (EApplication ((EIdentifier (Id "f")),
                          (EIdentifier (Id "b")), [])),
                       (EListConstructor (
                          (EApplication ((EIdentifier (Id "f")),
                             (EIdentifier (Id "c")), [])),
                          EEmptyList))
                       ))
                    )));
                ((PListConstructor ((PVar (Id "a")),
                    (PListConstructor ((PVar (Id "b")),
                       (PListConstructor ((PVar (Id "c")),
                          (PListConstructor ((PVar (Id "d")), (PVar (Id "tl"))
                             ))
                          ))
                       ))
                    )),
                 (EListConstructor (
                    (EApplication ((EIdentifier (Id "f")),
                       (EIdentifier (Id "a")), [])),
                    (EListConstructor (
                       (EApplication ((EIdentifier (Id "f")),
                          (EIdentifier (Id "b")), [])),
                       (EListConstructor (
                          (EApplication ((EIdentifier (Id "f")),
                             (EIdentifier (Id "c")), [])),
                          (EListConstructor (
                             (EApplication ((EIdentifier (Id "f")),
                                (EIdentifier (Id "d")), [])),
                             (EApplication ((EIdentifier (Id "map")),
                                (EIdentifier (Id "f")),
                                [(EIdentifier (Id "tl"))]))
                             ))
                          ))
                       ))
                    )))
                ]
              ))
           ))),
       []));
    (DRecursive (
       ((PVar (Id "append")),
        (EFun (((PVar (Id "xs")), [(PVar (Id "ys"))]),
           (EMatchWith ((EIdentifier (Id "xs")),
              (PNill, (EIdentifier (Id "ys"))),
              [((PListConstructor ((PVar (Id "x")), (PVar (Id "xs")))),
                (EListConstructor ((EIdentifier (Id "x")),
                   (EApplication ((EIdentifier (Id "append")),
                      (EIdentifier (Id "xs")), [(EIdentifier (Id "ys"))]))
                   )))
                ]
              ))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "concat")),
        (ERecLetIn (
           ((PVar (Id "helper")),
            (EFun (((PVar (Id "xs")), []),
               (EMatchWith ((EIdentifier (Id "xs")), (PNill, EEmptyList),
                  [((PListConstructor ((PVar (Id "h")), (PVar (Id "tl")))),
                    (EApplication ((EIdentifier (Id "append")),
                       (EIdentifier (Id "h")),
                       [(EApplication ((EIdentifier (Id "helper")),
                           (EIdentifier (Id "tl")), []))
                         ]
                       )))
                    ]
                  ))
               ))),
           [], (EIdentifier (Id "helper"))))),
       []));
    (DRecursive (
       ((PVar (Id "iter")),
        (EFun (((PVar (Id "f")), [(PVar (Id "xs"))]),
           (EMatchWith ((EIdentifier (Id "xs")), (PNill, (EConstant CUnit)),
              [((PListConstructor ((PVar (Id "h")), (PVar (Id "tl")))),
                (ELetIn (
                   ((PConst CUnit),
                    (EApplication ((EIdentifier (Id "f")),
                       (EIdentifier (Id "h")), []))),
                   [],
                   (EApplication ((EIdentifier (Id "iter")),
                      (EIdentifier (Id "f")), [(EIdentifier (Id "tl"))]))
                   )))
                ]
              ))
           ))),
       []));
    (DRecursive (
       ((PVar (Id "cartesian")),
        (EFun (((PVar (Id "xs")), [(PVar (Id "ys"))]),
           (EMatchWith ((EIdentifier (Id "xs")), (PNill, EEmptyList),
              [((PListConstructor ((PVar (Id "h")), (PVar (Id "tl")))),
                (EApplication ((EIdentifier (Id "append")),
                   (EApplication ((EIdentifier (Id "map")),
                      (EFun (((PVar (Id "a")), []),
                         (ETuple ((EIdentifier (Id "h")),
                            (EIdentifier (Id "a")), []))
                         )),
                      [(EIdentifier (Id "ys"))])),
                   [(EApplication ((EIdentifier (Id "cartesian")),
                       (EIdentifier (Id "tl")), [(EIdentifier (Id "ys"))]))
                     ]
                   )))
                ]
              ))
           ))),
       []));
    (DOrdinary (
       ((PVar (Id "main")),
        (ELetIn (
           ((PConst CUnit),
            (EApplication ((EIdentifier (Id "iter")),
               (EIdentifier (Id "print_int")),
               [(EListConstructor ((EConstant (CInt 1)),
                   (EListConstructor ((EConstant (CInt 2)),
                      (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
                   ))
                 ]
               ))),
           [],
           (ELetIn (
              ((PConst CUnit),
               (EApplication ((EIdentifier (Id "print_int")),
                  (EApplication ((EIdentifier (Id "length")),
                     (EApplication ((EIdentifier (Id "cartesian")),
                        (EListConstructor ((EConstant (CInt 1)),
                           (EListConstructor ((EConstant (CInt 2)), EEmptyList
                              ))
                           )),
                        [(EListConstructor ((EConstant (CInt 1)),
                            (EListConstructor ((EConstant (CInt 2)),
                               (EListConstructor ((EConstant (CInt 3)),
                                  (EListConstructor ((EConstant (CInt 4)),
                                     EEmptyList))
                                  ))
                               ))
                            ))
                          ]
                        )),
                     [])),
                  []))),
              [], (EConstant (CInt 0))))
           ))),
       []))
    ]

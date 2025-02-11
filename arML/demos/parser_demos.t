
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
                 (EApplication ((EIdentifier (Id "( == )")),
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
                 (EApplication ((EIdentifier (Id "( == )")),
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

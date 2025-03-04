  $ dune exec manytests_parser < manytests_link/do_not_type/001.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("recfac", [(Pat_var "n")],
          (Exp_if (
             (Exp_apply ((Exp_apply ((Exp_ident "( <= )"), (Exp_ident "n"))),
                (Exp_constant (Const_int 1)))),
             (Exp_constant (Const_int 1)),
             (Some (Exp_apply (
                      (Exp_apply ((Exp_ident "( * )"), (Exp_ident "n"))),
                      (Exp_apply ((Exp_ident "fac"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                            (Exp_constant (Const_int 1))))
                         ))
                      )))
             ))
          ))
        ]
      ))
    ]
  $ dune exec manytests_parser < manytests_link/do_not_type/002if.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("main", [],
          (Exp_if ((Exp_constant (Const_bool true)),
             (Exp_constant (Const_int 1)),
             (Some (Exp_constant (Const_bool false)))))
          ))
        ]
      ))
    ]
  $ dune exec manytests_parser < manytests_link/do_not_type/003occurs.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("fix", [(Pat_var "f")],
          (Exp_apply (
             (Exp_fun ([(Pat_var "x")],
                (Exp_apply ((Exp_ident "f"),
                   (Exp_fun ([(Pat_var "f")],
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "x"), (Exp_ident "x"))),
                         (Exp_ident "f")))
                      ))
                   ))
                )),
             (Exp_fun ([(Pat_var "x")],
                (Exp_apply ((Exp_ident "f"),
                   (Exp_fun ([(Pat_var "f")],
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "x"), (Exp_ident "x"))),
                         (Exp_ident "f")))
                      ))
                   ))
                ))
             ))
          ))
        ]
      ))
    ]
  $ dune exec manytests_parser < manytests_link/do_not_type/004let_poly.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("temp", [],
          (Exp_apply (
             (Exp_fun ([(Pat_var "f")],
                (Exp_tuple
                   [(Exp_apply ((Exp_ident "f"), (Exp_constant (Const_int 1))));
                     (Exp_apply ((Exp_ident "f"),
                        (Exp_constant (Const_bool true))))
                     ])
                )),
             (Exp_fun ([(Pat_var "x")], (Exp_ident "x")))))
          ))
        ]
      ))
    ]
  $ dune exec manytests_parser < manytests_link/do_not_type/015tuples.ml
  [(Str_value (Recursive,
      [(Pat_binding ((Pat_tuple [(Pat_var "a"); (Pat_var "b")]),
          (Exp_tuple [(Exp_ident "a"); (Exp_ident "b")])))
        ]
      ))
    ]
  $ dune exec manytests_parser < manytests_link/do_not_type/099.ml
  [(Str_value (Recursive,
      [(Pat_binding ((Pat_construct ("Some", (Some (Pat_var "x")))),
          (Exp_construct ("Some", (Some (Exp_constant (Const_int 1)))))))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Pat_binding ((Pat_construct ("Some", (Some (Pat_var "a")))),
           (Exp_ident "( < )")))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Pat_binding ((Pat_constant Const_unit),
           (Exp_fun ([(Pat_var "x")], (Exp_ident "x")))))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/001fac.ml
  [(Str_value (Recursive,
      [(Val_binding ("fac", [(Pat_var "n")],
          (Exp_if (
             (Exp_apply ((Exp_apply ((Exp_ident "( <= )"), (Exp_ident "n"))),
                (Exp_constant (Const_int 1)))),
             (Exp_constant (Const_int 1)),
             (Some (Exp_apply (
                      (Exp_apply ((Exp_ident "( * )"), (Exp_ident "n"))),
                      (Exp_apply ((Exp_ident "fac"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                            (Exp_constant (Const_int 1))))
                         ))
                      )))
             ))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_constant Const_unit),
                  (Exp_apply ((Exp_ident "print_int"),
                     (Exp_apply ((Exp_ident "fac"),
                        (Exp_constant (Const_int 4))))
                     ))
                  ))
                ],
              (Exp_constant (Const_int 0))))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/002fac.ml
  [(Str_value (Recursive,
      [(Val_binding ("fac_cps", [(Pat_var "n"); (Pat_var "k")],
          (Exp_if (
             (Exp_apply ((Exp_apply ((Exp_ident "( = )"), (Exp_ident "n"))),
                (Exp_constant (Const_int 1)))),
             (Exp_apply ((Exp_ident "k"), (Exp_constant (Const_int 1)))),
             (Some (Exp_apply (
                      (Exp_apply ((Exp_ident "fac_cps"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                            (Exp_constant (Const_int 1))))
                         )),
                      (Exp_fun ([(Pat_var "p")],
                         (Exp_apply ((Exp_ident "k"),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "( * )"), (Exp_ident "p")
                                  )),
                               (Exp_ident "n")))
                            ))
                         ))
                      )))
             ))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_constant Const_unit),
                  (Exp_apply ((Exp_ident "print_int"),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "fac_cps"),
                           (Exp_constant (Const_int 4)))),
                        (Exp_fun ([(Pat_var "print_int")],
                           (Exp_ident "print_int")))
                        ))
                     ))
                  ))
                ],
              (Exp_constant (Const_int 0))))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/003fib.ml
  [(Str_value (Recursive,
      [(Val_binding ("fib_acc", [(Pat_var "a"); (Pat_var "b"); (Pat_var "n")],
          (Exp_if (
             (Exp_apply ((Exp_apply ((Exp_ident "( = )"), (Exp_ident "n"))),
                (Exp_constant (Const_int 1)))),
             (Exp_ident "b"),
             (Some (Exp_let (Nonrecursive,
                      [(Val_binding ("n1", [],
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                             (Exp_constant (Const_int 1))))
                          ))
                        ],
                      (Exp_let (Nonrecursive,
                         [(Val_binding ("ab", [],
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "( + )"),
                                   (Exp_ident "a"))),
                                (Exp_ident "b")))
                             ))
                           ],
                         (Exp_apply (
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "fib_acc"),
                                  (Exp_ident "b"))),
                               (Exp_ident "ab"))),
                            (Exp_ident "n1")))
                         ))
                      )))
             ))
          ))
        ]
      ));
    (Str_value (Recursive,
       [(Val_binding ("fib", [(Pat_var "n")],
           (Exp_if (
              (Exp_apply ((Exp_apply ((Exp_ident "( < )"), (Exp_ident "n"))),
                 (Exp_constant (Const_int 2)))),
              (Exp_ident "n"),
              (Some (Exp_apply (
                       (Exp_apply ((Exp_ident "( + )"),
                          (Exp_apply ((Exp_ident "fib"),
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "( - )"),
                                   (Exp_ident "n"))),
                                (Exp_constant (Const_int 1))))
                             ))
                          )),
                       (Exp_apply ((Exp_ident "fib"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                             (Exp_constant (Const_int 2))))
                          ))
                       )))
              ))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_constant Const_unit),
                  (Exp_apply ((Exp_ident "print_int"),
                     (Exp_apply (
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "fib_acc"),
                              (Exp_constant (Const_int 0)))),
                           (Exp_constant (Const_int 1)))),
                        (Exp_constant (Const_int 4))))
                     ))
                  ))
                ],
              (Exp_let (Nonrecursive,
                 [(Pat_binding ((Pat_constant Const_unit),
                     (Exp_apply ((Exp_ident "print_int"),
                        (Exp_apply ((Exp_ident "fib"),
                           (Exp_constant (Const_int 4))))
                        ))
                     ))
                   ],
                 (Exp_constant (Const_int 0))))
              ))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/004manyargs.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("wrap", [(Pat_var "f")],
          (Exp_if (
             (Exp_apply (
                (Exp_apply ((Exp_ident "( = )"), (Exp_constant (Const_int 1)))),
                (Exp_constant (Const_int 1)))),
             (Exp_ident "f"), (Some (Exp_ident "f"))))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("test3", [(Pat_var "a"); (Pat_var "b"); (Pat_var "c")],
           (Exp_let (Nonrecursive,
              [(Val_binding ("a", [],
                  (Exp_apply ((Exp_ident "print_int"), (Exp_ident "a")))))
                ],
              (Exp_let (Nonrecursive,
                 [(Val_binding ("b", [],
                     (Exp_apply ((Exp_ident "print_int"), (Exp_ident "b")))))
                   ],
                 (Exp_let (Nonrecursive,
                    [(Val_binding ("c", [],
                        (Exp_apply ((Exp_ident "print_int"), (Exp_ident "c")))
                        ))
                      ],
                    (Exp_constant (Const_int 0))))
                 ))
              ))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("test10",
           [(Pat_var "a"); (Pat_var "b"); (Pat_var "c"); (Pat_var "d");
             (Pat_var "e"); (Pat_var "f"); (Pat_var "g"); (Pat_var "h");
             (Pat_var "i"); (Pat_var "j")],
           (Exp_apply (
              (Exp_apply ((Exp_ident "( + )"),
                 (Exp_apply (
                    (Exp_apply ((Exp_ident "( + )"),
                       (Exp_apply (
                          (Exp_apply ((Exp_ident "( + )"),
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "( + )"),
                                   (Exp_apply (
                                      (Exp_apply ((Exp_ident "( + )"),
                                         (Exp_apply (
                                            (Exp_apply ((Exp_ident "( + )"),
                                               (Exp_apply (
                                                  (Exp_apply (
                                                     (Exp_ident "( + )"),
                                                     (Exp_apply (
                                                        (Exp_apply (
                                                           (Exp_ident "( + )"),
                                                           (Exp_apply (
                                                              (Exp_apply (
                                                                 (Exp_ident
                                                                    "( + )"),
                                                                 (Exp_ident "a")
                                                                 )),
                                                              (Exp_ident "b")))
                                                           )),
                                                        (Exp_ident "c")))
                                                     )),
                                                  (Exp_ident "d")))
                                               )),
                                            (Exp_ident "e")))
                                         )),
                                      (Exp_ident "f")))
                                   )),
                                (Exp_ident "g")))
                             )),
                          (Exp_ident "h")))
                       )),
                    (Exp_ident "i")))
                 )),
              (Exp_ident "j")))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Val_binding ("rez", [],
                  (Exp_apply (
                     (Exp_apply (
                        (Exp_apply (
                           (Exp_apply (
                              (Exp_apply (
                                 (Exp_apply (
                                    (Exp_apply (
                                       (Exp_apply (
                                          (Exp_apply (
                                             (Exp_apply (
                                                (Exp_apply ((Exp_ident "wrap"),
                                                   (Exp_ident "test10"))),
                                                (Exp_constant (Const_int 1)))),
                                             (Exp_constant (Const_int 10)))),
                                          (Exp_constant (Const_int 100)))),
                                       (Exp_constant (Const_int 1000)))),
                                    (Exp_constant (Const_int 10000)))),
                                 (Exp_constant (Const_int 100000)))),
                              (Exp_constant (Const_int 1000000)))),
                           (Exp_constant (Const_int 10000000)))),
                        (Exp_constant (Const_int 100000000)))),
                     (Exp_constant (Const_int 1000000000))))
                  ))
                ],
              (Exp_let (Nonrecursive,
                 [(Pat_binding ((Pat_constant Const_unit),
                     (Exp_apply ((Exp_ident "print_int"), (Exp_ident "rez")))))
                   ],
                 (Exp_let (Nonrecursive,
                    [(Val_binding ("temp2", [],
                        (Exp_apply (
                           (Exp_apply (
                              (Exp_apply (
                                 (Exp_apply ((Exp_ident "wrap"),
                                    (Exp_ident "test3"))),
                                 (Exp_constant (Const_int 1)))),
                              (Exp_constant (Const_int 10)))),
                           (Exp_constant (Const_int 100))))
                        ))
                      ],
                    (Exp_constant (Const_int 0))))
                 ))
              ))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/005fix.ml
  [(Str_value (Recursive,
      [(Val_binding ("fix", [(Pat_var "f"); (Pat_var "x")],
          (Exp_apply (
             (Exp_apply ((Exp_ident "f"),
                (Exp_apply ((Exp_ident "fix"), (Exp_ident "f"))))),
             (Exp_ident "x")))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("fac", [(Pat_var "self"); (Pat_var "n")],
           (Exp_if (
              (Exp_apply ((Exp_apply ((Exp_ident "( <= )"), (Exp_ident "n"))),
                 (Exp_constant (Const_int 1)))),
              (Exp_constant (Const_int 1)),
              (Some (Exp_apply (
                       (Exp_apply ((Exp_ident "( * )"), (Exp_ident "n"))),
                       (Exp_apply ((Exp_ident "self"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                             (Exp_constant (Const_int 1))))
                          ))
                       )))
              ))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_constant Const_unit),
                  (Exp_apply ((Exp_ident "print_int"),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "fix"), (Exp_ident "fac"))),
                        (Exp_constant (Const_int 6))))
                     ))
                  ))
                ],
              (Exp_constant (Const_int 0))))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/006partial.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("foo", [(Pat_var "b")],
          (Exp_if ((Exp_ident "b"),
             (Exp_fun ([(Pat_var "foo")],
                (Exp_apply (
                   (Exp_apply ((Exp_ident "( + )"), (Exp_ident "foo"))),
                   (Exp_constant (Const_int 2))))
                )),
             (Some (Exp_fun ([(Pat_var "foo")],
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "( * )"), (Exp_ident "foo"))),
                         (Exp_constant (Const_int 10))))
                      )))
             ))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("foo", [(Pat_var "x")],
           (Exp_apply (
              (Exp_apply ((Exp_ident "foo"), (Exp_constant (Const_bool true)))),
              (Exp_apply (
                 (Exp_apply ((Exp_ident "foo"),
                    (Exp_constant (Const_bool false)))),
                 (Exp_apply (
                    (Exp_apply ((Exp_ident "foo"),
                       (Exp_constant (Const_bool true)))),
                    (Exp_apply (
                       (Exp_apply ((Exp_ident "foo"),
                          (Exp_constant (Const_bool false)))),
                       (Exp_ident "x")))
                    ))
                 ))
              ))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_constant Const_unit),
                  (Exp_apply ((Exp_ident "print_int"),
                     (Exp_apply ((Exp_ident "foo"),
                        (Exp_constant (Const_int 11))))
                     ))
                  ))
                ],
              (Exp_constant (Const_int 0))))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/006partial2.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("foo", [(Pat_var "a"); (Pat_var "b"); (Pat_var "c")],
          (Exp_let (Nonrecursive,
             [(Pat_binding ((Pat_constant Const_unit),
                 (Exp_apply ((Exp_ident "print_int"), (Exp_ident "a")))))
               ],
             (Exp_let (Nonrecursive,
                [(Pat_binding ((Pat_constant Const_unit),
                    (Exp_apply ((Exp_ident "print_int"), (Exp_ident "b")))))
                  ],
                (Exp_let (Nonrecursive,
                   [(Pat_binding ((Pat_constant Const_unit),
                       (Exp_apply ((Exp_ident "print_int"), (Exp_ident "c")))))
                     ],
                   (Exp_apply (
                      (Exp_apply ((Exp_ident "( + )"), (Exp_ident "a"))),
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "( * )"), (Exp_ident "b"))),
                         (Exp_ident "c")))
                      ))
                   ))
                ))
             ))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Val_binding ("foo", [],
                  (Exp_apply ((Exp_ident "foo"), (Exp_constant (Const_int 1))))
                  ))
                ],
              (Exp_let (Nonrecursive,
                 [(Val_binding ("foo", [],
                     (Exp_apply ((Exp_ident "foo"),
                        (Exp_constant (Const_int 2))))
                     ))
                   ],
                 (Exp_let (Nonrecursive,
                    [(Val_binding ("foo", [],
                        (Exp_apply ((Exp_ident "foo"),
                           (Exp_constant (Const_int 3))))
                        ))
                      ],
                    (Exp_let (Nonrecursive,
                       [(Pat_binding ((Pat_constant Const_unit),
                           (Exp_apply ((Exp_ident "print_int"),
                              (Exp_ident "foo")))
                           ))
                         ],
                       (Exp_constant (Const_int 0))))
                    ))
                 ))
              ))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/006partial3.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("foo", [(Pat_var "a")],
          (Exp_let (Nonrecursive,
             [(Pat_binding ((Pat_constant Const_unit),
                 (Exp_apply ((Exp_ident "print_int"), (Exp_ident "a")))))
               ],
             (Exp_fun ([(Pat_var "b")],
                (Exp_let (Nonrecursive,
                   [(Pat_binding ((Pat_constant Const_unit),
                       (Exp_apply ((Exp_ident "print_int"), (Exp_ident "b")))))
                     ],
                   (Exp_fun ([(Pat_var "c")],
                      (Exp_apply ((Exp_ident "print_int"), (Exp_ident "c")))))
                   ))
                ))
             ))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_constant Const_unit),
                  (Exp_apply (
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "foo"),
                           (Exp_constant (Const_int 4)))),
                        (Exp_constant (Const_int 8)))),
                     (Exp_constant (Const_int 9))))
                  ))
                ],
              (Exp_constant (Const_int 0))))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/007order.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("_start",
          [(Pat_constant Const_unit); (Pat_constant Const_unit); (Pat_var "a");
            (Pat_constant Const_unit); (Pat_var "b"); (Pat_var "_c");
            (Pat_constant Const_unit); (Pat_var "d"); (Pat_var "__")],
          (Exp_let (Nonrecursive,
             [(Pat_binding ((Pat_constant Const_unit),
                 (Exp_apply ((Exp_ident "print_int"),
                    (Exp_apply (
                       (Exp_apply ((Exp_ident "( + )"), (Exp_ident "a"))),
                       (Exp_ident "b")))
                    ))
                 ))
               ],
             (Exp_let (Nonrecursive,
                [(Pat_binding ((Pat_constant Const_unit),
                    (Exp_apply ((Exp_ident "print_int"), (Exp_ident "__")))))
                  ],
                (Exp_apply (
                   (Exp_apply ((Exp_ident "( + )"),
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "( / )"),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "( * )"), (Exp_ident "a")
                                  )),
                               (Exp_ident "b")))
                            )),
                         (Exp_ident "_c")))
                      )),
                   (Exp_ident "d")))
                ))
             ))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_apply ((Exp_ident "print_int"),
              (Exp_apply (
                 (Exp_apply (
                    (Exp_apply (
                       (Exp_apply (
                          (Exp_apply (
                             (Exp_apply (
                                (Exp_apply (
                                   (Exp_apply (
                                      (Exp_apply ((Exp_ident "_start"),
                                         (Exp_apply ((Exp_ident "print_int"),
                                            (Exp_constant (Const_int 1))))
                                         )),
                                      (Exp_apply ((Exp_ident "print_int"),
                                         (Exp_constant (Const_int 2))))
                                      )),
                                   (Exp_constant (Const_int 3)))),
                                (Exp_apply ((Exp_ident "print_int"),
                                   (Exp_constant (Const_int 4))))
                                )),
                             (Exp_constant (Const_int 100)))),
                          (Exp_constant (Const_int 1000)))),
                       (Exp_apply ((Exp_ident "print_int"),
                          (Exp_apply ((Exp_ident "( ~- )"),
                             (Exp_constant (Const_int 1))))
                          ))
                       )),
                    (Exp_constant (Const_int 10000)))),
                 (Exp_apply ((Exp_ident "( ~- )"),
                    (Exp_constant (Const_int 555555))))
                 ))
              ))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/008ascription.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("addi", [],
          (Exp_fun ([(Pat_var "f"); (Pat_var "g"); (Pat_var "x")],
             (Exp_type (
                (Exp_apply ((Exp_apply ((Exp_ident "f"), (Exp_ident "x"))),
                   (Exp_type ((Exp_apply ((Exp_ident "g"), (Exp_ident "x"))),
                      (Type_single "bool")))
                   )),
                (Type_single "int")))
             ))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_constant Const_unit),
                  (Exp_apply ((Exp_ident "print_int"),
                     (Exp_apply (
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "addi"),
                              (Exp_fun ([(Pat_var "x"); (Pat_var "b")],
                                 (Exp_if ((Exp_ident "b"),
                                    (Exp_apply (
                                       (Exp_apply ((Exp_ident "( + )"),
                                          (Exp_ident "x"))),
                                       (Exp_constant (Const_int 1)))),
                                    (Some (Exp_apply (
                                             (Exp_apply ((Exp_ident "( * )"),
                                                (Exp_ident "x"))),
                                             (Exp_constant (Const_int 2)))))
                                    ))
                                 ))
                              )),
                           (Exp_fun ([(Pat_var "_start")],
                              (Exp_apply (
                                 (Exp_apply ((Exp_ident "( = )"),
                                    (Exp_apply (
                                       (Exp_apply ((Exp_ident "( / )"),
                                          (Exp_ident "_start"))),
                                       (Exp_constant (Const_int 2))))
                                    )),
                                 (Exp_constant (Const_int 0))))
                              ))
                           )),
                        (Exp_constant (Const_int 4))))
                     ))
                  ))
                ],
              (Exp_constant (Const_int 0))))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/009let_poly.ml
  [(Str_value (Nonrecursive,
      [(Val_binding ("temp", [],
          (Exp_let (Nonrecursive,
             [(Val_binding ("f", [],
                 (Exp_fun ([(Pat_var "x")], (Exp_ident "x")))))
               ],
             (Exp_tuple
                [(Exp_apply ((Exp_ident "f"), (Exp_constant (Const_int 1))));
                  (Exp_apply ((Exp_ident "f"), (Exp_constant (Const_bool true))
                     ))
                  ])
             ))
          ))
        ]
      ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/010sukharev.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/015tuples.ml
  [(Str_value (Recursive,
      [(Val_binding ("fix", [(Pat_var "f"); (Pat_var "x")],
          (Exp_apply (
             (Exp_apply ((Exp_ident "f"),
                (Exp_apply ((Exp_ident "fix"), (Exp_ident "f"))))),
             (Exp_ident "x")))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("map", [(Pat_var "f"); (Pat_var "p")],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_tuple [(Pat_var "a"); (Pat_var "b")]),
                  (Exp_ident "p")))
                ],
              (Exp_tuple
                 [(Exp_apply ((Exp_ident "f"), (Exp_ident "a")));
                   (Exp_apply ((Exp_ident "f"), (Exp_ident "b")))])
              ))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("fixpoly", [(Pat_var "l")],
           (Exp_apply (
              (Exp_apply ((Exp_ident "fix"),
                 (Exp_fun ([(Pat_var "self"); (Pat_var "l")],
                    (Exp_apply (
                       (Exp_apply ((Exp_ident "map"),
                          (Exp_fun ([(Pat_var "li"); (Pat_var "x")],
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "li"),
                                   (Exp_apply ((Exp_ident "self"),
                                      (Exp_ident "l")))
                                   )),
                                (Exp_ident "x")))
                             ))
                          )),
                       (Exp_ident "l")))
                    ))
                 )),
              (Exp_ident "l")))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("feven", [(Pat_var "p"); (Pat_var "n")],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_tuple [(Pat_var "e"); (Pat_var "o")]),
                  (Exp_ident "p")))
                ],
              (Exp_if (
                 (Exp_apply (
                    (Exp_apply ((Exp_ident "( == )"), (Exp_ident "n"))),
                    (Exp_constant (Const_int 0)))),
                 (Exp_constant (Const_int 1)),
                 (Some (Exp_apply ((Exp_ident "o"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                             (Exp_constant (Const_int 1))))
                          )))
                 ))
              ))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("fodd", [(Pat_var "p"); (Pat_var "n")],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_tuple [(Pat_var "e"); (Pat_var "o")]),
                  (Exp_ident "p")))
                ],
              (Exp_if (
                 (Exp_apply (
                    (Exp_apply ((Exp_ident "( == )"), (Exp_ident "n"))),
                    (Exp_constant (Const_int 0)))),
                 (Exp_constant (Const_int 0)),
                 (Some (Exp_apply ((Exp_ident "e"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                             (Exp_constant (Const_int 1))))
                          )))
                 ))
              ))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("tie", [],
           (Exp_apply ((Exp_ident "fixpoly"),
              (Exp_tuple [(Exp_ident "feven"); (Exp_ident "fodd")])))
           ))
         ]
       ));
    (Str_value (Recursive,
       [(Val_binding ("meven", [(Pat_var "n")],
           (Exp_if (
              (Exp_apply ((Exp_apply ((Exp_ident "( = )"), (Exp_ident "n"))),
                 (Exp_constant (Const_int 0)))),
              (Exp_constant (Const_int 1)),
              (Some (Exp_apply ((Exp_ident "modd"),
                       (Exp_apply (
                          (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                          (Exp_constant (Const_int 1))))
                       )))
              ))
           ));
         (Val_binding ("modd", [(Pat_var "n")],
            (Exp_if (
               (Exp_apply ((Exp_apply ((Exp_ident "( = )"), (Exp_ident "n"))),
                  (Exp_constant (Const_int 0)))),
               (Exp_constant (Const_int 1)),
               (Some (Exp_apply ((Exp_ident "meven"),
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "( - )"), (Exp_ident "n"))),
                           (Exp_constant (Const_int 1))))
                        )))
               ))
            ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_constant Const_unit),
                  (Exp_apply ((Exp_ident "print_int"),
                     (Exp_apply ((Exp_ident "modd"),
                        (Exp_constant (Const_int 1))))
                     ))
                  ))
                ],
              (Exp_let (Nonrecursive,
                 [(Pat_binding ((Pat_constant Const_unit),
                     (Exp_apply ((Exp_ident "print_int"),
                        (Exp_apply ((Exp_ident "meven"),
                           (Exp_constant (Const_int 2))))
                        ))
                     ))
                   ],
                 (Exp_let (Nonrecursive,
                    [(Pat_binding (
                        (Pat_tuple [(Pat_var "even"); (Pat_var "odd")]),
                        (Exp_ident "tie")))
                      ],
                    (Exp_let (Nonrecursive,
                       [(Pat_binding ((Pat_constant Const_unit),
                           (Exp_apply ((Exp_ident "print_int"),
                              (Exp_apply ((Exp_ident "odd"),
                                 (Exp_constant (Const_int 3))))
                              ))
                           ))
                         ],
                       (Exp_let (Nonrecursive,
                          [(Pat_binding ((Pat_constant Const_unit),
                              (Exp_apply ((Exp_ident "print_int"),
                                 (Exp_apply ((Exp_ident "even"),
                                    (Exp_constant (Const_int 4))))
                                 ))
                              ))
                            ],
                          (Exp_constant (Const_int 0))))
                       ))
                    ))
                 ))
              ))
           ))
         ]
       ))
    ]
  $ dune exec manytests_parser < manytests_link/typed/016lists.ml
  [(Str_value (Recursive,
      [(Val_binding ("length", [(Pat_var "xs")],
          (Exp_match ((Exp_ident "xs"),
             [{ left = (Pat_construct ("[]", None));
                right = (Exp_constant (Const_int 0)) };
               { left =
                 (Pat_construct ("::",
                    (Some (Pat_tuple [(Pat_var "h"); (Pat_var "tl")]))));
                 right =
                 (Exp_apply (
                    (Exp_apply ((Exp_ident "( + )"),
                       (Exp_constant (Const_int 1)))),
                    (Exp_apply ((Exp_ident "length"), (Exp_ident "tl")))))
                 }
               ]
             ))
          ))
        ]
      ));
    (Str_value (Nonrecursive,
       [(Val_binding ("length_tail", [],
           (Exp_let (Recursive,
              [(Val_binding ("helper", [(Pat_var "acc"); (Pat_var "xs")],
                  (Exp_match ((Exp_ident "xs"),
                     [{ left = (Pat_construct ("[]", None));
                        right = (Exp_ident "acc") };
                       { left =
                         (Pat_construct ("::",
                            (Some (Pat_tuple [(Pat_var "h"); (Pat_var "tl")]))
                            ));
                         right =
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "helper"),
                               (Exp_apply (
                                  (Exp_apply ((Exp_ident "( + )"),
                                     (Exp_ident "acc"))),
                                  (Exp_constant (Const_int 1))))
                               )),
                            (Exp_ident "tl")))
                         }
                       ]
                     ))
                  ))
                ],
              (Exp_apply ((Exp_ident "helper"), (Exp_constant (Const_int 0))))
              ))
           ))
         ]
       ));
    (Str_value (Recursive,
       [(Val_binding ("map", [(Pat_var "f"); (Pat_var "xs")],
           (Exp_match ((Exp_ident "xs"),
              [{ left = (Pat_construct ("[]", None));
                 right = (Exp_construct ("[]", None)) };
                { left =
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_var "a"); (Pat_construct ("[]", None))]))
                     ));
                  right =
                  (Exp_construct ("::",
                     (Some (Exp_tuple
                              [(Exp_apply ((Exp_ident "f"), (Exp_ident "a")));
                                (Exp_construct ("[]", None))]))
                     ))
                  };
                { left =
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_var "a");
                                (Pat_construct ("::",
                                   (Some (Pat_tuple
                                            [(Pat_var "b");
                                              (Pat_construct ("[]", None))]))
                                   ))
                                ]))
                     ));
                  right =
                  (Exp_construct ("::",
                     (Some (Exp_tuple
                              [(Exp_apply ((Exp_ident "f"), (Exp_ident "a")));
                                (Exp_construct ("::",
                                   (Some (Exp_tuple
                                            [(Exp_apply ((Exp_ident "f"),
                                                (Exp_ident "b")));
                                              (Exp_construct ("[]", None))]))
                                   ))
                                ]))
                     ))
                  };
                { left =
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_var "a");
                                (Pat_construct ("::",
                                   (Some (Pat_tuple
                                            [(Pat_var "b");
                                              (Pat_construct ("::",
                                                 (Some (Pat_tuple
                                                          [(Pat_var "c");
                                                            (Pat_construct (
                                                               "[]", None))
                                                            ]))
                                                 ))
                                              ]))
                                   ))
                                ]))
                     ));
                  right =
                  (Exp_construct ("::",
                     (Some (Exp_tuple
                              [(Exp_apply ((Exp_ident "f"), (Exp_ident "a")));
                                (Exp_construct ("::",
                                   (Some (Exp_tuple
                                            [(Exp_apply ((Exp_ident "f"),
                                                (Exp_ident "b")));
                                              (Exp_construct ("::",
                                                 (Some (Exp_tuple
                                                          [(Exp_apply (
                                                              (Exp_ident "f"),
                                                              (Exp_ident "c")));
                                                            (Exp_construct (
                                                               "[]", None))
                                                            ]))
                                                 ))
                                              ]))
                                   ))
                                ]))
                     ))
                  };
                { left =
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_var "a");
                                (Pat_construct ("::",
                                   (Some (Pat_tuple
                                            [(Pat_var "b");
                                              (Pat_construct ("::",
                                                 (Some (Pat_tuple
                                                          [(Pat_var "c");
                                                            (Pat_construct (
                                                               "::",
                                                               (Some (Pat_tuple
                                                                      [(Pat_var
                                                                      "d");
                                                                      (Pat_var
                                                                      "tl")]))
                                                               ))
                                                            ]))
                                                 ))
                                              ]))
                                   ))
                                ]))
                     ));
                  right =
                  (Exp_construct ("::",
                     (Some (Exp_tuple
                              [(Exp_apply ((Exp_ident "f"), (Exp_ident "a")));
                                (Exp_construct ("::",
                                   (Some (Exp_tuple
                                            [(Exp_apply ((Exp_ident "f"),
                                                (Exp_ident "b")));
                                              (Exp_construct ("::",
                                                 (Some (Exp_tuple
                                                          [(Exp_apply (
                                                              (Exp_ident "f"),
                                                              (Exp_ident "c")));
                                                            (Exp_construct (
                                                               "::",
                                                               (Some (Exp_tuple
                                                                      [(Exp_apply (
                                                                      (Exp_ident
                                                                      "f"),
                                                                      (Exp_ident
                                                                      "d")));
                                                                      (Exp_apply (
                                                                      (Exp_apply (
                                                                      (Exp_ident
                                                                      "map"),
                                                                      (Exp_ident
                                                                      "f"))),
                                                                      (Exp_ident
                                                                      "tl")))]))
                                                               ))
                                                            ]))
                                                 ))
                                              ]))
                                   ))
                                ]))
                     ))
                  }
                ]
              ))
           ))
         ]
       ));
    (Str_value (Recursive,
       [(Val_binding ("append", [(Pat_var "xs"); (Pat_var "ys")],
           (Exp_match ((Exp_ident "xs"),
              [{ left = (Pat_construct ("[]", None)); right = (Exp_ident "ys")
                 };
                { left =
                  (Pat_construct ("::",
                     (Some (Pat_tuple [(Pat_var "x"); (Pat_var "xs")]))));
                  right =
                  (Exp_construct ("::",
                     (Some (Exp_tuple
                              [(Exp_ident "x");
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "append"),
                                      (Exp_ident "xs"))),
                                   (Exp_ident "ys")))
                                ]))
                     ))
                  }
                ]
              ))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("concat", [],
           (Exp_let (Recursive,
              [(Val_binding ("helper", [(Pat_var "xs")],
                  (Exp_match ((Exp_ident "xs"),
                     [{ left = (Pat_construct ("[]", None));
                        right = (Exp_construct ("[]", None)) };
                       { left =
                         (Pat_construct ("::",
                            (Some (Pat_tuple [(Pat_var "h"); (Pat_var "tl")]))
                            ));
                         right =
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "append"), (Exp_ident "h"))),
                            (Exp_apply ((Exp_ident "helper"), (Exp_ident "tl")
                               ))
                            ))
                         }
                       ]
                     ))
                  ))
                ],
              (Exp_ident "helper")))
           ))
         ]
       ));
    (Str_value (Recursive,
       [(Val_binding ("iter", [(Pat_var "f"); (Pat_var "xs")],
           (Exp_match ((Exp_ident "xs"),
              [{ left = (Pat_construct ("[]", None));
                 right = (Exp_constant Const_unit) };
                { left =
                  (Pat_construct ("::",
                     (Some (Pat_tuple [(Pat_var "h"); (Pat_var "tl")]))));
                  right =
                  (Exp_let (Nonrecursive,
                     [(Pat_binding ((Pat_constant Const_unit),
                         (Exp_apply ((Exp_ident "f"), (Exp_ident "h")))))
                       ],
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "iter"), (Exp_ident "f"))),
                        (Exp_ident "tl")))
                     ))
                  }
                ]
              ))
           ))
         ]
       ));
    (Str_value (Recursive,
       [(Val_binding ("cartesian", [(Pat_var "xs"); (Pat_var "ys")],
           (Exp_match ((Exp_ident "xs"),
              [{ left = (Pat_construct ("[]", None));
                 right = (Exp_construct ("[]", None)) };
                { left =
                  (Pat_construct ("::",
                     (Some (Pat_tuple [(Pat_var "h"); (Pat_var "tl")]))));
                  right =
                  (Exp_apply (
                     (Exp_apply ((Exp_ident "append"),
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "map"),
                              (Exp_fun ([(Pat_var "a")],
                                 (Exp_tuple [(Exp_ident "h"); (Exp_ident "a")])
                                 ))
                              )),
                           (Exp_ident "ys")))
                        )),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "cartesian"), (Exp_ident "tl"))),
                        (Exp_ident "ys")))
                     ))
                  }
                ]
              ))
           ))
         ]
       ));
    (Str_value (Nonrecursive,
       [(Val_binding ("main", [],
           (Exp_let (Nonrecursive,
              [(Pat_binding ((Pat_constant Const_unit),
                  (Exp_apply (
                     (Exp_apply ((Exp_ident "iter"), (Exp_ident "print_int"))),
                     (Exp_construct ("::",
                        (Some (Exp_tuple
                                 [(Exp_sequence ((Exp_constant (Const_int 1)),
                                     (Exp_sequence (
                                        (Exp_constant (Const_int 2)),
                                        (Exp_constant (Const_int 3))))
                                     ));
                                   (Exp_construct ("[]", None))]))
                        ))
                     ))
                  ))
                ],
              (Exp_let (Nonrecursive,
                 [(Pat_binding ((Pat_constant Const_unit),
                     (Exp_apply ((Exp_ident "print_int"),
                        (Exp_apply ((Exp_ident "length"),
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "cartesian"),
                                 (Exp_construct ("::",
                                    (Some (Exp_tuple
                                             [(Exp_sequence (
                                                 (Exp_constant (Const_int 1)),
                                                 (Exp_constant (Const_int 2))));
                                               (Exp_construct ("[]", None))]))
                                    ))
                                 )),
                              (Exp_construct ("::",
                                 (Some (Exp_tuple
                                          [(Exp_sequence (
                                              (Exp_constant (Const_int 1)),
                                              (Exp_sequence (
                                                 (Exp_constant (Const_int 2)),
                                                 (Exp_sequence (
                                                    (Exp_constant (Const_int 3)),
                                                    (Exp_constant (Const_int 4))
                                                    ))
                                                 ))
                                              ));
                                            (Exp_construct ("[]", None))]))
                                 ))
                              ))
                           ))
                        ))
                     ))
                   ],
                 (Exp_constant (Const_int 0))))
              ))
           ))
         ]
       ))
    ]

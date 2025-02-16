  $ ./parser_runner.exe < manytests/typed/001fac.ml
  [(Str_value
      (Decl (Recursive,
         [{ vb_pat = (Pat_var "fac");
            vb_expr =
            (Exp_function ((Pat_var "n"),
               (Exp_ifthenelse (
                  (Exp_apply ((Exp_apply ((Exp_ident "<="), (Exp_ident "n"))),
                     (Exp_constant (Const_int 1)))),
                  (Exp_constant (Const_int 1)),
                  (Exp_apply ((Exp_apply ((Exp_ident "*"), (Exp_ident "n"))),
                     (Exp_apply ((Exp_ident "fac"),
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                           (Exp_constant (Const_int 1))))
                        ))
                     ))
                  ))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_const Const_unit);
                      vb_expr =
                      (Exp_apply ((Exp_ident "print_int"),
                         (Exp_apply ((Exp_ident "fac"),
                            (Exp_constant (Const_int 4))))
                         ))
                      }
                     ]
                   )),
                (Exp_constant (Const_int 0))))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/002fac.ml
  [(Str_value
      (Decl (Recursive,
         [{ vb_pat = (Pat_var "fac_cps");
            vb_expr =
            (Exp_function ((Pat_var "n"),
               (Exp_function ((Pat_var "k"),
                  (Exp_ifthenelse (
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "="), (Exp_ident "n"))),
                        (Exp_constant (Const_int 1)))),
                     (Exp_apply ((Exp_ident "k"), (Exp_constant (Const_int 1))
                        )),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "fac_cps"),
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                              (Exp_constant (Const_int 1))))
                           )),
                        (Exp_function ((Pat_var "p"),
                           (Exp_apply ((Exp_ident "k"),
                              (Exp_apply (
                                 (Exp_apply ((Exp_ident "*"), (Exp_ident "p"))),
                                 (Exp_ident "n")))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_const Const_unit);
                      vb_expr =
                      (Exp_apply ((Exp_ident "print_int"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "fac_cps"),
                               (Exp_constant (Const_int 4)))),
                            (Exp_function ((Pat_var "print_int"),
                               (Exp_ident "print_int")))
                            ))
                         ))
                      }
                     ]
                   )),
                (Exp_constant (Const_int 0))))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/002fac.ml
  [(Str_value
      (Decl (Recursive,
         [{ vb_pat = (Pat_var "fac_cps");
            vb_expr =
            (Exp_function ((Pat_var "n"),
               (Exp_function ((Pat_var "k"),
                  (Exp_ifthenelse (
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "="), (Exp_ident "n"))),
                        (Exp_constant (Const_int 1)))),
                     (Exp_apply ((Exp_ident "k"), (Exp_constant (Const_int 1))
                        )),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "fac_cps"),
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                              (Exp_constant (Const_int 1))))
                           )),
                        (Exp_function ((Pat_var "p"),
                           (Exp_apply ((Exp_ident "k"),
                              (Exp_apply (
                                 (Exp_apply ((Exp_ident "*"), (Exp_ident "p"))),
                                 (Exp_ident "n")))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_const Const_unit);
                      vb_expr =
                      (Exp_apply ((Exp_ident "print_int"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "fac_cps"),
                               (Exp_constant (Const_int 4)))),
                            (Exp_function ((Pat_var "print_int"),
                               (Exp_ident "print_int")))
                            ))
                         ))
                      }
                     ]
                   )),
                (Exp_constant (Const_int 0))))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/003fib.ml
  [(Str_value
      (Decl (Recursive,
         [{ vb_pat = (Pat_var "fib_acc");
            vb_expr =
            (Exp_function ((Pat_var "a"),
               (Exp_function ((Pat_var "b"),
                  (Exp_function ((Pat_var "n"),
                     (Exp_ifthenelse (
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "="), (Exp_ident "n"))),
                           (Exp_constant (Const_int 1)))),
                        (Exp_ident "b"),
                        (Exp_let (
                           (Decl (Nonrecursive,
                              [{ vb_pat = (Pat_var "n1");
                                 vb_expr =
                                 (Exp_apply (
                                    (Exp_apply ((Exp_ident "-"),
                                       (Exp_ident "n"))),
                                    (Exp_constant (Const_int 1))))
                                 }
                                ]
                              )),
                           (Exp_let (
                              (Decl (Nonrecursive,
                                 [{ vb_pat = (Pat_var "ab");
                                    vb_expr =
                                    (Exp_apply (
                                       (Exp_apply ((Exp_ident "+"),
                                          (Exp_ident "a"))),
                                       (Exp_ident "b")))
                                    }
                                   ]
                                 )),
                              (Exp_apply (
                                 (Exp_apply (
                                    (Exp_apply ((Exp_ident "fib_acc"),
                                       (Exp_ident "b"))),
                                    (Exp_ident "ab"))),
                                 (Exp_ident "n1")))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Recursive,
          [{ vb_pat = (Pat_var "fib");
             vb_expr =
             (Exp_function ((Pat_var "n"),
                (Exp_ifthenelse (
                   (Exp_apply ((Exp_apply ((Exp_ident "<"), (Exp_ident "n"))),
                      (Exp_constant (Const_int 2)))),
                   (Exp_ident "n"),
                   (Exp_apply (
                      (Exp_apply ((Exp_ident "+"),
                         (Exp_apply ((Exp_ident "fib"),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                               (Exp_constant (Const_int 1))))
                            ))
                         )),
                      (Exp_apply ((Exp_ident "fib"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                            (Exp_constant (Const_int 2))))
                         ))
                      ))
                   ))
                ))
             }
            ]
          )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_const Const_unit);
                      vb_expr =
                      (Exp_apply ((Exp_ident "print_int"),
                         (Exp_apply (
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "fib_acc"),
                                  (Exp_constant (Const_int 0)))),
                               (Exp_constant (Const_int 1)))),
                            (Exp_constant (Const_int 4))))
                         ))
                      }
                     ]
                   )),
                (Exp_let (
                   (Decl (Nonrecursive,
                      [{ vb_pat = (Pat_const Const_unit);
                         vb_expr =
                         (Exp_apply ((Exp_ident "print_int"),
                            (Exp_apply ((Exp_ident "fib"),
                               (Exp_constant (Const_int 4))))
                            ))
                         }
                        ]
                      )),
                   (Exp_constant (Const_int 0))))
                ))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/004manyargs.ml
  [(Str_value
      (Decl (Nonrecursive,
         [{ vb_pat = (Pat_var "wrap");
            vb_expr =
            (Exp_function ((Pat_var "f"),
               (Exp_ifthenelse (
                  (Exp_apply (
                     (Exp_apply ((Exp_ident "="), (Exp_constant (Const_int 1))
                        )),
                     (Exp_constant (Const_int 1)))),
                  (Exp_ident "f"), (Exp_ident "f")))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "test3");
             vb_expr =
             (Exp_function ((Pat_var "a"),
                (Exp_function ((Pat_var "b"),
                   (Exp_function ((Pat_var "c"),
                      (Exp_let (
                         (Decl (Nonrecursive,
                            [{ vb_pat = (Pat_var "a");
                               vb_expr =
                               (Exp_apply ((Exp_ident "print_int"),
                                  (Exp_ident "a")))
                               }
                              ]
                            )),
                         (Exp_let (
                            (Decl (Nonrecursive,
                               [{ vb_pat = (Pat_var "b");
                                  vb_expr =
                                  (Exp_apply ((Exp_ident "print_int"),
                                     (Exp_ident "b")))
                                  }
                                 ]
                               )),
                            (Exp_let (
                               (Decl (Nonrecursive,
                                  [{ vb_pat = (Pat_var "c");
                                     vb_expr =
                                     (Exp_apply ((Exp_ident "print_int"),
                                        (Exp_ident "c")))
                                     }
                                    ]
                                  )),
                               (Exp_constant (Const_int 0))))
                            ))
                         ))
                      ))
                   ))
                ))
             }
            ]
          )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "test10");
             vb_expr =
             (Exp_function ((Pat_var "a"),
                (Exp_function ((Pat_var "b"),
                   (Exp_function ((Pat_var "c"),
                      (Exp_function ((Pat_var "d"),
                         (Exp_function ((Pat_var "e"),
                            (Exp_function ((Pat_var "f"),
                               (Exp_function ((Pat_var "g"),
                                  (Exp_function ((Pat_var "h"),
                                     (Exp_function ((Pat_var "i"),
                                        (Exp_function ((Pat_var "j"),
                                           (Exp_apply (
                                              (Exp_apply ((Exp_ident "+"),
                                                 (Exp_apply (
                                                    (Exp_apply (
                                                       (Exp_ident "+"),
                                                       (Exp_apply (
                                                          (Exp_apply (
                                                             (Exp_ident "+"),
                                                             (Exp_apply (
                                                                (Exp_apply (
                                                                   (Exp_ident
                                                                      "+"),
                                                                   (Exp_apply (
                                                                      (
                                                                      Exp_apply (
                                                                      (Exp_ident
                                                                      "+"),
                                                                      (Exp_apply (
                                                                      (Exp_apply (
                                                                      (Exp_ident
                                                                      "+"),
                                                                      (Exp_apply (
                                                                      (Exp_apply (
                                                                      (Exp_ident
                                                                      "+"),
                                                                      (Exp_apply (
                                                                      (Exp_apply (
                                                                      (Exp_ident
                                                                      "+"),
                                                                      (Exp_apply (
                                                                      (Exp_apply (
                                                                      (Exp_ident
                                                                      "+"),
                                                                      (Exp_ident
                                                                      "a"))),
                                                                      (Exp_ident
                                                                      "b"))))),
                                                                      (Exp_ident
                                                                      "c"))))),
                                                                      (Exp_ident
                                                                      "d"))))),
                                                                      (Exp_ident
                                                                      "e"))))),
                                                                      (
                                                                      Exp_ident
                                                                      "f")))
                                                                   )),
                                                                (Exp_ident "g")
                                                                ))
                                                             )),
                                                          (Exp_ident "h")))
                                                       )),
                                                    (Exp_ident "i")))
                                                 )),
                                              (Exp_ident "j")))
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
             }
            ]
          )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_var "rez");
                      vb_expr =
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
                                                    (Exp_apply (
                                                       (Exp_ident "wrap"),
                                                       (Exp_ident "test10"))),
                                                    (Exp_constant (Const_int 1))
                                                    )),
                                                 (Exp_constant (Const_int 10))
                                                 )),
                                              (Exp_constant (Const_int 100)))),
                                           (Exp_constant (Const_int 1000)))),
                                        (Exp_constant (Const_int 10000)))),
                                     (Exp_constant (Const_int 100000)))),
                                  (Exp_constant (Const_int 1000000)))),
                               (Exp_constant (Const_int 10000000)))),
                            (Exp_constant (Const_int 100000000)))),
                         (Exp_constant (Const_int 1000000000))))
                      }
                     ]
                   )),
                (Exp_let (
                   (Decl (Nonrecursive,
                      [{ vb_pat = (Pat_const Const_unit);
                         vb_expr =
                         (Exp_apply ((Exp_ident "print_int"), (Exp_ident "rez")
                            ))
                         }
                        ]
                      )),
                   (Exp_let (
                      (Decl (Nonrecursive,
                         [{ vb_pat = (Pat_var "temp2");
                            vb_expr =
                            (Exp_apply (
                               (Exp_apply (
                                  (Exp_apply (
                                     (Exp_apply ((Exp_ident "wrap"),
                                        (Exp_ident "test3"))),
                                     (Exp_constant (Const_int 1)))),
                                  (Exp_constant (Const_int 10)))),
                               (Exp_constant (Const_int 100))))
                            }
                           ]
                         )),
                      (Exp_constant (Const_int 0))))
                   ))
                ))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/005fix.ml
  [(Str_value
      (Decl (Recursive,
         [{ vb_pat = (Pat_var "fix");
            vb_expr =
            (Exp_function ((Pat_var "f"),
               (Exp_function ((Pat_var "x"),
                  (Exp_apply (
                     (Exp_apply ((Exp_ident "f"),
                        (Exp_apply ((Exp_ident "fix"), (Exp_ident "f"))))),
                     (Exp_ident "x")))
                  ))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "fac");
             vb_expr =
             (Exp_function ((Pat_var "self"),
                (Exp_function ((Pat_var "n"),
                   (Exp_ifthenelse (
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "<="), (Exp_ident "n"))),
                         (Exp_constant (Const_int 1)))),
                      (Exp_constant (Const_int 1)),
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "*"), (Exp_ident "n"))),
                         (Exp_apply ((Exp_ident "self"),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                               (Exp_constant (Const_int 1))))
                            ))
                         ))
                      ))
                   ))
                ))
             }
            ]
          )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_const Const_unit);
                      vb_expr =
                      (Exp_apply ((Exp_ident "print_int"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "fix"), (Exp_ident "fac"))),
                            (Exp_constant (Const_int 6))))
                         ))
                      }
                     ]
                   )),
                (Exp_constant (Const_int 0))))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/006partial.ml
  [(Str_value
      (Decl (Nonrecursive,
         [{ vb_pat = (Pat_var "foo");
            vb_expr =
            (Exp_function ((Pat_var "b"),
               (Exp_ifthenelse ((Exp_ident "b"),
                  (Exp_function ((Pat_var "foo"),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "+"), (Exp_ident "foo"))),
                        (Exp_constant (Const_int 2))))
                     )),
                  (Exp_function ((Pat_var "foo"),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "*"), (Exp_ident "foo"))),
                        (Exp_constant (Const_int 10))))
                     ))
                  ))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "foo");
             vb_expr =
             (Exp_function ((Pat_var "x"),
                (Exp_apply (
                   (Exp_apply ((Exp_ident "foo"),
                      (Exp_constant (Const_bool true)))),
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
             }
            ]
          )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_const Const_unit);
                      vb_expr =
                      (Exp_apply ((Exp_ident "print_int"),
                         (Exp_apply ((Exp_ident "foo"),
                            (Exp_constant (Const_int 11))))
                         ))
                      }
                     ]
                   )),
                (Exp_constant (Const_int 0))))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/006partial2.ml
  [(Str_value
      (Decl (Nonrecursive,
         [{ vb_pat = (Pat_var "foo");
            vb_expr =
            (Exp_function ((Pat_var "a"),
               (Exp_function ((Pat_var "b"),
                  (Exp_function ((Pat_var "c"),
                     (Exp_let (
                        (Decl (Nonrecursive,
                           [{ vb_pat = (Pat_const Const_unit);
                              vb_expr =
                              (Exp_apply ((Exp_ident "print_int"),
                                 (Exp_ident "a")))
                              }
                             ]
                           )),
                        (Exp_let (
                           (Decl (Nonrecursive,
                              [{ vb_pat = (Pat_const Const_unit);
                                 vb_expr =
                                 (Exp_apply ((Exp_ident "print_int"),
                                    (Exp_ident "b")))
                                 }
                                ]
                              )),
                           (Exp_let (
                              (Decl (Nonrecursive,
                                 [{ vb_pat = (Pat_const Const_unit);
                                    vb_expr =
                                    (Exp_apply ((Exp_ident "print_int"),
                                       (Exp_ident "c")))
                                    }
                                   ]
                                 )),
                              (Exp_apply (
                                 (Exp_apply ((Exp_ident "+"), (Exp_ident "a"))),
                                 (Exp_apply (
                                    (Exp_apply ((Exp_ident "*"),
                                       (Exp_ident "b"))),
                                    (Exp_ident "c")))
                                 ))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_var "foo");
                      vb_expr =
                      (Exp_apply ((Exp_ident "foo"),
                         (Exp_constant (Const_int 1))))
                      }
                     ]
                   )),
                (Exp_let (
                   (Decl (Nonrecursive,
                      [{ vb_pat = (Pat_var "foo");
                         vb_expr =
                         (Exp_apply ((Exp_ident "foo"),
                            (Exp_constant (Const_int 2))))
                         }
                        ]
                      )),
                   (Exp_let (
                      (Decl (Nonrecursive,
                         [{ vb_pat = (Pat_var "foo");
                            vb_expr =
                            (Exp_apply ((Exp_ident "foo"),
                               (Exp_constant (Const_int 3))))
                            }
                           ]
                         )),
                      (Exp_let (
                         (Decl (Nonrecursive,
                            [{ vb_pat = (Pat_const Const_unit);
                               vb_expr =
                               (Exp_apply ((Exp_ident "print_int"),
                                  (Exp_ident "foo")))
                               }
                              ]
                            )),
                         (Exp_constant (Const_int 0))))
                      ))
                   ))
                ))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/006partial3.ml
  [(Str_value
      (Decl (Nonrecursive,
         [{ vb_pat = (Pat_var "foo");
            vb_expr =
            (Exp_function ((Pat_var "a"),
               (Exp_let (
                  (Decl (Nonrecursive,
                     [{ vb_pat = (Pat_const Const_unit);
                        vb_expr =
                        (Exp_apply ((Exp_ident "print_int"), (Exp_ident "a")))
                        }
                       ]
                     )),
                  (Exp_function ((Pat_var "b"),
                     (Exp_let (
                        (Decl (Nonrecursive,
                           [{ vb_pat = (Pat_const Const_unit);
                              vb_expr =
                              (Exp_apply ((Exp_ident "print_int"),
                                 (Exp_ident "b")))
                              }
                             ]
                           )),
                        (Exp_function ((Pat_var "c"),
                           (Exp_apply ((Exp_ident "print_int"), (Exp_ident "c")
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_const Const_unit);
                      vb_expr =
                      (Exp_apply (
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "foo"),
                               (Exp_constant (Const_int 4)))),
                            (Exp_constant (Const_int 8)))),
                         (Exp_constant (Const_int 9))))
                      }
                     ]
                   )),
                (Exp_constant (Const_int 0))))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/007order.ml
  [(Str_value
      (Decl (Nonrecursive,
         [{ vb_pat = (Pat_var "_start");
            vb_expr =
            (Exp_function ((Pat_const Const_unit),
               (Exp_function ((Pat_const Const_unit),
                  (Exp_function ((Pat_var "a"),
                     (Exp_function ((Pat_const Const_unit),
                        (Exp_function ((Pat_var "b"),
                           (Exp_function ((Pat_var "_c"),
                              (Exp_function ((Pat_const Const_unit),
                                 (Exp_function ((Pat_var "d"),
                                    (Exp_function ((Pat_var "__"),
                                       (Exp_let (
                                          (Decl (Nonrecursive,
                                             [{ vb_pat = (Pat_const Const_unit);
                                                vb_expr =
                                                (Exp_apply (
                                                   (Exp_ident "print_int"),
                                                   (Exp_apply (
                                                      (Exp_apply (
                                                         (Exp_ident "+"),
                                                         (Exp_ident "a"))),
                                                      (Exp_ident "b")))
                                                   ))
                                                }
                                               ]
                                             )),
                                          (Exp_let (
                                             (Decl (Nonrecursive,
                                                [{ vb_pat =
                                                   (Pat_const Const_unit);
                                                   vb_expr =
                                                   (Exp_apply (
                                                      (Exp_ident "print_int"),
                                                      (Exp_ident "__")))
                                                   }
                                                  ]
                                                )),
                                             (Exp_apply (
                                                (Exp_apply ((Exp_ident "+"),
                                                   (Exp_apply (
                                                      (Exp_apply (
                                                         (Exp_ident "/"),
                                                         (Exp_apply (
                                                            (Exp_apply (
                                                               (Exp_ident "*"),
                                                               (Exp_ident "a")
                                                               )),
                                                            (Exp_ident "b")))
                                                         )),
                                                      (Exp_ident "_c")))
                                                   )),
                                                (Exp_ident "d")))
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
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
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
                            (Exp_apply ((Exp_ident "~-"),
                               (Exp_constant (Const_int 1))))
                            ))
                         )),
                      (Exp_constant (Const_int 10000)))),
                   (Exp_apply ((Exp_ident "~-"),
                      (Exp_constant (Const_int 555555))))
                   ))
                ))
             }
            ]
          )))
    ]

  $ ./parser_runner.exe < manytests/typed/016lists.ml
  [(Str_value
      (Decl (Recursive,
         [{ vb_pat = (Pat_var "length");
            vb_expr =
            (Exp_function ((Pat_var "xs"),
               (Exp_match ((Exp_ident "xs"),
                  [((Pat_const Const_nil), (Exp_constant (Const_int 0)));
                    ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "+"),
                           (Exp_constant (Const_int 1)))),
                        (Exp_apply ((Exp_ident "length"), (Exp_ident "tl"))))))
                    ]
                  ))
               ))
            }
           ]
         )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "length_tail");
             vb_expr =
             (Exp_let (
                (Decl (Recursive,
                   [{ vb_pat = (Pat_var "helper");
                      vb_expr =
                      (Exp_function ((Pat_var "acc"),
                         (Exp_function ((Pat_var "xs"),
                            (Exp_match ((Exp_ident "xs"),
                               [((Pat_const Const_nil), (Exp_ident "acc"));
                                 ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                                  (Exp_apply (
                                     (Exp_apply ((Exp_ident "helper"),
                                        (Exp_apply (
                                           (Exp_apply ((Exp_ident "+"),
                                              (Exp_ident "acc"))),
                                           (Exp_constant (Const_int 1))))
                                        )),
                                     (Exp_ident "tl"))))
                                 ]
                               ))
                            ))
                         ))
                      }
                     ]
                   )),
                (Exp_apply ((Exp_ident "helper"), (Exp_constant (Const_int 0))
                   ))
                ))
             }
            ]
          )));
    (Str_value
       (Decl (Recursive,
          [{ vb_pat = (Pat_var "map");
             vb_expr =
             (Exp_function ((Pat_var "f"),
                (Exp_function ((Pat_var "xs"),
                   (Exp_match ((Exp_ident "xs"),
                      [((Pat_const Const_nil), (Exp_constant Const_nil));
                        ((Pat_cons ((Pat_var "a"), (Pat_const Const_nil))),
                         (Exp_list (
                            (Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                            (Exp_constant Const_nil))));
                        ((Pat_cons ((Pat_var "a"),
                            (Pat_cons ((Pat_var "b"), (Pat_const Const_nil))))),
                         (Exp_list (
                            (Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                            (Exp_list (
                               (Exp_apply ((Exp_ident "f"), (Exp_ident "b"))),
                               (Exp_constant Const_nil)))
                            )));
                        ((Pat_cons ((Pat_var "a"),
                            (Pat_cons ((Pat_var "b"),
                               (Pat_cons ((Pat_var "c"), (Pat_const Const_nil)
                                  ))
                               ))
                            )),
                         (Exp_list (
                            (Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                            (Exp_list (
                               (Exp_apply ((Exp_ident "f"), (Exp_ident "b"))),
                               (Exp_list (
                                  (Exp_apply ((Exp_ident "f"), (Exp_ident "c")
                                     )),
                                  (Exp_constant Const_nil)))
                               ))
                            )));
                        ((Pat_cons ((Pat_var "a"),
                            (Pat_cons ((Pat_var "b"),
                               (Pat_cons ((Pat_var "c"),
                                  (Pat_cons ((Pat_var "d"), (Pat_var "tl")))))
                               ))
                            )),
                         (Exp_list (
                            (Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                            (Exp_list (
                               (Exp_apply ((Exp_ident "f"), (Exp_ident "b"))),
                               (Exp_list (
                                  (Exp_apply ((Exp_ident "f"), (Exp_ident "c")
                                     )),
                                  (Exp_list (
                                     (Exp_apply ((Exp_ident "f"),
                                        (Exp_ident "d"))),
                                     (Exp_apply (
                                        (Exp_apply ((Exp_ident "map"),
                                           (Exp_ident "f"))),
                                        (Exp_ident "tl")))
                                     ))
                                  ))
                               ))
                            )))
                        ]
                      ))
                   ))
                ))
             }
            ]
          )));
    (Str_value
       (Decl (Recursive,
          [{ vb_pat = (Pat_var "append");
             vb_expr =
             (Exp_function ((Pat_var "xs"),
                (Exp_function ((Pat_var "ys"),
                   (Exp_match ((Exp_ident "xs"),
                      [((Pat_const Const_nil), (Exp_ident "ys"));
                        ((Pat_cons ((Pat_var "x"), (Pat_var "xs"))),
                         (Exp_list ((Exp_ident "x"),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "append"),
                                  (Exp_ident "xs"))),
                               (Exp_ident "ys")))
                            )))
                        ]
                      ))
                   ))
                ))
             }
            ]
          )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "concat");
             vb_expr =
             (Exp_let (
                (Decl (Recursive,
                   [{ vb_pat = (Pat_var "helper");
                      vb_expr =
                      (Exp_function ((Pat_var "xs"),
                         (Exp_match ((Exp_ident "xs"),
                            [((Pat_const Const_nil), (Exp_constant Const_nil));
                              ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                               (Exp_apply (
                                  (Exp_apply ((Exp_ident "append"),
                                     (Exp_ident "h"))),
                                  (Exp_apply ((Exp_ident "helper"),
                                     (Exp_ident "tl")))
                                  )))
                              ]
                            ))
                         ))
                      }
                     ]
                   )),
                (Exp_ident "helper")))
             }
            ]
          )));
    (Str_value
       (Decl (Recursive,
          [{ vb_pat = (Pat_var "iter");
             vb_expr =
             (Exp_function ((Pat_var "f"),
                (Exp_function ((Pat_var "xs"),
                   (Exp_match ((Exp_ident "xs"),
                      [((Pat_const Const_nil), (Exp_constant Const_unit));
                        ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                         (Exp_let (
                            (Decl (Nonrecursive,
                               [{ vb_pat = (Pat_const Const_unit);
                                  vb_expr =
                                  (Exp_apply ((Exp_ident "f"), (Exp_ident "h")
                                     ))
                                  }
                                 ]
                               )),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "iter"), (Exp_ident "f")
                                  )),
                               (Exp_ident "tl")))
                            )))
                        ]
                      ))
                   ))
                ))
             }
            ]
          )));
    (Str_value
       (Decl (Recursive,
          [{ vb_pat = (Pat_var "cartesian");
             vb_expr =
             (Exp_function ((Pat_var "xs"),
                (Exp_function ((Pat_var "ys"),
                   (Exp_match ((Exp_ident "xs"),
                      [((Pat_const Const_nil), (Exp_constant Const_nil));
                        ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "append"),
                               (Exp_apply (
                                  (Exp_apply ((Exp_ident "map"),
                                     (Exp_function ((Pat_var "a"),
                                        (Exp_tuple
                                           [(Exp_ident "h"); (Exp_ident "a")])
                                        ))
                                     )),
                                  (Exp_ident "ys")))
                               )),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "cartesian"),
                                  (Exp_ident "tl"))),
                               (Exp_ident "ys")))
                            )))
                        ]
                      ))
                   ))
                ))
             }
            ]
          )));
    (Str_value
       (Decl (Nonrecursive,
          [{ vb_pat = (Pat_var "main");
             vb_expr =
             (Exp_let (
                (Decl (Nonrecursive,
                   [{ vb_pat = (Pat_const Const_unit);
                      vb_expr =
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "iter"),
                            (Exp_ident "print_int"))),
                         (Exp_list ((Exp_constant (Const_int 1)),
                            (Exp_list ((Exp_constant (Const_int 2)),
                               (Exp_list ((Exp_constant (Const_int 3)),
                                  (Exp_constant Const_nil)))
                               ))
                            ))
                         ))
                      }
                     ]
                   )),
                (Exp_let (
                   (Decl (Nonrecursive,
                      [{ vb_pat = (Pat_const Const_unit);
                         vb_expr =
                         (Exp_apply ((Exp_ident "print_int"),
                            (Exp_apply ((Exp_ident "length"),
                               (Exp_apply (
                                  (Exp_apply ((Exp_ident "cartesian"),
                                     (Exp_list ((Exp_constant (Const_int 1)),
                                        (Exp_list (
                                           (Exp_constant (Const_int 2)),
                                           (Exp_constant Const_nil)))
                                        ))
                                     )),
                                  (Exp_list ((Exp_constant (Const_int 1)),
                                     (Exp_list ((Exp_constant (Const_int 2)),
                                        (Exp_list (
                                           (Exp_constant (Const_int 3)),
                                           (Exp_list (
                                              (Exp_constant (Const_int 4)),
                                              (Exp_constant Const_nil)))
                                           ))
                                        ))
                                     ))
                                  ))
                               ))
                            ))
                         }
                        ]
                      )),
                   (Exp_constant (Const_int 0))))
                ))
             }
            ]
          )))
    ]

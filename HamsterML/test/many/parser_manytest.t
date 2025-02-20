  $ dune exec ParserRunner < manytests/do_not_type/001.ml
  [(Ast.Let (Ast.Nonrecursive,
      [((Ast.Var "recfac"), [(Ast.Var "n")],
        (Ast.If (
           (Ast.Application (
              (Ast.Application ((Ast.EOperation (Ast.Binary Ast.LTE)),
                 (Ast.EVar "n"))),
              (Ast.EConst (Ast.Int 1)))),
           (Ast.EConst (Ast.Int 1)),
           (Some (Ast.Application (
                    (Ast.Application ((Ast.EOperation (Ast.Binary Ast.MUL)),
                       (Ast.EVar "n"))),
                    (Ast.Application ((Ast.EVar "fac"),
                       (Ast.Application (
                          (Ast.Application (
                             (Ast.EOperation (Ast.Binary Ast.SUB)),
                             (Ast.EVar "n"))),
                          (Ast.EConst (Ast.Int 1))))
                       ))
                    )))
           )))
        ],
      None))
    ]

  $ dune exec ParserRunner < manytests/do_not_type/002if.ml
  [(Ast.Let (Ast.Nonrecursive,
      [((Ast.Var "main"), [],
        (Ast.If ((Ast.EConst (Ast.Bool true)), (Ast.EConst (Ast.Int 1)),
           (Some (Ast.EConst (Ast.Bool false))))))
        ],
      None))
    ]

  $ dune exec ParserRunner < manytests/do_not_type/003occurs.ml
  [(Ast.Let (Ast.Nonrecursive,
      [((Ast.Var "fix"), [(Ast.Var "f")],
        (Ast.Application (
           (Ast.Fun ([(Ast.Var "x")],
              (Ast.Application ((Ast.EVar "f"),
                 (Ast.Fun ([(Ast.Var "f")],
                    (Ast.Application (
                       (Ast.Application ((Ast.EVar "x"), (Ast.EVar "x"))),
                       (Ast.EVar "f")))
                    ))
                 ))
              )),
           (Ast.Fun ([(Ast.Var "x")],
              (Ast.Application ((Ast.EVar "f"),
                 (Ast.Fun ([(Ast.Var "f")],
                    (Ast.Application (
                       (Ast.Application ((Ast.EVar "x"), (Ast.EVar "x"))),
                       (Ast.EVar "f")))
                    ))
                 ))
              ))
           )))
        ],
      None))
    ]

  $ dune exec ParserRunner < manytests/do_not_type/004let_poly.ml
  [(Ast.Let (Ast.Nonrecursive,
      [((Ast.Var "temp"), [],
        (Ast.Application (
           (Ast.Fun ([(Ast.Var "f")],
              (Ast.ETuple (
                 (Ast.Application ((Ast.EVar "f"), (Ast.EConst (Ast.Int 1)))),
                 (Ast.Application ((Ast.EVar "f"), (Ast.EConst (Ast.Bool true))
                    )),
                 []))
              )),
           (Ast.Fun ([(Ast.Var "x")], (Ast.EVar "x"))))))
        ],
      None))
    ]

  $ dune exec ParserRunner < manytests/do_not_type/015tuples.ml
  [(Ast.Let (Ast.Recursive,
      [((Ast.Tuple ((Ast.Var "a"), (Ast.Var "b"), [])), [],
        (Ast.ETuple ((Ast.EVar "a"), (Ast.EVar "b"), [])))],
      None))
    ]

  $ dune exec ParserRunner < manytests/do_not_type/099.ml
  [(Ast.Let (Ast.Recursive,
      [((Ast.Var "Some"), [(Ast.Var "x")],
        (Ast.Application ((Ast.EVar "Some"), (Ast.EConst (Ast.Int 1)))))],
      None));
    (Ast.Let (Ast.Nonrecursive,
       [((Ast.Var "Some"), [(Ast.Var "a")],
         (Ast.EOperation (Ast.Binary Ast.LT)))],
       None));
    (Ast.Let (Ast.Nonrecursive,
       [((Ast.Const Ast.Unit), [], (Ast.Fun ([(Ast.Var "x")], (Ast.EVar "x"))))
         ],
       None))
    ]

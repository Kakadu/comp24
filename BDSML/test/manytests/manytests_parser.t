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
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/001fac.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/002fac.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/003fib.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/004manyargs.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/005fix.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/006partial.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/006partial2.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/006partial3.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/007order.ml
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/008ascription.ml
  Error: end_of_input
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
  Error: end_of_input
  $ dune exec manytests_parser < manytests_link/typed/016lists.ml
  Error: end_of_input

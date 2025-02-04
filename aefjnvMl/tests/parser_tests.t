  $ ./parser_runner.exe < manytests/typed/001fac.ml
  [(Str_value
      { d_rec = Recursive; d_pat = (Pat_var "fac");
        d_expr =
        (Exp_function ((Pat_var "n"),
           (Exp_ifthenelse (
              (Exp_apply ((Exp_apply ((Exp_ident "<="), (Exp_ident "n"))),
                 (Exp_constant (Const_int 1)))),
              (Exp_constant (Const_int 1)),
              (Exp_apply ((Exp_apply ((Exp_ident "*"), (Exp_ident "n"))),
                 (Exp_apply ((Exp_ident "fac"),
                    (Exp_apply ((Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                       (Exp_constant (Const_int 1))))
                    ))
                 ))
              ))
           ))
        });
    (Str_value
       { d_rec = Nonrecursive; d_pat = (Pat_var "main");
         d_expr =
         (Exp_let (
            { d_rec = Nonrecursive; d_pat = (Pat_const Const_unit);
              d_expr =
              (Exp_apply ((Exp_ident "print_int"),
                 (Exp_apply ((Exp_ident "fac"), (Exp_constant (Const_int 4))))
                 ))
              },
            (Exp_constant (Const_int 0))))
         })
    ]

  $ ./parser_runner.exe < manytests/typed/002fac.ml

  $ ./parser_runner.exe < manytests/typed/007order.ml

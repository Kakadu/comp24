  $ ./parser_runner.exe < manytests/typed/001fac.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/002fac.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/003fib.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/004manyargs.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/005fix.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/006partial.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/006partial2.ml
  Error: : end_of_input
  $ ./parser_runner.exe < manytests/typed/006partial3.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/007order.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/008ascription.ml
  Error: : end_of_input

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
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/015tuples.ml
  Error: : end_of_input

  $ ./parser_runner.exe < manytests/typed/016lists.ml
  Error: : end_of_input

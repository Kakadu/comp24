  $ ./demoParse.exe << EOF
  > let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
  > EOF
  [(SILet (Recursive,
      [(Binding ((PVar (Id "factorial_recursive")),
          (EFun ([(PVar (Id "n"))],
             (EIf (
                (EApp ((EApp ((EVar (Id "( <= )")), (EVar (Id "n")))),
                   (EConst (CInt 1)))),
                (EConst (CInt 1)),
                (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "n")))),
                   (EApp ((EVar (Id "factorial_recursive")),
                      (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                         (EConst (CInt 1))))
                      ))
                   ))
                ))
             ))
          ))
        ]
      ))
    ]

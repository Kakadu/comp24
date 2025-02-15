  $ ./demos.exe <<- EOF
  > let rec fibo_cps = fun n acc -> match n with
  >   | 0 -> acc 0
  >   | 1 -> acc 1
  >   | _ -> fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
  [(Let (Rec, "fibo_cps",
      (EFun ((PVar ("n", TUnknown)),
         (EFun ((PVar ("acc", TUnknown)),
            (EMatch ((EVar ("n", TUnknown)),
               [((PConst (CInt 0)),
                 (EApp ((EVar ("acc", TUnknown)), (EConst (CInt 0)))));
                 ((PConst (CInt 1)),
                  (EApp ((EVar ("acc", TUnknown)), (EConst (CInt 1)))));
                 (PWild,
                  (EApp (
                     (EApp ((EVar ("fibo_cps", TUnknown)),
                        (EBinaryOp (Sub, (EVar ("n", TUnknown)),
                           (EConst (CInt 1))))
                        )),
                     (EFun ((PVar ("x", TUnknown)),
                        (EApp (
                           (EApp ((EVar ("fibo_cps", TUnknown)),
                              (EBinaryOp (Sub, (EVar ("n", TUnknown)),
                                 (EConst (CInt 2))))
                              )),
                           (EFun ((PVar ("y", TUnknown)),
                              (EApp ((EVar ("acc", TUnknown)),
                                 (EBinaryOp (Add, (EVar ("x", TUnknown)),
                                    (EVar ("y", TUnknown))))
                                 ))
                              ))
                           ))
                        ))
                     )))
                 ]
               ))
            ))
         ))
      ))
    ]

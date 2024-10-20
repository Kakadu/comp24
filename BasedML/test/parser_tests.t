  $ dune exec parser_demo << EOF
  > let fibo = fun n -> let rec fiboCPS = fun n acc -> match n with
  >   | 0 -> acc 0
  >   | 1 -> acc 1
  >   | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
  > in
  > fiboCPS n (fun x -> x)
  > EOF
  [(DSingleLet
      (DLet (NotRec, (PNConstraint (PIdentifier "fibo")),
         (EFunction ((PNConstraint (PIdentifier "n")),
            (ELetIn (Rec, (PNConstraint (PIdentifier "fiboCPS")),
               (EFunction ((PNConstraint (PIdentifier "n")),
                  (EFunction ((PNConstraint (PIdentifier "acc")),
                     (EMatch ((PNConstraint (PIdentifier "n")),
                        [((PNConstraint (PConstant (CInt 0))),
                          (EApplication ((EIdentifier "acc"),
                             (EConstant (CInt 0)))));
                          ((PNConstraint (PConstant (CInt 1))),
                           (EApplication ((EIdentifier "acc"),
                              (EConstant (CInt 1)))));
                          ((PNConstraint PWildCard),
                           (EApplication (
                              (EApplication ((EIdentifier "fiboCPS"),
                                 (EApplication (
                                    (EApplication ((EIdentifier "( - )"),
                                       (EIdentifier "n"))),
                                    (EConstant (CInt 1))))
                                 )),
                              (EFunction ((PNConstraint (PIdentifier "x")),
                                 (EApplication (
                                    (EApplication ((EIdentifier "fiboCPS"),
                                       (EApplication (
                                          (EApplication ((EIdentifier "( - )"),
                                             (EIdentifier "n"))),
                                          (EConstant (CInt 2))))
                                       )),
                                    (EFunction (
                                       (PNConstraint (PIdentifier "y")),
                                       (EApplication ((EIdentifier "acc"),
                                          (EApplication (
                                             (EApplication (
                                                (EIdentifier "( + )"),
                                                (EIdentifier "x"))),
                                             (EIdentifier "y")))
                                          ))
                                       ))
                                    ))
                                 ))
                              )))
                          ]
                        ))
                     ))
                  )),
               (EApplication (
                  (EApplication ((EIdentifier "fiboCPS"), (EIdentifier "n"))),
                  (EFunction ((PNConstraint (PIdentifier "x")),
                     (EIdentifier "x")))
                  ))
               ))
            ))
         )))
    ]

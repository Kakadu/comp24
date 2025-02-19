  $ dune exec parser_demo << EOF
  > let    true = 3
  > EOF
  [(DSingleLet (NotRec, (DLet ((PConstant (CBool true)), (EConstant (CInt 3))))
      ))
    ]
  $ dune exec parser_demo << EOF
  > let x = true
  > let y = false
  > EOF
  [(DSingleLet (NotRec, (DLet ((PIdentifier "x"), (EConstant (CBool true))))));
    (DSingleLet (NotRec, (DLet ((PIdentifier "y"), (EConstant (CBool false))))
       ))
    ]

  $ dune exec parser_demo << EOF
  > let x = true
  > 
  > 
  > let y = false
  > EOF
  [(DSingleLet (NotRec, (DLet ((PIdentifier "x"), (EConstant (CBool true))))));
    (DSingleLet (NotRec, (DLet ((PIdentifier "y"), (EConstant (CBool false))))
       ))
    ]

  $ dune exec parser_demo << EOF
  > let x = (5 + 5) * 6 + (5 + 5) / 2
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PIdentifier "x"),
         (EApplication (
            (EApplication ((EIdentifier "( + )"),
               (EApplication (
                  (EApplication ((EIdentifier "( * )"),
                     (EApplication (
                        (EApplication ((EIdentifier "( + )"),
                           (EConstant (CInt 5)))),
                        (EConstant (CInt 5))))
                     )),
                  (EConstant (CInt 6))))
               )),
            (EApplication (
               (EApplication ((EIdentifier "( / )"),
                  (EApplication (
                     (EApplication ((EIdentifier "( + )"), (EConstant (CInt 5))
                        )),
                     (EConstant (CInt 5))))
                  )),
               (EConstant (CInt 2))))
            ))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let x = if 5 > 4 then true else false
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PIdentifier "x"),
         (EIfThenElse (
            (EApplication (
               (EApplication ((EIdentifier "( > )"), (EConstant (CInt 5)))),
               (EConstant (CInt 4)))),
            (EConstant (CBool true)), (EConstant (CBool false))))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let succ = fun n -> n + 1
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PIdentifier "succ"),
         (EFunction ((PIdentifier "n"),
            (EApplication (
               (EApplication ((EIdentifier "( + )"), (EIdentifier "n"))),
               (EConstant (CInt 1))))
            ))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let x = let y = 5 in y
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PIdentifier "x"),
         (ELetIn (NotRec, (PIdentifier "y"), (EConstant (CInt 5)),
            (EIdentifier "y")))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let (x, y) = (1, 2)
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PTuple [(PIdentifier "x"); (PIdentifier "y")]),
         (ETuple [(EConstant (CInt 1)); (EConstant (CInt 2))])))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let x :: y = [1; 2]
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PCons ((PIdentifier "x"), (PIdentifier "y"))),
         (EApplication (
            (EApplication ((EIdentifier "( :: )"), (EConstant (CInt 1)))),
            (EApplication (
               (EApplication ((EIdentifier "( :: )"), (EConstant (CInt 2)))),
               (EConstant CNil)))
            ))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let rec fac = fun n -> if n = 0 then 1 else n * (fac (n - 1))
  > EOF
  [(DSingleLet (Rec,
      (DLet ((PIdentifier "fac"),
         (EFunction ((PIdentifier "n"),
            (EIfThenElse (
               (EApplication (
                  (EApplication ((EIdentifier "( = )"), (EIdentifier "n"))),
                  (EConstant (CInt 0)))),
               (EConstant (CInt 1)),
               (EApplication (
                  (EApplication ((EIdentifier "( * )"), (EIdentifier "n"))),
                  (EApplication ((EIdentifier "fac"),
                     (EApplication (
                        (EApplication ((EIdentifier "( - )"), (EIdentifier "n")
                           )),
                        (EConstant (CInt 1))))
                     ))
                  ))
               ))
            ))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let rec facCPS = fun n k -> if n = 0 then k 1 else facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  [(DSingleLet (Rec,
      (DLet ((PIdentifier "facCPS"),
         (EFunction ((PIdentifier "n"),
            (EFunction ((PIdentifier "k"),
               (EIfThenElse (
                  (EApplication (
                     (EApplication ((EIdentifier "( = )"), (EIdentifier "n"))),
                     (EConstant (CInt 0)))),
                  (EApplication ((EIdentifier "k"), (EConstant (CInt 1)))),
                  (EApplication (
                     (EApplication ((EIdentifier "facCPS"),
                        (EApplication (
                           (EApplication ((EIdentifier "( - )"),
                              (EIdentifier "n"))),
                           (EConstant (CInt 1))))
                        )),
                     (EFunction ((PIdentifier "t"),
                        (EApplication ((EIdentifier "k"),
                           (EApplication (
                              (EApplication ((EIdentifier "( * )"),
                                 (EIdentifier "n"))),
                              (EIdentifier "t")))
                           ))
                        ))
                     ))
                  ))
               ))
            ))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let rec map = fun f list -> match list with
  >   | [] -> list
  >   | h :: tl -> map f ((f h) :: tl)
  > EOF
  [(DSingleLet (Rec,
      (DLet ((PIdentifier "map"),
         (EFunction ((PIdentifier "f"),
            (EFunction ((PIdentifier "list"),
               (EMatch ((EIdentifier "list"),
                  [((PConstant CNil), (EIdentifier "list"));
                    ((PCons ((PIdentifier "h"), (PIdentifier "tl"))),
                     (EApplication (
                        (EApplication ((EIdentifier "map"), (EIdentifier "f"))),
                        (EApplication (
                           (EApplication ((EIdentifier "( :: )"),
                              (EApplication ((EIdentifier "f"),
                                 (EIdentifier "h")))
                              )),
                           (EIdentifier "tl")))
                        )))
                    ]
                  ))
               ))
            ))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let rec facCPS = fun n k -> match n with
  >   | 0 -> k 1
  >   | n -> facCPS (n - 1) (fun t -> k (n * t))
  > EOF
  [(DSingleLet (Rec,
      (DLet ((PIdentifier "facCPS"),
         (EFunction ((PIdentifier "n"),
            (EFunction ((PIdentifier "k"),
               (EMatch ((EIdentifier "n"),
                  [((PConstant (CInt 0)),
                    (EApplication ((EIdentifier "k"), (EConstant (CInt 1)))));
                    ((PIdentifier "n"),
                     (EApplication (
                        (EApplication ((EIdentifier "facCPS"),
                           (EApplication (
                              (EApplication ((EIdentifier "( - )"),
                                 (EIdentifier "n"))),
                              (EConstant (CInt 1))))
                           )),
                        (EFunction ((PIdentifier "t"),
                           (EApplication ((EIdentifier "k"),
                              (EApplication (
                                 (EApplication ((EIdentifier "( * )"),
                                    (EIdentifier "n"))),
                                 (EIdentifier "t")))
                              ))
                           ))
                        )))
                    ]
                  ))
               ))
            ))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let rec even = fun n -> match n with
  >   | 0 -> true
  >   | x -> odd (x - 1)
  > and odd = fun n -> match n with
  >   | 0 -> false
  >   | x -> even (x - 1)
  > EOF
  [(DMutualRecDecl (Rec,
      [(DLet ((PIdentifier "even"),
          (EFunction ((PIdentifier "n"),
             (EMatch ((EIdentifier "n"),
                [((PConstant (CInt 0)), (EConstant (CBool true)));
                  ((PIdentifier "x"),
                   (EApplication ((EIdentifier "odd"),
                      (EApplication (
                         (EApplication ((EIdentifier "( - )"),
                            (EIdentifier "x"))),
                         (EConstant (CInt 1))))
                      )))
                  ]
                ))
             ))
          ));
        (DLet ((PIdentifier "odd"),
           (EFunction ((PIdentifier "n"),
              (EMatch ((EIdentifier "n"),
                 [((PConstant (CInt 0)), (EConstant (CBool false)));
                   ((PIdentifier "x"),
                    (EApplication ((EIdentifier "even"),
                       (EApplication (
                          (EApplication ((EIdentifier "( - )"),
                             (EIdentifier "x"))),
                          (EConstant (CInt 1))))
                       )))
                   ]
                 ))
              ))
           ))
        ]
      ))
    ]

  $ dune exec parser_demo << EOF
  > let (x : int -> bool -> 'loooong) = some_func
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PConstraint ((PIdentifier "x"), (int -> (bool -> 'loooong)))),
         (EIdentifier "some_func")))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let (x : ('b * int -> int * 'pa) list list) = ([[fun1; fun2]] : ('b * int -> int * 'a) list list)
  > EOF
  [(DSingleLet (NotRec,
      (DLet (
         (PConstraint ((PIdentifier "x"),
            (((('b * int) -> (int * 'pa)) list) list))),
         (EConstraint (
            (EApplication (
               (EApplication ((EIdentifier "( :: )"),
                  (EApplication (
                     (EApplication ((EIdentifier "( :: )"),
                        (EIdentifier "fun1"))),
                     (EApplication (
                        (EApplication ((EIdentifier "( :: )"),
                           (EIdentifier "fun2"))),
                        (EConstant CNil)))
                     ))
                  )),
               (EConstant CNil))),
            (((('b * int) -> (int * 'a)) list) list)))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let ((x : int) : int) = 5
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PConstraint ((PConstraint ((PIdentifier "x"), int)), int)),
         (EConstant (CInt 5))))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let () = ()
  > EOF
  [(DSingleLet (NotRec, (DLet ((PConstant CUnit), (EConstant CUnit)))))]

  $ dune exec parser_demo << EOF
  > let recfib = fun n -> if n=1 then 1 else fib (n-1)
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PIdentifier "recfib"),
         (EFunction ((PIdentifier "n"),
            (EIfThenElse (
               (EApplication (
                  (EApplication ((EIdentifier "( = )"), (EIdentifier "n"))),
                  (EConstant (CInt 1)))),
               (EConstant (CInt 1)),
               (EApplication ((EIdentifier "fib"),
                  (EApplication (
                     (EApplication ((EIdentifier "( - )"), (EIdentifier "n"))),
                     (EConstant (CInt 1))))
                  ))
               ))
            ))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let fibo = fun n -> let rec fiboCPS = fun n acc -> match n with
  >   | 0 -> acc 0
  >   | 1 -> acc 1
  >   | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
  > in
  > fiboCPS n (fun x -> x)
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PIdentifier "fibo"),
         (EFunction ((PIdentifier "n"),
            (ELetIn (Rec, (PIdentifier "fiboCPS"),
               (EFunction ((PIdentifier "n"),
                  (EFunction ((PIdentifier "acc"),
                     (EMatch ((EIdentifier "n"),
                        [((PConstant (CInt 0)),
                          (EApplication ((EIdentifier "acc"),
                             (EConstant (CInt 0)))));
                          ((PConstant (CInt 1)),
                           (EApplication ((EIdentifier "acc"),
                              (EConstant (CInt 1)))));
                          (PWildCard,
                           (EApplication (
                              (EApplication ((EIdentifier "fiboCPS"),
                                 (EApplication (
                                    (EApplication ((EIdentifier "( - )"),
                                       (EIdentifier "n"))),
                                    (EConstant (CInt 1))))
                                 )),
                              (EFunction ((PIdentifier "x"),
                                 (EApplication (
                                    (EApplication ((EIdentifier "fiboCPS"),
                                       (EApplication (
                                          (EApplication ((EIdentifier "( - )"),
                                             (EIdentifier "n"))),
                                          (EConstant (CInt 2))))
                                       )),
                                    (EFunction ((PIdentifier "y"),
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
                  (EFunction ((PIdentifier "x"), (EIdentifier "x")))))
               ))
            ))
         ))
      ))
    ]

  $ dune exec parser_demo << EOF
  > let test a1 a2 a3 = a1 + a2 + a3
  > EOF
  [(DSingleLet (NotRec,
      (DLet ((PIdentifier "test"),
         (EFunction ((PIdentifier "a1"),
            (EFunction ((PIdentifier "a2"),
               (EFunction ((PIdentifier "a3"),
                  (EApplication (
                     (EApplication ((EIdentifier "( + )"),
                        (EApplication (
                           (EApplication ((EIdentifier "( + )"),
                              (EIdentifier "a1"))),
                           (EIdentifier "a2")))
                        )),
                     (EIdentifier "a3")))
                  ))
               ))
            ))
         ))
      ))
    ]

  $ dune exec parser << EOF
  > let f ((x : int) : int) ((y : int) : int) = x + y;;
  > EOF
  [(SValue (Nonrec,
      [((PVar "f"),
        (EFun ((PConstraint ((PConstraint ((PVar "x"), AInt)), AInt)),
           (EFun ((PConstraint ((PConstraint ((PVar "y"), AInt)), AInt)),
              (EApply ((EApply ((EVar "+"), (EVar "x"))), (EVar "y")))))
           )))
        ]
      ))
    ]
  $ dune exec parser << EOF
  > let rec fold_left f acc l =
  >    match l with
  >    | [] -> acc
  >    | h :: tl -> fold_left f (f acc h) tl
  > ;;
  > EOF
  [(SValue (Rec,
      [((PVar "fold_left"),
        (EFun ((PVar "f"),
           (EFun ((PVar "acc"),
              (EFun ((PVar "l"),
                 (EMatch ((EVar "l"),
                    [((PConst CNil), (EVar "acc"));
                      ((PCons ((PVar "h"), (PVar "tl"))),
                       (EApply (
                          (EApply ((EApply ((EVar "fold_left"), (EVar "f"))),
                             (EApply ((EApply ((EVar "f"), (EVar "acc"))),
                                (EVar "h")))
                             )),
                          (EVar "tl"))))
                      ]
                    ))
                 ))
              ))
           )))
        ]
      ))
    ]
  $ dune exec parser << EOF
  > let a b c = a && b || b && c
  > EOF
  [(SValue (Nonrec,
      [((PVar "a"),
        (EFun ((PVar "b"),
           (EFun ((PVar "c"),
              (EApply (
                 (EApply ((EVar "||"),
                    (EApply ((EApply ((EVar "&&"), (EVar "a"))), (EVar "b"))))),
                 (EApply ((EApply ((EVar "&&"), (EVar "b"))), (EVar "c")))))
              ))
           )))
        ]
      ))
    ]
  $ dune exec parser << EOF
  >      let f x y = x + y
  >      and
  >      g x y = x - y
  >      and
  >      h x y = x * y
  >      in
  >      f 2 (g 1 (h 2 3))
  > ;;
  > EOF
  [(SValue (Nonrec,
      [((PVar "f"),
        (EFun ((PVar "x"),
           (EFun ((PVar "y"),
              (EApply ((EApply ((EVar "+"), (EVar "x"))), (EVar "y")))))
           )));
        ((PVar "g"),
         (EFun ((PVar "x"),
            (EFun ((PVar "y"),
               (EApply ((EApply ((EVar "-"), (EVar "x"))), (EVar "y")))))
            )));
        ((PVar "h"),
         (EFun ((PVar "x"),
            (EFun ((PVar "y"),
               (EApply ((EApply ((EVar "*"), (EVar "x"))), (EVar "y")))))
            )))
        ]
      ))
    ]
  $ dune exec parser << EOF
  > let rec f x = x + 1 and g x = x + 1
  > ;;
  > EOF
  [(SValue (Rec,
      [((PVar "f"),
        (EFun ((PVar "x"),
           (EApply ((EApply ((EVar "+"), (EVar "x"))), (EConst (CInt 1)))))));
        ((PVar "g"),
         (EFun ((PVar "x"),
            (EApply ((EApply ((EVar "+"), (EVar "x"))), (EConst (CInt 1)))))))
        ]
      ))
    ]
  $ dune exec parser << EOF
  > let (x : int) = 3;;
  > EOF
  [(SValue (Nonrec, [((PConstraint ((PVar "x"), AInt)), (EConst (CInt 3)))]))]
  $ dune exec parser << EOF
  > let (x : (int -> int) list) = f;;
  > EOF
  [(SValue (Nonrec,
      [((PConstraint ((PVar "x"), (AList (AFun (AInt, AInt))))), (EVar "f"))]))
    ]
  $ dune exec parser << EOF
  > let (x : int -> int -> (int -> int)) = f;;
  > EOF
  [(SValue (Nonrec,
      [((PConstraint ((PVar "x"),
           (AFun (AInt, (AFun (AInt, (AFun (AInt, AInt)))))))),
        (EVar "f"))]
      ))
    ]
  $ dune exec parser << EOF
  > let rec fix f x = f (fix f) x;;
  > let fac_ fac n = if n = 1 then 1 else n * fac (n - 1);;
  > EOF
  [(SValue (Rec,
      [((PVar "fix"),
        (EFun ((PVar "f"),
           (EFun ((PVar "x"),
              (EApply (
                 (EApply ((EVar "f"), (EApply ((EVar "fix"), (EVar "f"))))),
                 (EVar "x")))
              ))
           )))
        ]
      ));
    (SValue (Nonrec,
       [((PVar "fac_"),
         (EFun ((PVar "fac"),
            (EFun ((PVar "n"),
               (EIf (
                  (EApply ((EApply ((EVar "="), (EVar "n"))), (EConst (CInt 1))
                     )),
                  (EConst (CInt 1)),
                  (EApply ((EApply ((EVar "*"), (EVar "n"))),
                     (EApply ((EVar "fac"),
                        (EApply ((EApply ((EVar "-"), (EVar "n"))),
                           (EConst (CInt 1))))
                        ))
                     ))
                  ))
               ))
            )))
         ]
       ))
    ]
  $ dune exec parser << EOF
  > let n = fun y -> if y > 0 then 1 else 2
  > EOF
  [(SValue (Nonrec,
      [((PVar "n"),
        (EFun ((PVar "y"),
           (EIf (
              (EApply ((EApply ((EVar ">"), (EVar "y"))), (EConst (CInt 0)))),
              (EConst (CInt 1)), (EConst (CInt 2))))
           )))
        ]
      ))
    ]
  $ dune exec parser << EOF
  > let rec fac n = if n < 2 then 1 else n * fac(n - 1);;
  > EOF
  [(SValue (Rec,
      [((PVar "fac"),
        (EFun ((PVar "n"),
           (EIf (
              (EApply ((EApply ((EVar "<"), (EVar "n"))), (EConst (CInt 2)))),
              (EConst (CInt 1)),
              (EApply ((EApply ((EVar "*"), (EVar "n"))),
                 (EApply ((EVar "fac"),
                    (EApply ((EApply ((EVar "-"), (EVar "n"))),
                       (EConst (CInt 1))))
                    ))
                 ))
              ))
           )))
        ]
      ))
    ]
  $ dune exec parser << EOF
  > let f = let g x = x + 1 in g;;
  > let rec len l =
  >   match l with
  >   | [] -> 0
  >   | _ :: xs -> 1 + len xs
  > ;;
  > EOF
  [(SValue (Nonrec,
      [((PVar "f"),
        (ELet (Nonrec,
           ((PVar "g"),
            (EFun ((PVar "x"),
               (EApply ((EApply ((EVar "+"), (EVar "x"))), (EConst (CInt 1))))
               ))),
           (EVar "g"))))
        ]
      ));
    (SValue (Rec,
       [((PVar "len"),
         (EFun ((PVar "l"),
            (EMatch ((EVar "l"),
               [((PConst CNil), (EConst (CInt 0)));
                 ((PCons (PAny, (PVar "xs"))),
                  (EApply ((EApply ((EVar "+"), (EConst (CInt 1)))),
                     (EApply ((EVar "len"), (EVar "xs"))))))
                 ]
               ))
            )))
         ]
       ))
    ]
  $ dune exec parser << EOF
  > let f = (fun x -> x + 1) 123 in f;;
  > let x, y, z = (1, 2, 3);;
  > EOF
  [(SEval
      (ELet (Nonrec,
         ((PVar "f"),
          (EApply (
             (EFun ((PVar "x"),
                (EApply ((EApply ((EVar "+"), (EVar "x"))), (EConst (CInt 1))))
                )),
             (EConst (CInt 123))))),
         (EVar "f"))));
    (SValue (Nonrec,
       [((PTuple [(PVar "x"); (PVar "y"); (PVar "z")]),
         (ETuple [(EConst (CInt 1)); (EConst (CInt 2)); (EConst (CInt 3))]))]
       ))
    ]
  $ dune exec parser << EOF
  > let addi = fun f g x -> (f x (g x: bool) : int)
  > EOF
  [(SValue (Nonrec,
      [((PVar "addi"),
        (EFun ((PVar "f"),
           (EFun ((PVar "g"),
              (EFun ((PVar "x"),
                 (EConstraint (
                    (EApply ((EApply ((EVar "f"), (EVar "x"))),
                       (EConstraint ((EApply ((EVar "g"), (EVar "x"))), ABool))
                       )),
                    AInt))
                 ))
              ))
           )))
        ]
      ))
    ]

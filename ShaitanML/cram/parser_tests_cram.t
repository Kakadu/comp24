  $ dune exec test << EOF
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
  $ dune exec test << EOF
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

(** Copyright 2023-2024, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Shaitanml_lib
open Parser

let%expect_test _ =
  test_parse
    {|
   let rec fold_left f acc l =
      match l with
      | [] -> acc
      | h :: tl -> fold_left f (f acc h) tl
   ;;
      |};
  [%expect
    {|
    [(SValue (Rec,
        [((PVar ("fold_left", None)),
          (EFun ((PVar ("f", None)),
             (EFun ((PVar ("acc", None)),
                (EFun ((PVar ("l", None)),
                   (EMatch ((EVar "l"),
                      [((PConst CNil), (EVar "acc"));
                        ((PCons ((PVar ("h", None)), (PVar ("tl", None)))),
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
      ] |}]
;;

let%expect_test _ =
  test_parse {|
      let a b c = a && b || b && c
      |};
  [%expect
    {|
    [(SValue (Nonrec,
        [((PVar ("a", None)),
          (EFun ((PVar ("b", None)),
             (EFun ((PVar ("c", None)),
                (EApply (
                   (EApply ((EVar "||"),
                      (EApply ((EApply ((EVar "&&"), (EVar "a"))), (EVar "b"))))),
                   (EApply ((EApply ((EVar "&&"), (EVar "b"))), (EVar "c")))))
                ))
             )))
          ]
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse
    {|
      let f x y = x + y
      and
      g x y = x - y
      and
      h x y = x * y
      in
      f 2 (g 1 (h 2 3))
;;
      |};
  [%expect
    {|
    [(SEval
        (ELet (Nonrec,
           [((PVar ("f", None)),
             (EFun ((PVar ("x", None)),
                (EFun ((PVar ("y", None)),
                   (EApply ((EApply ((EVar "+"), (EVar "x"))), (EVar "y")))))
                )));
             ((PVar ("g", None)),
              (EFun ((PVar ("x", None)),
                 (EFun ((PVar ("y", None)),
                    (EApply ((EApply ((EVar "-"), (EVar "x"))), (EVar "y")))))
                 )));
             ((PVar ("h", None)),
              (EFun ((PVar ("x", None)),
                 (EFun ((PVar ("y", None)),
                    (EApply ((EApply ((EVar "*"), (EVar "x"))), (EVar "y")))))
                 )))
             ],
           (EApply ((EApply ((EVar "f"), (EConst (CInt 2)))),
              (EApply ((EApply ((EVar "g"), (EConst (CInt 1)))),
                 (EApply ((EApply ((EVar "h"), (EConst (CInt 2)))),
                    (EConst (CInt 3))))
                 ))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
      let rec f x = x + 1 and g x = x + 1
;;
      |};
  [%expect
    {|
    [(SValue (Rec,
        [((PVar ("f", None)),
          (EFun ((PVar ("x", None)),
             (EApply ((EApply ((EVar "+"), (EVar "x"))), (EConst (CInt 1)))))));
          ((PVar ("g", None)),
           (EFun ((PVar ("x", None)),
              (EApply ((EApply ((EVar "+"), (EVar "x"))), (EConst (CInt 1)))))))
          ]
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
      let (x : int) = 3;;
      |};
  [%expect
    {|
    [(SValue (Nonrec, [((PVar ("x", (Some AInt))), (EConst (CInt 3)))]))] |}]
;;

let%expect_test _ =
  test_parse {|
      let (x : (int -> int) list) = f;;
      |};
  [%expect
    {|
    [(SValue (Nonrec,
        [((PVar ("x", (Some (AList (AFun (AInt, AInt)))))), (EVar "f"))]))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
      let (x : int -> int -> (int -> int)) = f;;
      |};
  [%expect
    {|
    [(SValue (Nonrec,
        [((PVar ("x", (Some (AFun (AInt, (AFun (AInt, (AFun (AInt, AInt))))))))),
          (EVar "f"))]
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse
    {|
        let rec fix f x = f (fix f) x;;
        let fac_ fac n = if n = 1 then 1 else n * fac (n - 1);;
      |};
  [%expect
    {|
    [(SValue (Rec,
        [((PVar ("fix", None)),
          (EFun ((PVar ("f", None)),
             (EFun ((PVar ("x", None)),
                (EApply (
                   (EApply ((EVar "f"), (EApply ((EVar "fix"), (EVar "f"))))),
                   (EVar "x")))
                ))
             )))
          ]
        ));
      (SValue (Nonrec,
         [((PVar ("fac_", None)),
           (EFun ((PVar ("fac", None)),
              (EFun ((PVar ("n", None)),
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
      ] |}]
;;

let%expect_test _ =
  test_parse {|
      let n = fun y -> if y > 0 then 1 else 2
    |};
  [%expect
    {|
      [(SValue (Nonrec,
          [((PVar ("n", None)),
            (EFun ((PVar ("y", None)),
               (EIf (
                  (EApply ((EApply ((EVar ">"), (EVar "y"))), (EConst (CInt 0)))),
                  (EConst (CInt 1)), (EConst (CInt 2))))
               )))
            ]
          ))
        ] |}]
;;

let%expect_test _ =
  test_parse {|
    let rec fac n = if n < 2 then 1 else n * fac(n - 1);;
    |};
  [%expect
    {|
    [(SValue (Rec,
        [((PVar ("fac", None)),
          (EFun ((PVar ("n", None)),
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
      ] |}]
;;

let%expect_test _ =
  test_parse
    {|
    let f = let g x = x + 1 in g;;
    let rec len l =
      match l with
      | [] -> 0
      | _ :: xs -> 1 + len xs
    ;;
    |};
  [%expect
    {|
      [(SValue (Nonrec,
          [((PVar ("f", None)),
            (ELet (Nonrec,
               [((PVar ("g", None)),
                 (EFun ((PVar ("x", None)),
                    (EApply ((EApply ((EVar "+"), (EVar "x"))), (EConst (CInt 1))))
                    )))
                 ],
               (EVar "g"))))
            ]
          ));
        (SValue (Rec,
           [((PVar ("len", None)),
             (EFun ((PVar ("l", None)),
                (EMatch ((EVar "l"),
                   [((PConst CNil), (EConst (CInt 0)));
                     ((PCons (PAny, (PVar ("xs", None)))),
                      (EApply ((EApply ((EVar "+"), (EConst (CInt 1)))),
                         (EApply ((EVar "len"), (EVar "xs"))))))
                     ]
                   ))
                )))
             ]
           ))
        ] |}]
;;

let%expect_test _ =
  test_parse
    {|

    let f = (fun x -> x + 1) 123 in f;;
    let x, y, z = (1, 2, 3);;

    |};
  [%expect
    {|
      [(SEval
          (ELet (Nonrec,
             [((PVar ("f", None)),
               (EApply (
                  (EFun ((PVar ("x", None)),
                     (EApply ((EApply ((EVar "+"), (EVar "x"))), (EConst (CInt 1))
                        ))
                     )),
                  (EConst (CInt 123)))))
               ],
             (EVar "f"))));
        (SValue (Nonrec,
           [((PTuple [(PVar ("x", None)); (PVar ("y", None)); (PVar ("z", None))]),
             (ETuple [(EConst (CInt 1)); (EConst (CInt 2)); (EConst (CInt 3))]))]
           ))
        ] |}]
;;

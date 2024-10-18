(** Copyright 2023-2024, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Shaitanml_lib
open Parser
let%expect_test _ =
  test_parse {|
      let (x : int) = 3;;
      |};
  [%expect
    {|
    [(SValue (Nonrec, ((PVar ("x", (Some AInt))), (EConst (CInt 3)))))] |}]
;;

let%expect_test _ =
  test_parse {|
      let (x : (int -> int) list) = f;;
      |};
  [%expect
    {|
    [(SValue (Nonrec,
        ((PVar ("x", (Some (AList (AFun (AInt, AInt)))))), (EVar "f"))))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
      let (x : int -> int -> (int -> int)) = f;;
      |};
  [%expect
    {|
    [(SValue (Nonrec,
        ((PVar ("x", (Some (AFun (AInt, (AFun (AInt, (AFun (AInt, AInt))))))))),
         (EVar "f"))
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
        ((PVar ("fix", None)),
         (EFun ((PVar ("f", None)),
            (EFun ((PVar ("x", None)),
               (EApply (
                  (EApply ((EVar "f"), (EApply ((EVar "fix"), (EVar "f"))))),
                  (EVar "x")))
               ))
            )))
        ));
      (SValue (Nonrec,
         ((PVar ("fac_", None)),
          (EFun ((PVar ("fac", None)),
             (EFun ((PVar ("n", None)),
                (EIf ((EBin_op (Eq, (EVar "n"), (EConst (CInt 1)))),
                   (EConst (CInt 1)),
                   (EBin_op (Mul, (EVar "n"),
                      (EApply ((EVar "fac"),
                         (EBin_op (Sub, (EVar "n"), (EConst (CInt 1))))))
                      ))
                   ))
                ))
             )))
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
          ((PVar ("n", None)),
           (EFun ((PVar ("y", None)),
              (EIf ((EBin_op (Gt, (EVar "y"), (EConst (CInt 0)))),
                 (EConst (CInt 1)), (EConst (CInt 2))))
              )))
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
        ((PVar ("fac", None)),
         (EFun ((PVar ("n", None)),
            (EIf ((EBin_op (Lt, (EVar "n"), (EConst (CInt 2)))),
               (EConst (CInt 1)),
               (EBin_op (Mul, (EVar "n"),
                  (EApply ((EVar "fac"),
                     (EBin_op (Sub, (EVar "n"), (EConst (CInt 1))))))
                  ))
               ))
            )))
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
          ((PVar ("f", None)),
           (ELet (Nonrec,
              ((PVar ("g", None)),
               (EFun ((PVar ("x", None)),
                  (EBin_op (Add, (EVar "x"), (EConst (CInt 1))))))),
              (EVar "g"))))
          ));
        (SValue (Rec,
           ((PVar ("len", None)),
            (EFun ((PVar ("l", None)),
               (EMatch ((EVar "l"),
                  [((PConst CNil), (EConst (CInt 0)));
                    ((PCons (PAny, (PVar ("xs", None)))),
                     (EBin_op (Add, (EConst (CInt 1)),
                        (EApply ((EVar "len"), (EVar "xs"))))))
                    ]
                  ))
               )))
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
             ((PVar ("f", None)),
              (EApply (
                 (EFun ((PVar ("x", None)),
                    (EBin_op (Add, (EVar "x"), (EConst (CInt 1)))))),
                 (EConst (CInt 123))))),
             (EVar "f"))));
        (SValue (Nonrec,
           ((PTuple [(PVar ("x", None)); (PVar ("y", None)); (PVar ("z", None))]),
            (ETuple [(EConst (CInt 1)); (EConst (CInt 2)); (EConst (CInt 3))]))
           ))
        ] |}]
;;

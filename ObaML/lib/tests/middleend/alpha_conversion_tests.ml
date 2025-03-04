(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format

let parse_alpha_convert_and_print_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    let structure, _ =
      Alpha_conversion.run_alpha_conversion parse_result
    in
    printf "%a" Ast.pp_structure structure
  | Error _ -> printf "Syntax error"
;;

let parse_inner_alpha_and_print str = parse_alpha_convert_and_print_result str

let%expect_test "" =
  parse_inner_alpha_and_print {| let a = 4;; 
let b = 4;;|};
  [%expect {|
    [(SILet (Nonrecursive, [((PVar (Id "a")), (EConst (CInt 4)))]));
      (SILet (Nonrecursive, [((PVar (Id "b")), (EConst (CInt 4)))]))] |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {| 
  let a = 4;; 
  let (a, b) = (5, 6) and c = a;; |};
  [%expect
    {|
    [(SILet (Nonrecursive, [((PVar (Id "a")), (EConst (CInt 4)))]));
      (SILet (Nonrecursive,
         [((PTuple [(PVar (Id "oba0")); (PVar (Id "b"))]),
           (ETuple [(EConst (CInt 5)); (EConst (CInt 6))]));
           ((PVar (Id "c")), (EVar (Id "a")))]
         ))
      ] |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {|let map p = let (a,b) = p in a + b |};
  [%expect
    {|       
  [(SILet (Nonrecursive,
      [((PVar (Id "map")),
        (EFun ([(PVar (Id "p"))],
           (ELet (Nonrecursive,
              ((PTuple [(PVar (Id "a")); (PVar (Id "b"))]), (EVar (Id "p"))),
              (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "a")))),
                 (EVar (Id "b"))))
              ))
           )))
        ]
      ))
    ] |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print
    {| let f x y = 
      let x z = y + z in 
      let y z = x 1 + z in
      x 1 + y 2;; |};
  [%expect
    {|
      [(SILet (Nonrecursive,
          [((PVar (Id "f")),
            (EFun ([(PVar (Id "x")); (PVar (Id "y"))],
               (ELet (Nonrecursive,
                  ((PVar (Id "oba0")),
                   (EFun ([(PVar (Id "z"))],
                      (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "y")))),
                         (EVar (Id "z"))))
                      ))),
                  (ELet (Nonrecursive,
                     ((PVar (Id "oba1")),
                      (EFun ([(PVar (Id "oba2"))],
                         (EApp (
                            (EApp ((EVar (Id "( + )")),
                               (EApp ((EVar (Id "oba0")), (EConst (CInt 1)))))),
                            (EVar (Id "oba2"))))
                         ))),
                     (EApp (
                        (EApp ((EVar (Id "( + )")),
                           (EApp ((EVar (Id "oba0")), (EConst (CInt 1)))))),
                        (EApp ((EVar (Id "oba1")), (EConst (CInt 2))))))
                     ))
                  ))
               )))
            ]
          ))
        ] |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {|fun x () x -> x () |};
  [%expect {|       
  [(SIExpr
      (EFun ([(PVar (Id "x")); (PConst CUnit); (PVar (Id "oba0"))],
         (EApp ((EVar (Id "oba0")), (EConst CUnit))))))
    ] |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {| let rec a a = a;; |};
  [%expect {|       
  [(SILet (Recursive,
      [((PVar (Id "a")), (EFun ([(PVar (Id "oba0"))], (EVar (Id "oba0")))))]))
    ] |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {| let rec oba0 x x = x;; |};
  [%expect {|       
  [(SILet (Recursive,
      [((PVar (Id "oba0")),
        (EFun ([(PVar (Id "x")); (PVar (Id "oba1"))], (EVar (Id "oba1")))))]
      ))
    ] |}]
;;

let%expect_test "006partial2" =
  parse_inner_alpha_and_print
    {|
    let foo a b c =
      let () = print_int a in
      let () = print_int b in
      let () = print_int c in
      a + b * c
    
    let main =
      let foo = foo 1 in
      let foo = foo 2 in
      let foo = foo 3 in
      let () = print_int foo in
      0
|};
  [%expect
    {|
    [(SILet (Nonrecursive,
        [((PVar (Id "foo")),
          (EFun ([(PVar (Id "a")); (PVar (Id "b")); (PVar (Id "c"))],
             (ELet (Nonrecursive,
                ((PConst CUnit),
                 (EApp ((EVar (Id "print_int")), (EVar (Id "a"))))),
                (ELet (Nonrecursive,
                   ((PConst CUnit),
                    (EApp ((EVar (Id "print_int")), (EVar (Id "b"))))),
                   (ELet (Nonrecursive,
                      ((PConst CUnit),
                       (EApp ((EVar (Id "print_int")), (EVar (Id "c"))))),
                      (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "a")))),
                         (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "b")))),
                            (EVar (Id "c"))))
                         ))
                      ))
                   ))
                ))
             )))
          ]
        ));
      (SILet (Nonrecursive,
         [((PVar (Id "main")),
           (ELet (Nonrecursive,
              ((PVar (Id "oba0")), (EApp ((EVar (Id "foo")), (EConst (CInt 1))))),
              (ELet (Nonrecursive,
                 ((PVar (Id "oba1")),
                  (EApp ((EVar (Id "oba0")), (EConst (CInt 2))))),
                 (ELet (Nonrecursive,
                    ((PVar (Id "oba2")),
                     (EApp ((EVar (Id "oba1")), (EConst (CInt 3))))),
                    (ELet (Nonrecursive,
                       ((PConst CUnit),
                        (EApp ((EVar (Id "print_int")), (EVar (Id "oba2"))))),
                       (EConst (CInt 0))))
                    ))
                 ))
              )))
           ]
         ))
      ] |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print {|
    let a = 5

    let a = let a = a in a;;
  |};
  [%expect {|
      [(SILet (Nonrecursive, [((PVar (Id "a")), (EConst (CInt 5)))]));
        (SILet (Nonrecursive,
           [((PVar (Id "oba0")),
             (ELet (Nonrecursive, ((PVar (Id "oba1")), (EVar (Id "a"))),
                (EVar (Id "oba1")))))
             ]
           ))
        ] |}]
;;

let%expect_test "" =
  parse_inner_alpha_and_print
    {|
    let main = 5

    let main =
        let foo = (fun x -> x) in
        let foo = foo 2 in
        let () = print_int foo in
        0
  |};
  [%expect
    {|
      [(SILet (Nonrecursive, [((PVar (Id "main")), (EConst (CInt 5)))]));
        (SILet (Nonrecursive,
           [((PVar (Id "oba0")),
             (ELet (Nonrecursive,
                ((PVar (Id "foo")), (EFun ([(PVar (Id "x"))], (EVar (Id "x"))))),
                (ELet (Nonrecursive,
                   ((PVar (Id "oba1")),
                    (EApp ((EVar (Id "foo")), (EConst (CInt 2))))),
                   (ELet (Nonrecursive,
                      ((PConst CUnit),
                       (EApp ((EVar (Id "print_int")), (EVar (Id "oba1"))))),
                      (EConst (CInt 0))))
                   ))
                )))
             ]
           ))
        ] |}]
;;

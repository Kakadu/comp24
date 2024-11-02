(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParsingTests = struct
  let parse_test s =
    match Parser.parse_program s with
    | Ok actual -> Format.printf "%a\n" Ast.pp_prog actual
    | Error err -> Format.printf "%s\n" err
  ;;

  let%expect_test _ =
    parse_test {| let a = 3 |};
    [%expect {| [(DLet (Not_recursive, "a", (EConst (CInt 3)), None))] |}]
  ;;

  let%expect_test _ =
    parse_test {| let a : int = 2 ++ 2 ** 2 |};
    [%expect
      {|
      [(DLet (Not_recursive, "a",
          (EApp ((EApp ((EId (IOBinOp "++")), (EConst (CInt 2)))),
             (EApp ((EApp ((EId (IOBinOp "**")), (EConst (CInt 2)))),
                (EConst (CInt 2))))
             )),
          (Some (TGround GInt))))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test {| let a = (2 ++ 2) ** 2 |};
    [%expect
      {|
      [(DLet (Not_recursive, "a",
          (EApp (
             (EApp ((EId (IOBinOp "**")),
                (EApp ((EApp ((EId (IOBinOp "++")), (EConst (CInt 2)))),
                   (EConst (CInt 2))))
                )),
             (EConst (CInt 2)))),
          None))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test {| let a : int = let f x = x in 13 |};
    [%expect
      {| 
      [(DLet (Not_recursive, "a",
          (EClsr (
             (DLet (Not_recursive, "f",
                (EFun (((PId "x"), None), (EId (IOIdent "x")))), None)),
             (EConst (CInt 13)))),
          (Some (TGround GInt))))
        ]
      |}]
  ;;

  let%expect_test _ =
    parse_test {| let ( += ) = x -$ 1 |};
    [%expect
      {| 
      [(DLet (Not_recursive, "+=",
          (EApp ((EApp ((EId (IOBinOp "-$")), (EId (IOIdent "x")))),
             (EConst (CInt 1)))),
          None))
        ]
      |}]
  ;;

  let%expect_test _ =
    parse_test {| let a = x + y |};
    [%expect
      {| 
      [(DLet (Not_recursive, "a",
          (EApp ((EApp ((EId (IOBinOp "+")), (EId (IOIdent "x")))),
             (EId (IOIdent "y")))),
          None))
        ]
      |}]
  ;;

  let%expect_test _ =
    parse_test {| let a = (+) x y |};
    [%expect
      {| 
      [(DLet (Not_recursive, "a",
          (EApp ((EApp ((EId (IOBinOp "+")), (EId (IOIdent "x")))),
             (EId (IOIdent "y")))),
          None))
        ]
      |}]
  ;;

  let%expect_test _ =
    parse_test {| let ( += ) a b = (~-) 1 |};
    [%expect
      {| 
      [(DLet (Not_recursive, "+=",
          (EFun (((PId "a"), None),
             (EFun (((PId "b"), None),
                (EApp ((EId (IOUnOp "~-")), (EConst (CInt 1))))))
             )),
          None))
        ]|}]
  ;;

  let%expect_test _ =
    parse_test {| let f (lst: (int -> int)) = g |};
    [%expect
      {| 
      [(DLet (Not_recursive, "f",
          (EFun (((PId "lst"), (Some (TArr ((TGround GInt), (TGround GInt))))),
             (EId (IOIdent "g")))),
          None))
        ]
      |}]
  ;;

  let%expect_test _ =
    parse_test {| let a = let f x = x + 1 in let b = 5 in f (b + 1) |};
    [%expect
      {| 
      [(DLet (Not_recursive, "a",
          (EClsr (
             (DLet (Not_recursive, "f",
                (EFun (((PId "x"), None),
                   (EApp ((EApp ((EId (IOBinOp "+")), (EId (IOIdent "x")))),
                      (EConst (CInt 1))))
                   )),
                None)),
             (EClsr ((DLet (Not_recursive, "b", (EConst (CInt 5)), None)),
                (EApp ((EId (IOIdent "f")),
                   (EApp ((EApp ((EId (IOBinOp "+")), (EId (IOIdent "b")))),
                      (EConst (CInt 1))))
                   ))
                ))
             )),
          None))
        ]
    |}]
  ;;

  let%expect_test _ =
    parse_test "let a =([3;3], [4;4]) :: [([5;5],[6;6]);([7;7],[8;8])]";
    [%expect
      {| 
    [(DLet (Not_recursive, "a",
        (EList (
           (ETuple
              [(EList ((EConst (CInt 3)),
                  (EList ((EConst (CInt 3)), (EConst CNil)))));
                (EList ((EConst (CInt 4)),
                   (EList ((EConst (CInt 4)), (EConst CNil)))))
                ]),
           (EList (
              (ETuple
                 [(EList ((EConst (CInt 5)),
                     (EList ((EConst (CInt 5)), (EConst CNil)))));
                   (EList ((EConst (CInt 6)),
                      (EList ((EConst (CInt 6)), (EConst CNil)))))
                   ]),
              (EList (
                 (ETuple
                    [(EList ((EConst (CInt 7)),
                        (EList ((EConst (CInt 7)), (EConst CNil)))));
                      (EList ((EConst (CInt 8)),
                         (EList ((EConst (CInt 8)), (EConst CNil)))))
                      ]),
                 (EConst CNil)))
              ))
           )),
        None))
      ] |}]
  ;;

  let%expect_test _ =
    parse_test
      {|
    let a = 
      match l with 
      | (hd, tl) -> hd 
      | _ -> 0
    |};
    [%expect
      {|
      [(DLet (Not_recursive, "a",
          (EMatch ((EId (IOIdent "l")),
             [(((PTuple [((PId "hd"), None); ((PId "tl"), None)]), None),
               (EId (IOIdent "hd"))); (((PId "_"), None), (EConst (CInt 0)))]
             )),
          None))
        ]|}]
  ;;

  let%expect_test _ =
    parse_test {| let f (lst: ('b list)) = lst |};
    [%expect
      {|
      [(DLet (Not_recursive, "f",
          (EFun (((PId "lst"), (Some (TList (TVar "b")))), (EId (IOIdent "lst")))),
          None))
        ]|}]
  ;;

  let%expect_test _ =
    parse_test "let a = - 3";
    [%expect
      {|
        [(DLet (Not_recursive, "a", (EApp ((EId (IOUnOp "-")), (EConst (CInt 3)))),
            None))
          ] |}]
  ;;

  let%expect_test _ =
    parse_test "let a = 3 -> 3";
    [%expect {| Syntax error |}]
  ;;

  let%expect_test _ =
    parse_test "let a = f a + f b";
    [%expect
      {|
      [(DLet (Not_recursive, "a",
          (EApp (
             (EApp ((EId (IOBinOp "+")),
                (EApp ((EId (IOIdent "f")), (EId (IOIdent "a")))))),
             (EApp ((EId (IOIdent "f")), (EId (IOIdent "b")))))),
          None))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test "let a = - f 3 - f 4";
    [%expect
      {|
      [(DLet (Not_recursive, "a",
          (EApp (
             (EApp ((EId (IOBinOp "-")),
                (EApp ((EId (IOUnOp "-")),
                   (EApp ((EId (IOIdent "f")), (EConst (CInt 3))))))
                )),
             (EApp ((EId (IOIdent "f")), (EConst (CInt 4)))))),
          None))
        ]|}]
  ;;

  let%expect_test _ =
    parse_test "let f (hd::tl) = 3";
    [%expect
      {|
      [(DLet (Not_recursive, "f",
          (EFun (((PList (((PId "hd"), None), ((PId "tl"), None))), None),
             (EConst (CInt 3)))),
          None))
        ]|}]
  ;;

  let%expect_test _ =
    parse_test "let f (x,y) = x + y";
    [%expect
      {|
      [(DLet (Not_recursive, "f",
          (EFun (((PTuple [((PId "x"), None); ((PId "y"), None)]), None),
             (EApp ((EApp ((EId (IOBinOp "+")), (EId (IOIdent "x")))),
                (EId (IOIdent "y"))))
             )),
          None))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test "let f ((x: 'a),y) =  x * y";
    [%expect
      {|
      [(DLet (Not_recursive, "f",
          (EFun (
             ((PTuple [((PId "x"), (Some (TVar "a"))); ((PId "y"), None)]), None),
             (EApp ((EApp ((EId (IOBinOp "*")), (EId (IOIdent "x")))),
                (EId (IOIdent "y"))))
             )),
          None))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test "let f ((g : (int -> int)),y) = g y + g (y + 1)";
    [%expect
      {|
      [(DLet (Not_recursive, "f",
          (EFun (
             ((PTuple
                 [((PId "g"), (Some (TArr ((TGround GInt), (TGround GInt)))));
                   ((PId "y"), None)]),
              None),
             (EApp (
                (EApp ((EId (IOBinOp "+")),
                   (EApp ((EId (IOIdent "g")), (EId (IOIdent "y")))))),
                (EApp ((EId (IOIdent "g")),
                   (EApp ((EApp ((EId (IOBinOp "+")), (EId (IOIdent "y")))),
                      (EConst (CInt 1))))
                   ))
                ))
             )),
          None))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test "let f (x :: (x, y)) = 3";
    [%expect
      {|
      [(DLet (Not_recursive, "f",
          (EFun (
             ((PList (((PId "x"), None),
                 ((PTuple [((PId "x"), None); ((PId "y"), None)]), None))),
              None),
             (EConst (CInt 3)))),
          None))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test
      {| 
      let blst : int=
        let rec f x = if x > 0 then g (x - 1) else 1
        and g x = if x > 0 then f (x - 1) + g (x - 2) else 1 in
        f 10
      |};
    [%expect
      {|
      [(DLet (Not_recursive, "blst",
          (EClsr (
             (DLetMut (Recursive,
                [("f",
                  (EFun (((PId "x"), None),
                     (EIf (
                        (EApp ((EApp ((EId (IOBinOp ">")), (EId (IOIdent "x")))),
                           (EConst (CInt 0)))),
                        (EApp ((EId (IOIdent "g")),
                           (EApp (
                              (EApp ((EId (IOBinOp "-")), (EId (IOIdent "x")))),
                              (EConst (CInt 1))))
                           )),
                        (EConst (CInt 1))))
                     )),
                  None);
                  ("g",
                   (EFun (((PId "x"), None),
                      (EIf (
                         (EApp ((EApp ((EId (IOBinOp ">")), (EId (IOIdent "x")))),
                            (EConst (CInt 0)))),
                         (EApp (
                            (EApp ((EId (IOBinOp "+")),
                               (EApp ((EId (IOIdent "f")),
                                  (EApp (
                                     (EApp ((EId (IOBinOp "-")),
                                        (EId (IOIdent "x")))),
                                     (EConst (CInt 1))))
                                  ))
                               )),
                            (EApp ((EId (IOIdent "g")),
                               (EApp (
                                  (EApp ((EId (IOBinOp "-")), (EId (IOIdent "x")))),
                                  (EConst (CInt 2))))
                               ))
                            )),
                         (EConst (CInt 1))))
                      )),
                   None)
                  ]
                )),
             (EApp ((EId (IOIdent "f")), (EConst (CInt 10)))))),
          (Some (TGround GInt))))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test "let f (x, (y :: ys)) = 3";
    [%expect
      {|
      [(DLet (Not_recursive, "f",
          (EFun (
             ((PTuple
                 [((PId "x"), None);
                   ((PList (((PId "y"), None), ((PId "ys"), None))), None)]),
              None),
             (EConst (CInt 3)))),
          None))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test
      {| let xor (x : bool) (y : bool) = 
      match (x, y) with 
      | (true, true) -> false 
      | (true, false) -> true 
      | (false, true) -> true
      | (false, false) -> false
      |};
    [%expect
      {|
      [(DLet (Not_recursive, "xor",
          (EFun (((PId "x"), (Some (TGround GBool))),
             (EFun (((PId "y"), (Some (TGround GBool))),
                (EMatch ((ETuple [(EId (IOIdent "x")); (EId (IOIdent "y"))]),
                   [(((PTuple
                         [((PConst (CBool true)), None);
                           ((PConst (CBool true)), None)]),
                      None),
                     (EConst (CBool false)));
                     (((PTuple
                          [((PConst (CBool true)), None);
                            ((PConst (CBool false)), None)]),
                       None),
                      (EConst (CBool true)));
                     (((PTuple
                          [((PConst (CBool false)), None);
                            ((PConst (CBool true)), None)]),
                       None),
                      (EConst (CBool true)));
                     (((PTuple
                          [((PConst (CBool false)), None);
                            ((PConst (CBool false)), None)]),
                       None),
                      (EConst (CBool false)))
                     ]
                   ))
                ))
             )),
          None))
        ] |}]
  ;;

  let%expect_test _ =
    parse_test "let a = let f x = x in f (f (f (5 + f 5+5+5+f 5) + 5) + 5) + 5";
    [%expect
      {|
      [(DLet (Not_recursive, "a",
          (EClsr (
             (DLet (Not_recursive, "f",
                (EFun (((PId "x"), None), (EId (IOIdent "x")))), None)),
             (EApp (
                (EApp ((EId (IOBinOp "+")),
                   (EApp ((EId (IOIdent "f")),
                      (EApp (
                         (EApp ((EId (IOBinOp "+")),
                            (EApp ((EId (IOIdent "f")),
                               (EApp (
                                  (EApp ((EId (IOBinOp "+")),
                                     (EApp ((EId (IOIdent "f")),
                                        (EApp (
                                           (EApp ((EId (IOBinOp "+")),
                                              (EApp (
                                                 (EApp ((EId (IOBinOp "+")),
                                                    (EApp (
                                                       (EApp ((EId (IOBinOp "+")),
                                                          (EApp (
                                                             (EApp (
                                                                (EId (IOBinOp "+")),
                                                                (EConst (CInt 5)))),
                                                             (EApp (
                                                                (EId (IOIdent "f")),
                                                                (EConst (CInt 5))))
                                                             ))
                                                          )),
                                                       (EConst (CInt 5))))
                                                    )),
                                                 (EConst (CInt 5))))
                                              )),
                                           (EApp ((EId (IOIdent "f")),
                                              (EConst (CInt 5))))
                                           ))
                                        ))
                                     )),
                                  (EConst (CInt 5))))
                               ))
                            )),
                         (EConst (CInt 5))))
                      ))
                   )),
                (EConst (CInt 5))))
             )),
          None))
        ] |}]
  ;;
end

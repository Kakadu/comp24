(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser

let parse_test s =
  match parse s with
  | Ok v -> Format.printf "%s\n" Common.Ast.(show_program v)
  | Error _ -> Format.printf "Syntax error\n"
;;

let%expect_test _ =
  let () = parse_test "1 + 2 / 3;;" in
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_apply ((Exp_ident "+"), (Exp_constant (Const_int 1)))),
           (Exp_apply (
              (Exp_apply ((Exp_ident "/"), (Exp_constant (Const_int 2)))),
              (Exp_constant (Const_int 3))))
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "a != b" in
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_apply ((Exp_ident "!="), (Exp_ident "a"))),
           (Exp_ident "b"))))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "let (~-) a = a;;" in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_var "~-");
              vb_expr = (Exp_function ((Pat_var "a"), (Exp_ident "a"))) }
             ]
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "let (~!) a = a;;" in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_var "~!");
              vb_expr = (Exp_function ((Pat_var "a"), (Exp_ident "a"))) }
             ]
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "let (~!) a = (~-) a;;" in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_var "~!");
              vb_expr =
              (Exp_function ((Pat_var "a"),
                 (Exp_apply ((Exp_ident "~-"), (Exp_ident "a")))))
              }
             ]
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "1 * 2 / 3;;" in
  [%expect
    {|
    [(Str_eval
        (Exp_apply (
           (Exp_apply ((Exp_ident "/"),
              (Exp_apply (
                 (Exp_apply ((Exp_ident "*"), (Exp_constant (Const_int 1)))),
                 (Exp_constant (Const_int 2))))
              )),
           (Exp_constant (Const_int 3)))))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "(+) 1 2;;" in
  [%expect
    {|
      [(Str_eval
          (Exp_apply ((Exp_apply ((Exp_ident "+"), (Exp_constant (Const_int 1)))),
             (Exp_constant (Const_int 2)))))
        ] |}]
;;

let%expect_test _ =
  let () = parse_test {|true && true && false;;|} in
  [%expect
    {|
    [(Str_eval
        (Exp_apply (
           (Exp_apply ((Exp_ident "&&"), (Exp_constant (Const_bool true)))),
           (Exp_apply (
              (Exp_apply ((Exp_ident "&&"), (Exp_constant (Const_bool true)))),
              (Exp_constant (Const_bool false))))
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|! vbool ;;|} in
  [%expect {| [(Str_eval (Exp_apply ((Exp_ident "~!"), (Exp_ident "vbool"))))] |}]
;;

let%expect_test _ =
  let () = parse_test {|[1; 2; 3; 4];;|} in
  [%expect
    {|
    [(Str_eval
        (Exp_list ((Exp_constant (Const_int 1)),
           (Exp_list ((Exp_constant (Const_int 2)),
              (Exp_list ((Exp_constant (Const_int 3)),
                 (Exp_list ((Exp_constant (Const_int 4)),
                    (Exp_constant Const_nil)))
                 ))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "-1::[2];;" in
  [%expect
    {|
      [(Str_eval
          (Exp_list ((Exp_apply ((Exp_ident "~-"), (Exp_constant (Const_int 1)))),
             (Exp_list ((Exp_constant (Const_int 2)), (Exp_constant Const_nil))))))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test "let rec fact n k = if n <= 1 then k 1 else fact (n-1) (fun z -> k(z*n))"
  in
  [%expect
    {|
    [(Str_value
        (Decl (Recursive,
           [{ vb_pat = (Pat_var "fact");
              vb_expr =
              (Exp_function ((Pat_var "n"),
                 (Exp_function ((Pat_var "k"),
                    (Exp_ifthenelse (
                       (Exp_apply (
                          (Exp_apply ((Exp_ident "<="), (Exp_ident "n"))),
                          (Exp_constant (Const_int 1)))),
                       (Exp_apply ((Exp_ident "k"), (Exp_constant (Const_int 1))
                          )),
                       (Exp_apply (
                          (Exp_apply ((Exp_ident "fact"),
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                                (Exp_constant (Const_int 1))))
                             )),
                          (Exp_function ((Pat_var "z"),
                             (Exp_apply ((Exp_ident "k"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "*"), (Exp_ident "z"))),
                                   (Exp_ident "n")))
                                ))
                             ))
                          ))
                       ))
                    ))
                 ))
              }
             ]
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "let (+) (a : bool) (b : int) c: int = funct (10 - b)" in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_var "+");
              vb_expr =
              (Exp_function ((Pat_constraint ((Pat_var "a"), Ptyp_bool)),
                 (Exp_function ((Pat_constraint ((Pat_var "b"), Ptyp_int)),
                    (Exp_function ((Pat_var "c"),
                       (Exp_type (
                          (Exp_apply ((Exp_ident "funct"),
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "-"),
                                   (Exp_constant (Const_int 10)))),
                                (Exp_ident "b")))
                             )),
                          Ptyp_int))
                       ))
                    ))
                 ))
              }
             ]
           )))
      ] |}]
;;

let%expect_test _ =
  let () =
    parse_test {|
let rec x = 3 :: z2
 and z1 = 4:: x :: z2
 and z2 = 4:: x in
x
;;
  |}
  in
  [%expect
    {|
      [(Str_eval
          (Exp_let (
             (Decl (Recursive,
                [{ vb_pat = (Pat_var "x");
                   vb_expr =
                   (Exp_list ((Exp_constant (Const_int 3)), (Exp_ident "z2"))) };
                  { vb_pat = (Pat_var "z1");
                    vb_expr =
                    (Exp_list ((Exp_constant (Const_int 4)),
                       (Exp_list ((Exp_ident "x"), (Exp_ident "z2")))))
                    };
                  { vb_pat = (Pat_var "z2");
                    vb_expr =
                    (Exp_list ((Exp_constant (Const_int 4)), (Exp_ident "x"))) }
                  ]
                )),
             (Exp_ident "x"))))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
match (10, 11, 12::[]) with
| a, b, c::_ -> 10
| (a, b, s::_::_) -> 11
| _ -> 0
;;
  |}
  in
  [%expect
    {|
      [(Str_eval
          (Exp_match (
             (Exp_tuple
                [(Exp_constant (Const_int 10)); (Exp_constant (Const_int 11));
                  (Exp_list ((Exp_constant (Const_int 12)),
                     (Exp_constant Const_nil)))
                  ]),
             [((Pat_cons (
                  (Pat_tuple [(Pat_var "a"); (Pat_var "b"); (Pat_var "c")]),
                  Pat_any)),
               (Exp_constant (Const_int 10)));
               ((Pat_cons (
                   (Pat_tuple [(Pat_var "a"); (Pat_var "b"); (Pat_var "s")]),
                   (Pat_cons (Pat_any, Pat_any)))),
                (Exp_constant (Const_int 11)));
               (Pat_any, (Exp_constant (Const_int 0)))]
             )))
        ] |}]
;;

let%expect_test _ =
  let () = parse_test {|
[12; 13; 14]::[]
;;
  |} in
  [%expect
    {|
      [(Str_eval
          (Exp_list (
             (Exp_list ((Exp_constant (Const_int 12)),
                (Exp_list ((Exp_constant (Const_int 13)),
                   (Exp_list ((Exp_constant (Const_int 14)),
                      (Exp_constant Const_nil)))
                   ))
                )),
             (Exp_constant Const_nil))))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let (): unit = (
   ptint_int (
         (10 : int) + (true : bool)
      ) : unit
) in
10
;;
  |}
  in
  [%expect
    {|
      [(Str_eval
          (Exp_let (
             (Decl (Nonrecursive,
                [{ vb_pat = (Pat_const Const_unit);
                   vb_expr =
                   (Exp_type (
                      (Exp_type (
                         (Exp_apply ((Exp_ident "ptint_int"),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "+"),
                                  (Exp_type ((Exp_constant (Const_int 10)),
                                     Ptyp_int))
                                  )),
                               (Exp_type ((Exp_constant (Const_bool true)),
                                  Ptyp_bool))
                               ))
                            )),
                         Ptyp_unit)),
                      Ptyp_unit))
                   }
                  ]
                )),
             (Exp_constant (Const_int 10)))))
        ] |}]
;;

let%expect_test _ =
  let () = parse_test {|
let a = fun x -> fun y -> fun z -> x y z
;;
  |} in
  [%expect
    {|
      [(Str_value
          (Decl (Nonrecursive,
             [{ vb_pat = (Pat_var "a");
                vb_expr =
                (Exp_function ((Pat_var "x"),
                   (Exp_function ((Pat_var "y"),
                      (Exp_function ((Pat_var "z"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "x"), (Exp_ident "y"))),
                            (Exp_ident "z")))
                         ))
                      ))
                   ))
                }
               ]
             )))
        ] |}]
;;

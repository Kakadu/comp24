(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AefjnvMl_lib
open Parser

let parse_test s =
  match parse s with
  | Ok v -> Format.printf "%s\n" Ast.(show_program v)
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
    [(Str_eval
        (Exp_apply ((Exp_apply ((Exp_ident "!="), (Exp_ident "a"))),
           (Exp_ident "b"))))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "let (~!) a = a;;" in
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_apply ((Exp_ident "!="), (Exp_ident "a"))),
           (Exp_ident "b"))))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "let (~!) a = (~-) a;;" in
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_apply ((Exp_ident "!="), (Exp_ident "a"))),
           (Exp_ident "b"))))
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

(* new functionality *)

let%expect_test _ =
  let () = parse_test "let (+) (a : bool) (b : int) c: int = funct (10 - b)" in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_constraint ((Pat_var "+"), Ptyp_int));
              vb_expr =
              (Exp_function ((Pat_constraint ((Pat_var "a"), Ptyp_bool)),
                 (Exp_function ((Pat_constraint ((Pat_var "b"), Ptyp_int)),
                    (Exp_function ((Pat_var "c"),
                       (Exp_apply ((Exp_ident "funct"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "-"),
                                (Exp_constant (Const_int 10)))),
                             (Exp_ident "b")))
                          ))
                       ))
                    ))
                 ))
              }
             ]
           )))
      ] |}]
;;

(* Kakadu tests *)

let%expect_test _ =
  let () =
    parse_test
      {|
let rec fac n = if n<=1 then 1 else n * fac (n-1)

let main =
  let () = print_int (fac 4) in
  0
  |}
  in
  [%expect
    {|
    [(Str_value
        (Decl (Recursive,
           [{ vb_pat = (Pat_var "fac");
              vb_expr =
              (Exp_function ((Pat_var "n"),
                 (Exp_ifthenelse (
                    (Exp_apply ((Exp_apply ((Exp_ident "<="), (Exp_ident "n"))),
                       (Exp_constant (Const_int 1)))),
                    (Exp_constant (Const_int 1)),
                    (Exp_apply ((Exp_apply ((Exp_ident "*"), (Exp_ident "n"))),
                       (Exp_apply ((Exp_ident "fac"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                             (Exp_constant (Const_int 1))))
                          ))
                       ))
                    ))
                 ))
              }
             ]
           )));
      (Str_value
         (Decl (Nonrecursive,
            [{ vb_pat = (Pat_var "main");
               vb_expr =
               (Exp_let (
                  (Decl (Nonrecursive,
                     [{ vb_pat = (Pat_const Const_unit);
                        vb_expr =
                        (Exp_apply ((Exp_ident "print_int"),
                           (Exp_apply ((Exp_ident "fac"),
                              (Exp_constant (Const_int 4))))
                           ))
                        }
                       ]
                     )),
                  (Exp_constant (Const_int 0))))
               }
              ]
            )))
      ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let rec fib_acc a b n =
  if n=1 then b
  else
    let n1 = n-1 in
    let ab = a+b in
    fib_acc b ab n1

let rec fib n =
  if n<2
  then n
  else fib (n - 1) + fib (n - 2) 

let main =
  let () = print_int (fib_acc 0 1 4) in
  let () = print_int (fib 4) in
  0
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Recursive,
             [{ vb_pat = (Pat_var "fib_acc");
                vb_expr =
                (Exp_function ((Pat_var "a"),
                   (Exp_function ((Pat_var "b"),
                      (Exp_function ((Pat_var "n"),
                         (Exp_ifthenelse (
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "="), (Exp_ident "n"))),
                               (Exp_constant (Const_int 1)))),
                            (Exp_ident "b"),
                            (Exp_let (
                               (Decl (Nonrecursive,
                                  [{ vb_pat = (Pat_var "n1");
                                     vb_expr =
                                     (Exp_apply (
                                        (Exp_apply ((Exp_ident "-"),
                                           (Exp_ident "n"))),
                                        (Exp_constant (Const_int 1))))
                                     }
                                    ]
                                  )),
                               (Exp_let (
                                  (Decl (Nonrecursive,
                                     [{ vb_pat = (Pat_var "ab");
                                        vb_expr =
                                        (Exp_apply (
                                           (Exp_apply ((Exp_ident "+"),
                                              (Exp_ident "a"))),
                                           (Exp_ident "b")))
                                        }
                                       ]
                                     )),
                                  (Exp_apply (
                                     (Exp_apply (
                                        (Exp_apply ((Exp_ident "fib_acc"),
                                           (Exp_ident "b"))),
                                        (Exp_ident "ab"))),
                                     (Exp_ident "n1")))
                                  ))
                               ))
                            ))
                         ))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Recursive,
              [{ vb_pat = (Pat_var "fib");
                 vb_expr =
                 (Exp_function ((Pat_var "n"),
                    (Exp_ifthenelse (
                       (Exp_apply ((Exp_apply ((Exp_ident "<"), (Exp_ident "n"))),
                          (Exp_constant (Const_int 2)))),
                       (Exp_ident "n"),
                       (Exp_apply (
                          (Exp_apply ((Exp_ident "+"),
                             (Exp_apply ((Exp_ident "fib"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                                   (Exp_constant (Const_int 1))))
                                ))
                             )),
                          (Exp_apply ((Exp_ident "fib"),
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                                (Exp_constant (Const_int 2))))
                             ))
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_const Const_unit);
                          vb_expr =
                          (Exp_apply ((Exp_ident "print_int"),
                             (Exp_apply (
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "fib_acc"),
                                      (Exp_constant (Const_int 0)))),
                                   (Exp_constant (Const_int 1)))),
                                (Exp_constant (Const_int 4))))
                             ))
                          }
                         ]
                       )),
                    (Exp_let (
                       (Decl (Nonrecursive,
                          [{ vb_pat = (Pat_const Const_unit);
                             vb_expr =
                             (Exp_apply ((Exp_ident "print_int"),
                                (Exp_apply ((Exp_ident "fib"),
                                   (Exp_constant (Const_int 4))))
                                ))
                             }
                            ]
                          )),
                       (Exp_constant (Const_int 0))))
                    ))
                 }
                ]
              )))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let wrap f = if 1 = 1 then f else f

let test3 a b c =
  let a = print_int a in
  let b = print_int b in
  let c = print_int c in
  0

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let rez =
      (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
         1000000000)
  in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Nonrecursive,
             [{ vb_pat = (Pat_var "wrap");
                vb_expr =
                (Exp_function ((Pat_var "f"),
                   (Exp_ifthenelse (
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "="), (Exp_constant (Const_int 1))
                            )),
                         (Exp_constant (Const_int 1)))),
                      (Exp_ident "f"), (Exp_ident "f")))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "test3");
                 vb_expr =
                 (Exp_function ((Pat_var "a"),
                    (Exp_function ((Pat_var "b"),
                       (Exp_function ((Pat_var "c"),
                          (Exp_let (
                             (Decl (Nonrecursive,
                                [{ vb_pat = (Pat_var "a");
                                   vb_expr =
                                   (Exp_apply ((Exp_ident "print_int"),
                                      (Exp_ident "a")))
                                   }
                                  ]
                                )),
                             (Exp_let (
                                (Decl (Nonrecursive,
                                   [{ vb_pat = (Pat_var "b");
                                      vb_expr =
                                      (Exp_apply ((Exp_ident "print_int"),
                                         (Exp_ident "b")))
                                      }
                                     ]
                                   )),
                                (Exp_let (
                                   (Decl (Nonrecursive,
                                      [{ vb_pat = (Pat_var "c");
                                         vb_expr =
                                         (Exp_apply ((Exp_ident "print_int"),
                                            (Exp_ident "c")))
                                         }
                                        ]
                                      )),
                                   (Exp_constant (Const_int 0))))
                                ))
                             ))
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "test10");
                 vb_expr =
                 (Exp_function ((Pat_var "a"),
                    (Exp_function ((Pat_var "b"),
                       (Exp_function ((Pat_var "c"),
                          (Exp_function ((Pat_var "d"),
                             (Exp_function ((Pat_var "e"),
                                (Exp_function ((Pat_var "f"),
                                   (Exp_function ((Pat_var "g"),
                                      (Exp_function ((Pat_var "h"),
                                         (Exp_function ((Pat_var "i"),
                                            (Exp_function ((Pat_var "j"),
                                               (Exp_apply (
                                                  (Exp_apply ((Exp_ident "+"),
                                                     (Exp_apply (
                                                        (Exp_apply (
                                                           (Exp_ident "+"),
                                                           (Exp_apply (
                                                              (Exp_apply (
                                                                 (Exp_ident "+"),
                                                                 (Exp_apply (
                                                                    (Exp_apply (
                                                                       (Exp_ident
                                                                          "+"),
                                                                       (Exp_apply (
                                                                          (
                                                                          Exp_apply (
                                                                          (Exp_ident
                                                                          "+"),
                                                                          (Exp_apply (
                                                                          (Exp_apply (
                                                                          (Exp_ident
                                                                          "+"),
                                                                          (Exp_apply (
                                                                          (Exp_apply (
                                                                          (Exp_ident
                                                                          "+"),
                                                                          (Exp_apply (
                                                                          (Exp_apply (
                                                                          (Exp_ident
                                                                          "+"),
                                                                          (Exp_apply (
                                                                          (Exp_apply (
                                                                          (Exp_ident
                                                                          "+"),
                                                                          (Exp_ident
                                                                          "a"))),
                                                                          (Exp_ident
                                                                          "b"))))),
                                                                          (Exp_ident
                                                                          "c"))))),
                                                                          (Exp_ident
                                                                          "d"))))),
                                                                          (Exp_ident
                                                                          "e"))))),
                                                                          (
                                                                          Exp_ident
                                                                          "f")))
                                                                       )),
                                                                    (Exp_ident "g")
                                                                    ))
                                                                 )),
                                                              (Exp_ident "h")))
                                                           )),
                                                        (Exp_ident "i")))
                                                     )),
                                                  (Exp_ident "j")))
                                               ))
                                            ))
                                         ))
                                      ))
                                   ))
                                ))
                             ))
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_var "rez");
                          vb_expr =
                          (Exp_apply (
                             (Exp_apply (
                                (Exp_apply (
                                   (Exp_apply (
                                      (Exp_apply (
                                         (Exp_apply (
                                            (Exp_apply (
                                               (Exp_apply (
                                                  (Exp_apply (
                                                     (Exp_apply (
                                                        (Exp_apply (
                                                           (Exp_ident "wrap"),
                                                           (Exp_ident "test10"))),
                                                        (Exp_constant (Const_int 1))
                                                        )),
                                                     (Exp_constant (Const_int 10))
                                                     )),
                                                  (Exp_constant (Const_int 100)))),
                                               (Exp_constant (Const_int 1000)))),
                                            (Exp_constant (Const_int 10000)))),
                                         (Exp_constant (Const_int 100000)))),
                                      (Exp_constant (Const_int 1000000)))),
                                   (Exp_constant (Const_int 10000000)))),
                                (Exp_constant (Const_int 100000000)))),
                             (Exp_constant (Const_int 1000000000))))
                          }
                         ]
                       )),
                    (Exp_let (
                       (Decl (Nonrecursive,
                          [{ vb_pat = (Pat_const Const_unit);
                             vb_expr =
                             (Exp_apply ((Exp_ident "print_int"), (Exp_ident "rez")
                                ))
                             }
                            ]
                          )),
                       (Exp_let (
                          (Decl (Nonrecursive,
                             [{ vb_pat = (Pat_var "temp2");
                                vb_expr =
                                (Exp_apply (
                                   (Exp_apply (
                                      (Exp_apply (
                                         (Exp_apply ((Exp_ident "wrap"),
                                            (Exp_ident "test3"))),
                                         (Exp_constant (Const_int 1)))),
                                      (Exp_constant (Const_int 10)))),
                                   (Exp_constant (Const_int 100))))
                                }
                               ]
                             )),
                          (Exp_constant (Const_int 0))))
                       ))
                    ))
                 }
                ]
              )))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let rec fix f x = f (fix f) x

let fac self n = if n<=1 then 1 else n * self (n-1)

let main =
  let () = print_int (fix fac 6) in
  0
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Recursive,
             [{ vb_pat = (Pat_var "fix");
                vb_expr =
                (Exp_function ((Pat_var "f"),
                   (Exp_function ((Pat_var "x"),
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "f"),
                            (Exp_apply ((Exp_ident "fix"), (Exp_ident "f"))))),
                         (Exp_ident "x")))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "fac");
                 vb_expr =
                 (Exp_function ((Pat_var "self"),
                    (Exp_function ((Pat_var "n"),
                       (Exp_ifthenelse (
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "<="), (Exp_ident "n"))),
                             (Exp_constant (Const_int 1)))),
                          (Exp_constant (Const_int 1)),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "*"), (Exp_ident "n"))),
                             (Exp_apply ((Exp_ident "self"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                                   (Exp_constant (Const_int 1))))
                                ))
                             ))
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_const Const_unit);
                          vb_expr =
                          (Exp_apply ((Exp_ident "print_int"),
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "fix"), (Exp_ident "fac"))),
                                (Exp_constant (Const_int 6))))
                             ))
                          }
                         ]
                       )),
                    (Exp_constant (Const_int 0))))
                 }
                ]
              )))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

let foo x = foo true (foo false (foo true (foo false x)))
let main =
  let () = print_int (foo 11) in
  0
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Nonrecursive,
             [{ vb_pat = (Pat_var "foo");
                vb_expr =
                (Exp_function ((Pat_var "b"),
                   (Exp_ifthenelse ((Exp_ident "b"),
                      (Exp_function ((Pat_var "foo"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "+"), (Exp_ident "foo"))),
                            (Exp_constant (Const_int 2))))
                         )),
                      (Exp_function ((Pat_var "foo"),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "*"), (Exp_ident "foo"))),
                            (Exp_constant (Const_int 10))))
                         ))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "foo");
                 vb_expr =
                 (Exp_function ((Pat_var "x"),
                    (Exp_apply (
                       (Exp_apply ((Exp_ident "foo"),
                          (Exp_constant (Const_bool true)))),
                       (Exp_apply (
                          (Exp_apply ((Exp_ident "foo"),
                             (Exp_constant (Const_bool false)))),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "foo"),
                                (Exp_constant (Const_bool true)))),
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "foo"),
                                   (Exp_constant (Const_bool false)))),
                                (Exp_ident "x")))
                             ))
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_const Const_unit);
                          vb_expr =
                          (Exp_apply ((Exp_ident "print_int"),
                             (Exp_apply ((Exp_ident "foo"),
                                (Exp_constant (Const_int 11))))
                             ))
                          }
                         ]
                       )),
                    (Exp_constant (Const_int 0))))
                 }
                ]
              )))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
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
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Nonrecursive,
             [{ vb_pat = (Pat_var "foo");
                vb_expr =
                (Exp_function ((Pat_var "a"),
                   (Exp_function ((Pat_var "b"),
                      (Exp_function ((Pat_var "c"),
                         (Exp_let (
                            (Decl (Nonrecursive,
                               [{ vb_pat = (Pat_const Const_unit);
                                  vb_expr =
                                  (Exp_apply ((Exp_ident "print_int"),
                                     (Exp_ident "a")))
                                  }
                                 ]
                               )),
                            (Exp_let (
                               (Decl (Nonrecursive,
                                  [{ vb_pat = (Pat_const Const_unit);
                                     vb_expr =
                                     (Exp_apply ((Exp_ident "print_int"),
                                        (Exp_ident "b")))
                                     }
                                    ]
                                  )),
                               (Exp_let (
                                  (Decl (Nonrecursive,
                                     [{ vb_pat = (Pat_const Const_unit);
                                        vb_expr =
                                        (Exp_apply ((Exp_ident "print_int"),
                                           (Exp_ident "c")))
                                        }
                                       ]
                                     )),
                                  (Exp_apply (
                                     (Exp_apply ((Exp_ident "+"), (Exp_ident "a"))),
                                     (Exp_apply (
                                        (Exp_apply ((Exp_ident "*"),
                                           (Exp_ident "b"))),
                                        (Exp_ident "c")))
                                     ))
                                  ))
                               ))
                            ))
                         ))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_var "foo");
                          vb_expr =
                          (Exp_apply ((Exp_ident "foo"),
                             (Exp_constant (Const_int 1))))
                          }
                         ]
                       )),
                    (Exp_let (
                       (Decl (Nonrecursive,
                          [{ vb_pat = (Pat_var "foo");
                             vb_expr =
                             (Exp_apply ((Exp_ident "foo"),
                                (Exp_constant (Const_int 2))))
                             }
                            ]
                          )),
                       (Exp_let (
                          (Decl (Nonrecursive,
                             [{ vb_pat = (Pat_var "foo");
                                vb_expr =
                                (Exp_apply ((Exp_ident "foo"),
                                   (Exp_constant (Const_int 3))))
                                }
                               ]
                             )),
                          (Exp_let (
                             (Decl (Nonrecursive,
                                [{ vb_pat = (Pat_const Const_unit);
                                   vb_expr =
                                   (Exp_apply ((Exp_ident "print_int"),
                                      (Exp_ident "foo")))
                                   }
                                  ]
                                )),
                             (Exp_constant (Const_int 0))))
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
    parse_test
      {|
let foo a =
  let () = print_int a in fun b ->
  let () = print_int b in fun c ->
  print_int c

let main =
  let () = foo 4 8 9 in
  0
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Nonrecursive,
             [{ vb_pat = (Pat_var "foo");
                vb_expr =
                (Exp_function ((Pat_var "a"),
                   (Exp_let (
                      (Decl (Nonrecursive,
                         [{ vb_pat = (Pat_const Const_unit);
                            vb_expr =
                            (Exp_apply ((Exp_ident "print_int"), (Exp_ident "a")))
                            }
                           ]
                         )),
                      (Exp_function ((Pat_var "b"),
                         (Exp_let (
                            (Decl (Nonrecursive,
                               [{ vb_pat = (Pat_const Const_unit);
                                  vb_expr =
                                  (Exp_apply ((Exp_ident "print_int"),
                                     (Exp_ident "b")))
                                  }
                                 ]
                               )),
                            (Exp_function ((Pat_var "c"),
                               (Exp_apply ((Exp_ident "print_int"), (Exp_ident "c")
                                  ))
                               ))
                            ))
                         ))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_const Const_unit);
                          vb_expr =
                          (Exp_apply (
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "foo"),
                                   (Exp_constant (Const_int 4)))),
                                (Exp_constant (Const_int 8)))),
                             (Exp_constant (Const_int 9))))
                          }
                         ]
                       )),
                    (Exp_constant (Const_int 0))))
                 }
                ]
              )))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let _start () () a () b _c () d __ =
   let () = print_int (a+b) in
   let () = print_int __ in
   a*b / _c + d

let main =
   print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Nonrecursive,
             [{ vb_pat = (Pat_var "_start");
                vb_expr =
                (Exp_function ((Pat_const Const_unit),
                   (Exp_function ((Pat_const Const_unit),
                      (Exp_function ((Pat_var "a"),
                         (Exp_function ((Pat_const Const_unit),
                            (Exp_function ((Pat_var "b"),
                               (Exp_function ((Pat_var "_c"),
                                  (Exp_function ((Pat_const Const_unit),
                                     (Exp_function ((Pat_var "d"),
                                        (Exp_function ((Pat_var "__"),
                                           (Exp_let (
                                              (Decl (Nonrecursive,
                                                 [{ vb_pat = (Pat_const Const_unit);
                                                    vb_expr =
                                                    (Exp_apply (
                                                       (Exp_ident "print_int"),
                                                       (Exp_apply (
                                                          (Exp_apply (
                                                             (Exp_ident "+"),
                                                             (Exp_ident "a"))),
                                                          (Exp_ident "b")))
                                                       ))
                                                    }
                                                   ]
                                                 )),
                                              (Exp_let (
                                                 (Decl (Nonrecursive,
                                                    [{ vb_pat =
                                                       (Pat_const Const_unit);
                                                       vb_expr =
                                                       (Exp_apply (
                                                          (Exp_ident "print_int"),
                                                          (Exp_ident "__")))
                                                       }
                                                      ]
                                                    )),
                                                 (Exp_apply (
                                                    (Exp_apply ((Exp_ident "+"),
                                                       (Exp_apply (
                                                          (Exp_apply (
                                                             (Exp_ident "/"),
                                                             (Exp_apply (
                                                                (Exp_apply (
                                                                   (Exp_ident "*"),
                                                                   (Exp_ident "a")
                                                                   )),
                                                                (Exp_ident "b")))
                                                             )),
                                                          (Exp_ident "_c")))
                                                       )),
                                                    (Exp_ident "d")))
                                                 ))
                                              ))
                                           ))
                                        ))
                                     ))
                                  ))
                               ))
                            ))
                         ))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_apply ((Exp_ident "print_int"),
                    (Exp_apply (
                       (Exp_apply (
                          (Exp_apply (
                             (Exp_apply (
                                (Exp_apply (
                                   (Exp_apply (
                                      (Exp_apply (
                                         (Exp_apply (
                                            (Exp_apply ((Exp_ident "_start"),
                                               (Exp_apply ((Exp_ident "print_int"),
                                                  (Exp_constant (Const_int 1))))
                                               )),
                                            (Exp_apply ((Exp_ident "print_int"),
                                               (Exp_constant (Const_int 2))))
                                            )),
                                         (Exp_constant (Const_int 3)))),
                                      (Exp_apply ((Exp_ident "print_int"),
                                         (Exp_constant (Const_int 4))))
                                      )),
                                   (Exp_constant (Const_int 100)))),
                                (Exp_constant (Const_int 1000)))),
                             (Exp_apply ((Exp_ident "print_int"),
                                (Exp_apply ((Exp_ident "~-"),
                                   (Exp_constant (Const_int 1))))
                                ))
                             )),
                          (Exp_constant (Const_int 10000)))),
                       (Exp_apply ((Exp_ident "~-"),
                          (Exp_constant (Const_int 555555))))
                       ))
                    ))
                 }
                ]
              )))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let addi = fun f g x -> (f x (g x: bool) : int)

let main =
  let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
  0
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Nonrecursive,
             [{ vb_pat = (Pat_var "addi");
                vb_expr =
                (Exp_function ((Pat_var "f"),
                   (Exp_function ((Pat_var "g"),
                      (Exp_function ((Pat_var "x"),
                         (Exp_type (
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "f"), (Exp_ident "x"))),
                               (Exp_type (
                                  (Exp_apply ((Exp_ident "g"), (Exp_ident "x"))),
                                  Ptyp_bool))
                               )),
                            Ptyp_int))
                         ))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_const Const_unit);
                          vb_expr =
                          (Exp_apply ((Exp_ident "print_int"),
                             (Exp_apply (
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "addi"),
                                      (Exp_function ((Pat_var "x"),
                                         (Exp_function ((Pat_var "b"),
                                            (Exp_ifthenelse ((Exp_ident "b"),
                                               (Exp_apply (
                                                  (Exp_apply ((Exp_ident "+"),
                                                     (Exp_ident "x"))),
                                                  (Exp_constant (Const_int 1)))),
                                               (Exp_apply (
                                                  (Exp_apply ((Exp_ident "*"),
                                                     (Exp_ident "x"))),
                                                  (Exp_constant (Const_int 2))))
                                               ))
                                            ))
                                         ))
                                      )),
                                   (Exp_function ((Pat_var "_start"),
                                      (Exp_apply (
                                         (Exp_apply ((Exp_ident "="),
                                            (Exp_apply (
                                               (Exp_apply ((Exp_ident "/"),
                                                  (Exp_ident "_start"))),
                                               (Exp_constant (Const_int 2))))
                                            )),
                                         (Exp_constant (Const_int 0))))
                                      ))
                                   )),
                                (Exp_constant (Const_int 4))))
                             ))
                          }
                         ]
                       )),
                    (Exp_constant (Const_int 0))))
                 }
                ]
              )))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let temp =
  let f = fun x -> x in
  (f 1, f true)
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Nonrecursive,
             [{ vb_pat = (Pat_var "temp");
                vb_expr =
                (Exp_let (
                   (Decl (Nonrecursive,
                      [{ vb_pat = (Pat_var "f");
                         vb_expr = (Exp_function ((Pat_var "x"), (Exp_ident "x")))
                         }
                        ]
                      )),
                   (Exp_tuple
                      [(Exp_apply ((Exp_ident "f"), (Exp_constant (Const_int 1))));
                        (Exp_apply ((Exp_ident "f"),
                           (Exp_constant (Const_bool true))))
                        ])
                   ))
                }
               ]
             )))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let rec fix f x = f (fix f) x
let map f p = let (a,b) = p in (f a, f b)
let fixpoly l =
  fix (fun self l -> map (fun li x -> li (self l) x) l) l
let feven p n =
  let (e, o) = p in
  if n = 0 then 1 else o (n - 1)
let fodd p n =
  let (e, o) = p in
  if n = 0 then 0 else e (n - 1)
let tie = fixpoly (feven, fodd)

let rec meven n = if n = 0 then 1 else modd (n - 1)

|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Recursive,
             [{ vb_pat = (Pat_var "fix");
                vb_expr =
                (Exp_function ((Pat_var "f"),
                   (Exp_function ((Pat_var "x"),
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "f"),
                            (Exp_apply ((Exp_ident "fix"), (Exp_ident "f"))))),
                         (Exp_ident "x")))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "map");
                 vb_expr =
                 (Exp_function ((Pat_var "f"),
                    (Exp_function ((Pat_var "p"),
                       (Exp_let (
                          (Decl (Nonrecursive,
                             [{ vb_pat = (Pat_tuple [(Pat_var "a"); (Pat_var "b")]);
                                vb_expr = (Exp_ident "p") }
                               ]
                             )),
                          (Exp_tuple
                             [(Exp_apply ((Exp_ident "f"), (Exp_ident "a")));
                               (Exp_apply ((Exp_ident "f"), (Exp_ident "b")))])
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "fixpoly");
                 vb_expr =
                 (Exp_function ((Pat_var "l"),
                    (Exp_apply (
                       (Exp_apply ((Exp_ident "fix"),
                          (Exp_function ((Pat_var "self"),
                             (Exp_function ((Pat_var "l"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "map"),
                                      (Exp_function ((Pat_var "li"),
                                         (Exp_function ((Pat_var "x"),
                                            (Exp_apply (
                                               (Exp_apply ((Exp_ident "li"),
                                                  (Exp_apply ((Exp_ident "self"),
                                                     (Exp_ident "l")))
                                                  )),
                                               (Exp_ident "x")))
                                            ))
                                         ))
                                      )),
                                   (Exp_ident "l")))
                                ))
                             ))
                          )),
                       (Exp_ident "l")))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "feven");
                 vb_expr =
                 (Exp_function ((Pat_var "p"),
                    (Exp_function ((Pat_var "n"),
                       (Exp_let (
                          (Decl (Nonrecursive,
                             [{ vb_pat = (Pat_tuple [(Pat_var "e"); (Pat_var "o")]);
                                vb_expr = (Exp_ident "p") }
                               ]
                             )),
                          (Exp_ifthenelse (
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "="), (Exp_ident "n"))),
                                (Exp_constant (Const_int 0)))),
                             (Exp_constant (Const_int 1)),
                             (Exp_apply ((Exp_ident "o"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                                   (Exp_constant (Const_int 1))))
                                ))
                             ))
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "fodd");
                 vb_expr =
                 (Exp_function ((Pat_var "p"),
                    (Exp_function ((Pat_var "n"),
                       (Exp_let (
                          (Decl (Nonrecursive,
                             [{ vb_pat = (Pat_tuple [(Pat_var "e"); (Pat_var "o")]);
                                vb_expr = (Exp_ident "p") }
                               ]
                             )),
                          (Exp_ifthenelse (
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "="), (Exp_ident "n"))),
                                (Exp_constant (Const_int 0)))),
                             (Exp_constant (Const_int 0)),
                             (Exp_apply ((Exp_ident "e"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                                   (Exp_constant (Const_int 1))))
                                ))
                             ))
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "tie");
                 vb_expr =
                 (Exp_apply ((Exp_ident "fixpoly"),
                    (Exp_tuple [(Exp_ident "feven"); (Exp_ident "fodd")])))
                 }
                ]
              )));
        (Str_value
           (Decl (Recursive,
              [{ vb_pat = (Pat_var "meven");
                 vb_expr =
                 (Exp_function ((Pat_var "n"),
                    (Exp_ifthenelse (
                       (Exp_apply ((Exp_apply ((Exp_ident "="), (Exp_ident "n"))),
                          (Exp_constant (Const_int 0)))),
                       (Exp_constant (Const_int 1)),
                       (Exp_apply ((Exp_ident "modd"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                             (Exp_constant (Const_int 1))))
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
    parse_test
      {|
let rec length xs =
  match xs with
  | [] -> 0
  | h::tl -> 1 + length tl

let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0

let rec map f xs =
  match xs with
  | [] -> []
  | a::[] -> [f a]
  | a::b::[] -> [f a; f b]
  | a::b::c::[] -> [f a; f b; f c]
  | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl

let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

let concat =
  let rec helper xs =
    match xs with
    | [] -> []
    | h::tl -> append h (helper tl)
  in helper

let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl

let rec cartesian xs ys =
  match xs with
  | [] -> []
  | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)

let main =
  let () = iter print_int [1;2;3] in
  let () = print_int (length (cartesian [1;2] [1;2;3;4])) in
  0
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Recursive,
             [{ vb_pat = (Pat_var "length");
                vb_expr =
                (Exp_function ((Pat_var "xs"),
                   (Exp_match ((Exp_ident "xs"),
                      [((Pat_const Const_nil), (Exp_constant (Const_int 0)));
                        ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "+"),
                               (Exp_constant (Const_int 1)))),
                            (Exp_apply ((Exp_ident "length"), (Exp_ident "tl"))))))
                        ]
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "length_tail");
                 vb_expr =
                 (Exp_let (
                    (Decl (Recursive,
                       [{ vb_pat = (Pat_var "helper");
                          vb_expr =
                          (Exp_function ((Pat_var "acc"),
                             (Exp_function ((Pat_var "xs"),
                                (Exp_match ((Exp_ident "xs"),
                                   [((Pat_const Const_nil), (Exp_ident "acc"));
                                     ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                                      (Exp_apply (
                                         (Exp_apply ((Exp_ident "helper"),
                                            (Exp_apply (
                                               (Exp_apply ((Exp_ident "+"),
                                                  (Exp_ident "acc"))),
                                               (Exp_constant (Const_int 1))))
                                            )),
                                         (Exp_ident "tl"))))
                                     ]
                                   ))
                                ))
                             ))
                          }
                         ]
                       )),
                    (Exp_apply ((Exp_ident "helper"), (Exp_constant (Const_int 0))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Recursive,
              [{ vb_pat = (Pat_var "map");
                 vb_expr =
                 (Exp_function ((Pat_var "f"),
                    (Exp_function ((Pat_var "xs"),
                       (Exp_match ((Exp_ident "xs"),
                          [((Pat_const Const_nil), (Exp_constant Const_nil));
                            ((Pat_cons ((Pat_var "a"), (Pat_const Const_nil))),
                             (Exp_list (
                                (Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                (Exp_constant Const_nil))));
                            ((Pat_cons ((Pat_var "a"),
                                (Pat_cons ((Pat_var "b"), (Pat_const Const_nil))))),
                             (Exp_list (
                                (Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                (Exp_list (
                                   (Exp_apply ((Exp_ident "f"), (Exp_ident "b"))),
                                   (Exp_constant Const_nil)))
                                )));
                            ((Pat_cons ((Pat_var "a"),
                                (Pat_cons ((Pat_var "b"),
                                   (Pat_cons ((Pat_var "c"), (Pat_const Const_nil)
                                      ))
                                   ))
                                )),
                             (Exp_list (
                                (Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                (Exp_list (
                                   (Exp_apply ((Exp_ident "f"), (Exp_ident "b"))),
                                   (Exp_list (
                                      (Exp_apply ((Exp_ident "f"), (Exp_ident "c")
                                         )),
                                      (Exp_constant Const_nil)))
                                   ))
                                )));
                            ((Pat_cons ((Pat_var "a"),
                                (Pat_cons ((Pat_var "b"),
                                   (Pat_cons ((Pat_var "c"),
                                      (Pat_cons ((Pat_var "d"), (Pat_var "tl")))))
                                   ))
                                )),
                             (Exp_list (
                                (Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                (Exp_list (
                                   (Exp_apply ((Exp_ident "f"), (Exp_ident "b"))),
                                   (Exp_list (
                                      (Exp_apply ((Exp_ident "f"), (Exp_ident "c")
                                         )),
                                      (Exp_list (
                                         (Exp_apply ((Exp_ident "f"),
                                            (Exp_ident "d"))),
                                         (Exp_apply (
                                            (Exp_apply ((Exp_ident "map"),
                                               (Exp_ident "f"))),
                                            (Exp_ident "tl")))
                                         ))
                                      ))
                                   ))
                                )))
                            ]
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Recursive,
              [{ vb_pat = (Pat_var "append");
                 vb_expr =
                 (Exp_function ((Pat_var "xs"),
                    (Exp_function ((Pat_var "ys"),
                       (Exp_match ((Exp_ident "xs"),
                          [((Pat_const Const_nil), (Exp_ident "ys"));
                            ((Pat_cons ((Pat_var "x"), (Pat_var "xs"))),
                             (Exp_list ((Exp_ident "x"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "append"),
                                      (Exp_ident "xs"))),
                                   (Exp_ident "ys")))
                                )))
                            ]
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "concat");
                 vb_expr =
                 (Exp_let (
                    (Decl (Recursive,
                       [{ vb_pat = (Pat_var "helper");
                          vb_expr =
                          (Exp_function ((Pat_var "xs"),
                             (Exp_match ((Exp_ident "xs"),
                                [((Pat_const Const_nil), (Exp_constant Const_nil));
                                  ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                                   (Exp_apply (
                                      (Exp_apply ((Exp_ident "append"),
                                         (Exp_ident "h"))),
                                      (Exp_apply ((Exp_ident "helper"),
                                         (Exp_ident "tl")))
                                      )))
                                  ]
                                ))
                             ))
                          }
                         ]
                       )),
                    (Exp_ident "helper")))
                 }
                ]
              )));
        (Str_value
           (Decl (Recursive,
              [{ vb_pat = (Pat_var "iter");
                 vb_expr =
                 (Exp_function ((Pat_var "f"),
                    (Exp_function ((Pat_var "xs"),
                       (Exp_match ((Exp_ident "xs"),
                          [((Pat_const Const_nil), (Exp_constant Const_unit));
                            ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                             (Exp_let (
                                (Decl (Nonrecursive,
                                   [{ vb_pat = (Pat_const Const_unit);
                                      vb_expr =
                                      (Exp_apply ((Exp_ident "f"), (Exp_ident "h")
                                         ))
                                      }
                                     ]
                                   )),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "iter"), (Exp_ident "f")
                                      )),
                                   (Exp_ident "tl")))
                                )))
                            ]
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Recursive,
              [{ vb_pat = (Pat_var "cartesian");
                 vb_expr =
                 (Exp_function ((Pat_var "xs"),
                    (Exp_function ((Pat_var "ys"),
                       (Exp_match ((Exp_ident "xs"),
                          [((Pat_const Const_nil), (Exp_constant Const_nil));
                            ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "append"),
                                   (Exp_apply (
                                      (Exp_apply ((Exp_ident "map"),
                                         (Exp_function ((Pat_var "a"),
                                            (Exp_tuple
                                               [(Exp_ident "h"); (Exp_ident "a")])
                                            ))
                                         )),
                                      (Exp_ident "ys")))
                                   )),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "cartesian"),
                                      (Exp_ident "tl"))),
                                   (Exp_ident "ys")))
                                )))
                            ]
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_const Const_unit);
                          vb_expr =
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "iter"),
                                (Exp_ident "print_int"))),
                             (Exp_list ((Exp_constant (Const_int 1)),
                                (Exp_list ((Exp_constant (Const_int 2)),
                                   (Exp_list ((Exp_constant (Const_int 3)),
                                      (Exp_constant Const_nil)))
                                   ))
                                ))
                             ))
                          }
                         ]
                       )),
                    (Exp_let (
                       (Decl (Nonrecursive,
                          [{ vb_pat = (Pat_const Const_unit);
                             vb_expr =
                             (Exp_apply ((Exp_ident "print_int"),
                                (Exp_apply ((Exp_ident "length"),
                                   (Exp_apply (
                                      (Exp_apply ((Exp_ident "cartesian"),
                                         (Exp_list ((Exp_constant (Const_int 1)),
                                            (Exp_list (
                                               (Exp_constant (Const_int 2)),
                                               (Exp_constant Const_nil)))
                                            ))
                                         )),
                                      (Exp_list ((Exp_constant (Const_int 1)),
                                         (Exp_list ((Exp_constant (Const_int 2)),
                                            (Exp_list (
                                               (Exp_constant (Const_int 3)),
                                               (Exp_list (
                                                  (Exp_constant (Const_int 4)),
                                                  (Exp_constant Const_nil)))
                                               ))
                                            ))
                                         ))
                                      ))
                                   ))
                                ))
                             }
                            ]
                          )),
                       (Exp_constant (Const_int 0))))
                    ))
                 }
                ]
              )))
        ]
      |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let rec x = 3 :: y
and y = 4:: x in
x
|}
  in
  [%expect
    {|
      [(Str_eval
          (Exp_let (
             (Decl (Recursive,
                [{ vb_pat = (Pat_var "x");
                   vb_expr =
                   (Exp_list ((Exp_constant (Const_int 3)), (Exp_ident "y"))) };
                  { vb_pat = (Pat_var "y");
                    vb_expr =
                    (Exp_list ((Exp_constant (Const_int 4)), (Exp_ident "x"))) }
                  ]
                )),
             (Exp_ident "x"))))
        ]
      |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {|
let rec fix f x = f (fix f) x
let map f p = let (a,b) = p in (f a, f b)
let fixpoly l =
  fix (fun self l -> map (fun li x -> li (self l) x) l) l
let feven p n =
  let (e, o) = p in
  if n == 0 then 1 else o (n - 1)
let fodd p n =
  let (e, o) = p in
  if n == 0 then 0 else e (n - 1)
let tie = fixpoly (feven, fodd)

let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1)
let main =
  let () = print_int (modd 1) in
  let () = print_int (meven 2) in
  let (even,odd) = tie in
  let () = print_int (odd 3) in
  let () = print_int (even 4) in
  0
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Recursive,
             [{ vb_pat = (Pat_var "fix");
                vb_expr =
                (Exp_function ((Pat_var "f"),
                   (Exp_function ((Pat_var "x"),
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "f"),
                            (Exp_apply ((Exp_ident "fix"), (Exp_ident "f"))))),
                         (Exp_ident "x")))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "map");
                 vb_expr =
                 (Exp_function ((Pat_var "f"),
                    (Exp_function ((Pat_var "p"),
                       (Exp_let (
                          (Decl (Nonrecursive,
                             [{ vb_pat = (Pat_tuple [(Pat_var "a"); (Pat_var "b")]);
                                vb_expr = (Exp_ident "p") }
                               ]
                             )),
                          (Exp_tuple
                             [(Exp_apply ((Exp_ident "f"), (Exp_ident "a")));
                               (Exp_apply ((Exp_ident "f"), (Exp_ident "b")))])
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "fixpoly");
                 vb_expr =
                 (Exp_function ((Pat_var "l"),
                    (Exp_apply (
                       (Exp_apply ((Exp_ident "fix"),
                          (Exp_function ((Pat_var "self"),
                             (Exp_function ((Pat_var "l"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "map"),
                                      (Exp_function ((Pat_var "li"),
                                         (Exp_function ((Pat_var "x"),
                                            (Exp_apply (
                                               (Exp_apply ((Exp_ident "li"),
                                                  (Exp_apply ((Exp_ident "self"),
                                                     (Exp_ident "l")))
                                                  )),
                                               (Exp_ident "x")))
                                            ))
                                         ))
                                      )),
                                   (Exp_ident "l")))
                                ))
                             ))
                          )),
                       (Exp_ident "l")))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "feven");
                 vb_expr =
                 (Exp_function ((Pat_var "p"),
                    (Exp_function ((Pat_var "n"),
                       (Exp_let (
                          (Decl (Nonrecursive,
                             [{ vb_pat = (Pat_tuple [(Pat_var "e"); (Pat_var "o")]);
                                vb_expr = (Exp_ident "p") }
                               ]
                             )),
                          (Exp_ifthenelse (
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "=="), (Exp_ident "n"))),
                                (Exp_constant (Const_int 0)))),
                             (Exp_constant (Const_int 1)),
                             (Exp_apply ((Exp_ident "o"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                                   (Exp_constant (Const_int 1))))
                                ))
                             ))
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "fodd");
                 vb_expr =
                 (Exp_function ((Pat_var "p"),
                    (Exp_function ((Pat_var "n"),
                       (Exp_let (
                          (Decl (Nonrecursive,
                             [{ vb_pat = (Pat_tuple [(Pat_var "e"); (Pat_var "o")]);
                                vb_expr = (Exp_ident "p") }
                               ]
                             )),
                          (Exp_ifthenelse (
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "=="), (Exp_ident "n"))),
                                (Exp_constant (Const_int 0)))),
                             (Exp_constant (Const_int 0)),
                             (Exp_apply ((Exp_ident "e"),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                                   (Exp_constant (Const_int 1))))
                                ))
                             ))
                          ))
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "tie");
                 vb_expr =
                 (Exp_apply ((Exp_ident "fixpoly"),
                    (Exp_tuple [(Exp_ident "feven"); (Exp_ident "fodd")])))
                 }
                ]
              )));
        (Str_value
           (Decl (Recursive,
              [{ vb_pat = (Pat_var "meven");
                 vb_expr =
                 (Exp_function ((Pat_var "n"),
                    (Exp_ifthenelse (
                       (Exp_apply ((Exp_apply ((Exp_ident "="), (Exp_ident "n"))),
                          (Exp_constant (Const_int 0)))),
                       (Exp_constant (Const_int 1)),
                       (Exp_apply ((Exp_ident "modd"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                             (Exp_constant (Const_int 1))))
                          ))
                       ))
                    ))
                 };
                { vb_pat = (Pat_var "modd");
                  vb_expr =
                  (Exp_function ((Pat_var "n"),
                     (Exp_ifthenelse (
                        (Exp_apply ((Exp_apply ((Exp_ident "="), (Exp_ident "n"))),
                           (Exp_constant (Const_int 0)))),
                        (Exp_constant (Const_int 1)),
                        (Exp_apply ((Exp_ident "meven"),
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                              (Exp_constant (Const_int 1))))
                           ))
                        ))
                     ))
                  }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "main");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_const Const_unit);
                          vb_expr =
                          (Exp_apply ((Exp_ident "print_int"),
                             (Exp_apply ((Exp_ident "modd"),
                                (Exp_constant (Const_int 1))))
                             ))
                          }
                         ]
                       )),
                    (Exp_let (
                       (Decl (Nonrecursive,
                          [{ vb_pat = (Pat_const Const_unit);
                             vb_expr =
                             (Exp_apply ((Exp_ident "print_int"),
                                (Exp_apply ((Exp_ident "meven"),
                                   (Exp_constant (Const_int 2))))
                                ))
                             }
                            ]
                          )),
                       (Exp_let (
                          (Decl (Nonrecursive,
                             [{ vb_pat =
                                (Pat_tuple [(Pat_var "even"); (Pat_var "odd")]);
                                vb_expr = (Exp_ident "tie") }
                               ]
                             )),
                          (Exp_let (
                             (Decl (Nonrecursive,
                                [{ vb_pat = (Pat_const Const_unit);
                                   vb_expr =
                                   (Exp_apply ((Exp_ident "print_int"),
                                      (Exp_apply ((Exp_ident "odd"),
                                         (Exp_constant (Const_int 3))))
                                      ))
                                   }
                                  ]
                                )),
                             (Exp_let (
                                (Decl (Nonrecursive,
                                   [{ vb_pat = (Pat_const Const_unit);
                                      vb_expr =
                                      (Exp_apply ((Exp_ident "print_int"),
                                         (Exp_apply ((Exp_ident "even"),
                                            (Exp_constant (Const_int 4))))
                                         ))
                                      }
                                     ]
                                   )),
                                (Exp_constant (Const_int 0))))
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
  let () =
    parse_test
      {|
let _1 = fun x y (a, _) -> (x + y - a) = 1

let _2 =
    let x, f::[] = 1, ( + ) 4 :: []
    in f x

let _3 =  (1, 2)::[]

let _4 =
    let a, _ = 1, 2, 3 in a

let int_of_option a = match a with x::[] -> x | [] -> 0

let _5 = let rec f x = f 5 in f

let _42 a = match a with 42 -> true | _ -> false
|}
  in
  [%expect
    {|
      [(Str_value
          (Decl (Nonrecursive,
             [{ vb_pat = (Pat_var "_1");
                vb_expr =
                (Exp_function ((Pat_var "x"),
                   (Exp_function ((Pat_var "y"),
                      (Exp_function ((Pat_tuple [(Pat_var "a"); Pat_any]),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "="),
                               (Exp_apply (
                                  (Exp_apply ((Exp_ident "-"),
                                     (Exp_apply (
                                        (Exp_apply ((Exp_ident "+"),
                                           (Exp_ident "x"))),
                                        (Exp_ident "y")))
                                     )),
                                  (Exp_ident "a")))
                               )),
                            (Exp_constant (Const_int 1))))
                         ))
                      ))
                   ))
                }
               ]
             )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "_2");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat =
                          (Pat_cons ((Pat_tuple [(Pat_var "x"); (Pat_var "f")]),
                             (Pat_const Const_nil)));
                          vb_expr =
                          (Exp_tuple
                             [(Exp_constant (Const_int 1));
                               (Exp_list (
                                  (Exp_apply ((Exp_ident "+"),
                                     (Exp_constant (Const_int 4)))),
                                  (Exp_constant Const_nil)))
                               ])
                          }
                         ]
                       )),
                    (Exp_apply ((Exp_ident "f"), (Exp_ident "x")))))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "_3");
                 vb_expr =
                 (Exp_list (
                    (Exp_tuple
                       [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 2))]),
                    (Exp_constant Const_nil)))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "_4");
                 vb_expr =
                 (Exp_let (
                    (Decl (Nonrecursive,
                       [{ vb_pat = (Pat_tuple [(Pat_var "a"); Pat_any]);
                          vb_expr =
                          (Exp_tuple
                             [(Exp_constant (Const_int 1));
                               (Exp_constant (Const_int 2));
                               (Exp_constant (Const_int 3))])
                          }
                         ]
                       )),
                    (Exp_ident "a")))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "int_of_option");
                 vb_expr =
                 (Exp_function ((Pat_var "a"),
                    (Exp_match ((Exp_ident "a"),
                       [((Pat_cons ((Pat_var "x"), (Pat_const Const_nil))),
                         (Exp_ident "x"));
                         ((Pat_const Const_nil), (Exp_constant (Const_int 0)))]
                       ))
                    ))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "_5");
                 vb_expr =
                 (Exp_let (
                    (Decl (Recursive,
                       [{ vb_pat = (Pat_var "f");
                          vb_expr =
                          (Exp_function ((Pat_var "x"),
                             (Exp_apply ((Exp_ident "f"),
                                (Exp_constant (Const_int 5))))
                             ))
                          }
                         ]
                       )),
                    (Exp_ident "f")))
                 }
                ]
              )));
        (Str_value
           (Decl (Nonrecursive,
              [{ vb_pat = (Pat_var "_42");
                 vb_expr =
                 (Exp_function ((Pat_var "a"),
                    (Exp_match ((Exp_ident "a"),
                       [((Pat_const (Const_int 42)),
                         (Exp_constant (Const_bool true)));
                         (Pat_any, (Exp_constant (Const_bool false)))]
                       ))
                    ))
                 }
                ]
              )))
        ]

      |}]
;;

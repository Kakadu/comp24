(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base.Result

let ( let* ) = ( >>= )

let alpha_test s =
  let ast'_t =
    let* ast = Parser.parse s in
    let* _ = Inferencer.check_program ast in
    let* ast' = Alpha_converter.rename_ast_with_uniq ast in
    let* _ = Inferencer.check_program ast' in
    Ok ast'
  in
  match ast'_t with
  | Ok ast' -> Format.printf "%s\n" Common.Ast.(show_program ast')
  | Error err ->
    (match err with
     | Parser e -> Parser.PP.pp_error Format.std_formatter e
     | Infer e -> Inferencer.PP.pp_error Format.std_formatter e
     | Alpha_converter (Illegal_state_error s) -> Format.print_string s)
;;

let%expect_test "" =
  let () = alpha_test {|
let a = 1;;
let b = 1;;
let c = a + b;;
  |} in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_var "a"); vb_expr = (Exp_constant (Const_int 1)) }])));
      (Str_value
         (Decl (Nonrecursive,
            [{ vb_pat = (Pat_var "b"); vb_expr = (Exp_constant (Const_int 1)) }]
            )));
      (Str_value
         (Decl (Nonrecursive,
            [{ vb_pat = (Pat_var "c");
               vb_expr =
               (Exp_apply ((Exp_apply ((Exp_ident "+"), (Exp_ident "a"))),
                  (Exp_ident "b")))
               }
              ]
            )))
      ] |}]
;;

let%expect_test "" =
  let () = alpha_test {|
let a = 1;;
let b = a;;
let c = 
  let a = b in
  a
;;
  |} in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_var "a"); vb_expr = (Exp_constant (Const_int 1)) }])));
      (Str_value
         (Decl (Nonrecursive,
            [{ vb_pat = (Pat_var "b"); vb_expr = (Exp_ident "a") }])));
      (Str_value
         (Decl (Nonrecursive,
            [{ vb_pat = (Pat_var "c");
               vb_expr =
               (Exp_let (
                  (Decl (Nonrecursive,
                     [{ vb_pat = (Pat_var "ac0_a"); vb_expr = (Exp_ident "b") }]
                     )),
                  (Exp_ident "ac0_a")))
               }
              ]
            )))
      ] |}]
;;

let%expect_test "" =
  let () = alpha_test {|
let rt_a = 1
let ll_b = rt_a
let main = 1
;;
  |} in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_var "ac_rt_a");
              vb_expr = (Exp_constant (Const_int 1)) }
             ]
           )));
      (Str_value
         (Decl (Nonrecursive,
            [{ vb_pat = (Pat_var "ac_ll_b"); vb_expr = (Exp_ident "ac_rt_a") }])));
      (Str_value
         (Decl (Nonrecursive,
            [{ vb_pat = (Pat_var "ac0_main");
               vb_expr = (Exp_constant (Const_int 1)) }
              ]
            )))
      ] |}]
;;

let%expect_test "" =
  let () = alpha_test {|
let rec (+) b c = c + b
;;
  |} in
  [%expect
    {|
    [(Str_value
        (Decl (Recursive,
           [{ vb_pat = (Pat_var "op_plus");
              vb_expr =
              (Exp_function ((Pat_var "b"),
                 (Exp_function ((Pat_var "c"),
                    (Exp_apply (
                       (Exp_apply ((Exp_ident "op_plus"), (Exp_ident "c"))),
                       (Exp_ident "b")))
                    ))
                 ))
              }
             ]
           )))
      ] |}]
;;

let%expect_test "" =
  let () = alpha_test {|
let (+) b c = c + b
let (+) b c = c + b
;;
  |} in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_var "op_plus");
              vb_expr =
              (Exp_function ((Pat_var "b"),
                 (Exp_function ((Pat_var "c"),
                    (Exp_apply ((Exp_apply ((Exp_ident "+"), (Exp_ident "c"))),
                       (Exp_ident "b")))
                    ))
                 ))
              }
             ]
           )));
      (Str_value
         (Decl (Nonrecursive,
            [{ vb_pat = (Pat_var "ac2_op_plus");
               vb_expr =
               (Exp_function ((Pat_var "ac0_b"),
                  (Exp_function ((Pat_var "ac1_c"),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "op_plus"), (Exp_ident "ac1_c"))),
                        (Exp_ident "ac0_b")))
                     ))
                  ))
               }
              ]
            )))
      ] |}]
;;

let%expect_test "" =
  let () =
    alpha_test
      {|
let rec even n =
  match n with
    | 0 -> true
    | x -> odd (x-1)
and odd n =
  match n with
    | 0 -> false
    | x -> even (x-1);;
  |}
  in
  [%expect
    {|
    [(Str_value
        (Decl (Recursive,
           [{ vb_pat = (Pat_var "even");
              vb_expr =
              (Exp_function ((Pat_var "n"),
                 (Exp_match ((Exp_ident "n"),
                    [((Pat_const (Const_int 0)), (Exp_constant (Const_bool true)));
                      ((Pat_var "x"),
                       (Exp_apply ((Exp_ident "odd"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "-"), (Exp_ident "x"))),
                             (Exp_constant (Const_int 1))))
                          )))
                      ]
                    ))
                 ))
              };
             { vb_pat = (Pat_var "odd");
               vb_expr =
               (Exp_function ((Pat_var "ac0_n"),
                  (Exp_match ((Exp_ident "ac0_n"),
                     [((Pat_const (Const_int 0)),
                       (Exp_constant (Const_bool false)));
                       ((Pat_var "ac1_x"),
                        (Exp_apply ((Exp_ident "even"),
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "-"), (Exp_ident "ac1_x"))),
                              (Exp_constant (Const_int 1))))
                           )))
                       ]
                     ))
                  ))
               }
             ]
           )))
      ] |}]
;;

let%expect_test "" =
  let () = alpha_test {|
let a = 10;;
let a, b = 1, 2;;
  |} in
  [%expect
    {|
    [(Str_value
        (Decl (Nonrecursive,
           [{ vb_pat = (Pat_var "a"); vb_expr = (Exp_constant (Const_int 10)) }]
           )));
      (Str_value
         (Decl (Nonrecursive,
            [{ vb_pat = (Pat_tuple [(Pat_var "ac0_a"); (Pat_var "b")]);
               vb_expr =
               (Exp_tuple
                  [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 2))])
               }
              ]
            )))
      ] |}]
;;

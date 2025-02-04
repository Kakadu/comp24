(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AefjnvMl_lib
open Parser

let parse_test s =
  match parse_prefix s with
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
  let () = parse_test {|not vbool ;;|} in
  [%expect {| [(Str_eval (Exp_apply ((Exp_ident "not"), (Exp_ident "vbool"))))] |}]
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
          (Exp_list ((Exp_apply ((Exp_ident "-"), (Exp_constant (Const_int 1)))),
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
        { d_rec = Recursive; d_pat = (Pat_var "fact");
          d_expr =
          (Exp_function ((Pat_var "k"),
             (Exp_function ((Pat_var "n"),
                (Exp_ifthenelse (
                   (Exp_apply ((Exp_apply ((Exp_ident "<="), (Exp_ident "n"))),
                      (Exp_constant (Const_int 1)))),
                   (Exp_apply ((Exp_ident "k"), (Exp_constant (Const_int 1)))),
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
          })
      ] |}]
;;

(* new functionality *)

let%expect_test _ =
  let () =
    parse_test "let (+) (a : bool) (b : int) c: int = funct (10 - b)"
  in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "+");
          d_expr =
          (Exp_function ((Pat_var "c"),
             (Exp_function ((Pat_constraint ((Pat_var "b"), Ptyp_int)),
                (Exp_function (
                   (Pat_constraint ((Pat_constraint ((Pat_var "a"), Ptyp_bool)),
                      Ptyp_int)),
                   (Exp_apply ((Exp_ident "funct"),
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "-"),
                            (Exp_constant (Const_int 10)))),
                         (Exp_ident "b")))
                      ))
                   ))
                ))
             ))
          })
      ] |}]
;;


(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open XaxaML

let parse_expr_and_print_ast str =
  match Parser.run_parser_expr str with
  | Result.Ok expr -> Format.printf "%a\n" Ast.pp_expr expr
  | Result.Error str -> Format.printf "Parsing error%s\n" str
;;

let%expect_test "simple algebra" =
  parse_expr_and_print_ast {|1 + 2 * 3|};
  [%expect
    {|
    (E_app ((E_app ((E_ident "+"), (E_const (C_int 1)))),
       (E_app ((E_app ((E_ident "*"), (E_const (C_int 2)))), (E_const (C_int 3))
          ))
       )) |}]
;;

let%expect_test "simple function" =
  parse_expr_and_print_ast {|fun a b c -> a + b + c|};
  [%expect
    {|
    (E_fun ((P_val "a"), [(P_val "b"); (P_val "c")],
       (E_app (
          (E_app ((E_ident "+"),
             (E_app ((E_app ((E_ident "+"), (E_ident "a"))), (E_ident "b"))))),
          (E_ident "c")))
       )) |}]
;;

let%expect_test "if then else" =
  parse_expr_and_print_ast {|if true then if true then a else b else 1|};
  [%expect
    {|
    (E_ite ((E_const (C_bool true)),
       (E_ite ((E_const (C_bool true)), (E_ident "a"), (E_ident "b"))),
       (E_const (C_int 1)))) |}]
;;

let%expect_test "apply" =
  parse_expr_and_print_ast {|f 1 2|};
  [%expect
    {|
    (E_app ((E_app ((E_ident "f"), (E_const (C_int 1)))), (E_const (C_int 2)))) |}]
;;

let%expect_test "" =
  parse_expr_and_print_ast {|1 + f 2|};
  [%expect
    {|
    (E_app ((E_app ((E_ident "+"), (E_const (C_int 1)))),
       (E_app ((E_ident "f"), (E_const (C_int 2)))))) |}]
;;

let%expect_test "tuple" =
  parse_expr_and_print_ast {|(1, x, 1+2, (1, false))|};
  [%expect
    {|
    (E_tuple ((E_const (C_int 1)),
       [(E_ident "x");
         (E_app ((E_app ((E_ident "+"), (E_const (C_int 1)))),
            (E_const (C_int 2))));
         (E_tuple ((E_const (C_int 1)), [(E_const (C_bool false))]))]
       )) |}]
;;

let%expect_test "list" =
  parse_expr_and_print_ast {|3::[1; 2]|};
  [%expect
    {|
      (E_cons_list ((E_const (C_int 3)),
         (E_cons_list ((E_const (C_int 1)),
            (E_cons_list ((E_const (C_int 2)), (E_const C_empty_list)))))
         )) |}]
;;

let%expect_test "match with 1" =
  parse_expr_and_print_ast {|match x with
      | h::tl -> 1
      | _ -> 0 |};
  [%expect
    {|
    (E_match ((E_ident "x"),
       [((P_cons_list ((P_val "h"), (P_val "tl"))), (E_const (C_int 1)));
         (P_any, (E_const (C_int 0)))]
       )) |}]
;;

let%expect_test "match with 2" =
  parse_expr_and_print_ast {|match x with (a, b) -> true | _ -> false |};
  [%expect
    {|
    (E_match ((E_ident "x"),
       [((P_tuple ((P_val "a"), [(P_val "b")])), (E_const (C_bool true)));
         (P_any, (E_const (C_bool false)))]
       )) |}]
;;

let%expect_test "empty list" =
  parse_expr_and_print_ast {|let f = [] in f|};
  [%expect
    {|
    (E_let ((Non_rec ((P_val "f"), None, (E_const C_empty_list))), (E_ident "f")
       )) |}]
;;

let%expect_test "pattern in argument 1" =
  parse_expr_and_print_ast {|let k (1, 2) = 3 in k|};
  [%expect
    {|
    (E_let (
       (Non_rec
          ((P_val "k"), None,
           (E_fun ((P_tuple ((P_const (C_int 1)), [(P_const (C_int 2))])),
              [], (E_const (C_int 3)))))),
       (E_ident "k"))) |}]
;;

let%expect_test "pattern in argument 2" =
  parse_expr_and_print_ast {|let f (h :: tl) = 1 in f|};
  [%expect
    {|
  (E_let (
     (Non_rec
        ((P_val "f"), None,
         (E_fun ((P_cons_list ((P_val "h"), (P_val "tl"))), [],
            (E_const (C_int 1)))))),
     (E_ident "f"))) |}]
;;

let%expect_test "type 1" =
  parse_expr_and_print_ast {|(123 : ('a -> 'b) -> ('cd -> 'a0) )|};
  [%expect
    {|
    (E_typed ((E_const (C_int 123)),
       (RT_arr ((RT_arr ((RT_var "a"), (RT_var "b"))),
          (RT_arr ((RT_var "cd"), (RT_var "a0")))))
       )) |}]
;;

let%expect_test "type 2" =
  parse_expr_and_print_ast {|(fun (x : int) -> x + 1 : int -> int) |};
  [%expect
    {|
    (E_typed (
       (E_fun ((P_typed ((P_val "x"), (RT_prim "int"))), [],
          (E_app ((E_app ((E_ident "+"), (E_ident "x"))), (E_const (C_int 1)))))),
       (RT_arr ((RT_prim "int"), (RT_prim "int"))))) |}]
;;

let%expect_test "type 3" =
  parse_expr_and_print_ast {| let a : int = 1 in a |};
  [%expect
    {|
    (E_let ((Non_rec ((P_val "a"), (Some (RT_prim "int")), (E_const (C_int 1)))),
       (E_ident "a"))) |}]
;;

let%expect_test "type 4" =
  parse_expr_and_print_ast {| let inc (x : int) : int = x + 1 in inc |};
  [%expect
    {|
    (E_let (
       (Non_rec
          ((P_val "inc"), (Some (RT_arr ((RT_prim "int"), (RT_prim "int")))),
           (E_fun ((P_typed ((P_val "x"), (RT_prim "int"))), [],
              (E_app ((E_app ((E_ident "+"), (E_ident "x"))), (E_const (C_int 1))
                 ))
              )))),
       (E_ident "inc"))) |}]
;;

let%expect_test "rec and" =
  parse_expr_and_print_ast {|let rec f a = 1 and t b = 1 in 1|};
  [%expect
    {|
    (E_let (
       (Rec
          [((P_val "f"), None, (E_fun ((P_val "a"), [], (E_const (C_int 1)))));
            ((P_val "t"), None, (E_fun ((P_val "b"), [], (E_const (C_int 1)))))]),
       (E_const (C_int 1)))) |}]
;;

let%expect_test "rec and typed" =
  parse_expr_and_print_ast {|let rec f a : int = 1 and t b : int = 1 in 1|};
  [%expect
    {|
    (E_let (
       (Rec
          [((P_val "f"), (Some (RT_arr ((RT_var "0"), (RT_prim "int")))),
            (E_fun ((P_val "a"), [], (E_const (C_int 1)))));
            ((P_val "t"), (Some (RT_arr ((RT_var "0"), (RT_prim "int")))),
             (E_fun ((P_val "b"), [], (E_const (C_int 1)))))
            ]),
       (E_const (C_int 1)))) |}]
;;

let%expect_test "type 5" =
  parse_expr_and_print_ast {|(fun x -> x : 'a -> 'a)|};
  [%expect
    {|
    (E_typed ((E_fun ((P_val "x"), [], (E_ident "x"))),
       (RT_arr ((RT_var "a"), (RT_var "a"))))) |}]
;;

let%expect_test "apply" =
  parse_expr_and_print_ast {|let f x g = g x in f|};
  [%expect
    {|
    (E_let (
       (Non_rec
          ((P_val "f"), None,
           (E_fun ((P_val "x"), [(P_val "g")],
              (E_app ((E_ident "g"), (E_ident "x"))))))),
       (E_ident "f"))) |}]
;;

let%expect_test "operator as func" =
  parse_expr_and_print_ast {|let s = (+) 1 2 in (-)|};
  [%expect
    {|
    (E_let (
       (Non_rec
          ((P_val "s"), None,
           (E_app ((E_app ((E_ident "+"), (E_const (C_int 1)))),
              (E_const (C_int 2)))))),
       (E_ident "-"))) |}]
;;

(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Kreml_lib.Parser
open Kreml_lib.Ast

let%expect_test "patterns test" =
  let cases =
    [ "123"
    ; "-1"
    ; "true"
    ; "false"
    ; "x"
    ; "some_long_name"
    ; "letrec"
    ; "_"
    ; "(a)"
    ; "a, b"
    ; "a ,  b,  c"
    ; "a, (b,c), true"
    ; "((a, a), b), c"
    ; "(a, b)"
    ; "((a, b), c, (5, true))"
    ; "x::xs"
    ; "(a, (b, c))::rest"
    ; "x::y::rest"
    ; "1::y::_::xs"
    ; "x::xs, y::ys"
    ; "(x)::(xs)"
    ; "(a, b)::xs"
    ; "a,b::xs"
    ; "(a,b)::xs"
    ; "(a : int)"
    ; "(a, b : int * bool)"
    ; "((a, b) : int * int)"
    ; "(f : int -> int)"
    ; "(f : int -> int -> bool)"
    ; "(a : (int -> int) -> int)"
    ]
  in
  List.iter
    (fun i -> print_endline (show_res ~input:i ~parser:pattern ~to_string:show_pattern))
    cases;
  [%expect
    {|
    (Ast.Pat_const (Ast.Const_int 123))
    (Ast.Pat_const (Ast.Const_int -1))
    (Ast.Pat_const (Ast.Const_bool true))
    (Ast.Pat_const (Ast.Const_bool false))
    (Ast.Pat_var "x")
    (Ast.Pat_var "some_long_name")
    (Ast.Pat_var "letrec")
    Ast.Pat_wildcard
    (Ast.Pat_var "a")
    (Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), []))
    (Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), [(Ast.Pat_var "c")]))
    (Ast.Pat_tuple ((Ast.Pat_var "a"),
       (Ast.Pat_tuple ((Ast.Pat_var "b"), (Ast.Pat_var "c"), [])),
       [(Ast.Pat_const (Ast.Const_bool true))]))
    (Ast.Pat_tuple (
       (Ast.Pat_tuple (
          (Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "a"), [])),
          (Ast.Pat_var "b"), [])),
       (Ast.Pat_var "c"), []))
    (Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), []))
    (Ast.Pat_tuple ((Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), [])),
       (Ast.Pat_var "c"),
       [(Ast.Pat_tuple ((Ast.Pat_const (Ast.Const_int 5)),
           (Ast.Pat_const (Ast.Const_bool true)), []))
         ]
       ))
    (Ast.Pat_cons ((Ast.Pat_var "x"), (Ast.Pat_var "xs")))
    (Ast.Pat_cons (
       (Ast.Pat_tuple ((Ast.Pat_var "a"),
          (Ast.Pat_tuple ((Ast.Pat_var "b"), (Ast.Pat_var "c"), [])), [])),
       (Ast.Pat_var "rest")))
    (Ast.Pat_cons ((Ast.Pat_var "x"),
       (Ast.Pat_cons ((Ast.Pat_var "y"), (Ast.Pat_var "rest")))))
    (Ast.Pat_cons ((Ast.Pat_const (Ast.Const_int 1)),
       (Ast.Pat_cons ((Ast.Pat_var "y"),
          (Ast.Pat_cons (Ast.Pat_wildcard, (Ast.Pat_var "xs")))))
       ))
    (Ast.Pat_tuple ((Ast.Pat_cons ((Ast.Pat_var "x"), (Ast.Pat_var "xs"))),
       (Ast.Pat_cons ((Ast.Pat_var "y"), (Ast.Pat_var "ys"))), []))
    (Ast.Pat_cons ((Ast.Pat_var "x"), (Ast.Pat_var "xs")))
    (Ast.Pat_cons ((Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), [])),
       (Ast.Pat_var "xs")))
    (Ast.Pat_tuple ((Ast.Pat_var "a"),
       (Ast.Pat_cons ((Ast.Pat_var "b"), (Ast.Pat_var "xs"))), []))
    (Ast.Pat_cons ((Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), [])),
       (Ast.Pat_var "xs")))
    (Ast.Pat_constrained ((Ast.Pat_var "a"), Ast.Typ_int))
    (Ast.Pat_constrained (
       (Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), [])),
       (Ast.Typ_tuple (Ast.Typ_int, Ast.Typ_bool, []))))
    (Ast.Pat_constrained (
       (Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), [])),
       (Ast.Typ_tuple (Ast.Typ_int, Ast.Typ_int, []))))
    (Ast.Pat_constrained ((Ast.Pat_var "f"),
       (Ast.Typ_fun (Ast.Typ_int, Ast.Typ_int))))
    (Ast.Pat_constrained ((Ast.Pat_var "f"),
       (Ast.Typ_fun (Ast.Typ_int, (Ast.Typ_fun (Ast.Typ_int, Ast.Typ_bool))))))
    (Ast.Pat_constrained ((Ast.Pat_var "a"),
       (Ast.Typ_fun ((Ast.Typ_fun (Ast.Typ_int, Ast.Typ_int)), Ast.Typ_int)))) |}]
;;

let%expect_test "operations" =
  let inputs =
    [ "a"
    ; "a+b"
    ; "a+b+c+d+e"
    ; "a+b*c"
    ; "a/b*c"
    ; "a*b*c-d"
    ; "a<=b"
    ; "x <= z + w"
    ; "(a+b)*c-(x + y) >= u"
    ; "a + b = c && x + (y-w) >= k || a = b"
    ]
  in
  List.iter
    (fun i ->
       show_res ~input:i ~parser:(expr_with_ops ident_as_expr) ~to_string:show_expr
       |> print_endline)
    inputs;
  [%expect
    {|
    (Ast.Expr_var "a")
    (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "a"))),
       (Ast.Expr_var "b")))
    (Ast.Expr_app (
       (Ast.Expr_app ((Ast.Expr_var "+"),
          (Ast.Expr_app (
             (Ast.Expr_app ((Ast.Expr_var "+"),
                (Ast.Expr_app (
                   (Ast.Expr_app ((Ast.Expr_var "+"),
                      (Ast.Expr_app (
                         (Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "a"))),
                         (Ast.Expr_var "b")))
                      )),
                   (Ast.Expr_var "c")))
                )),
             (Ast.Expr_var "d")))
          )),
       (Ast.Expr_var "e")))
    (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "a"))),
       (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "*"), (Ast.Expr_var "b"))),
          (Ast.Expr_var "c")))
       ))
    (Ast.Expr_app (
       (Ast.Expr_app ((Ast.Expr_var "*"),
          (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "/"), (Ast.Expr_var "a"))),
             (Ast.Expr_var "b")))
          )),
       (Ast.Expr_var "c")))
    (Ast.Expr_app (
       (Ast.Expr_app ((Ast.Expr_var "-"),
          (Ast.Expr_app (
             (Ast.Expr_app ((Ast.Expr_var "*"),
                (Ast.Expr_app (
                   (Ast.Expr_app ((Ast.Expr_var "*"), (Ast.Expr_var "a"))),
                   (Ast.Expr_var "b")))
                )),
             (Ast.Expr_var "c")))
          )),
       (Ast.Expr_var "d")))
    (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "<="), (Ast.Expr_var "a"))),
       (Ast.Expr_var "b")))
    (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "<="), (Ast.Expr_var "x"))),
       (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "z"))),
          (Ast.Expr_var "w")))
       ))
    (Ast.Expr_app (
       (Ast.Expr_app ((Ast.Expr_var ">="),
          (Ast.Expr_app (
             (Ast.Expr_app ((Ast.Expr_var "-"),
                (Ast.Expr_app (
                   (Ast.Expr_app ((Ast.Expr_var "*"),
                      (Ast.Expr_app (
                         (Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "a"))),
                         (Ast.Expr_var "b")))
                      )),
                   (Ast.Expr_var "c")))
                )),
             (Ast.Expr_app (
                (Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "x"))),
                (Ast.Expr_var "y")))
             ))
          )),
       (Ast.Expr_var "u")))
    (Ast.Expr_app (
       (Ast.Expr_app ((Ast.Expr_var "||"),
          (Ast.Expr_app (
             (Ast.Expr_app ((Ast.Expr_var "&&"),
                (Ast.Expr_app (
                   (Ast.Expr_app ((Ast.Expr_var "="),
                      (Ast.Expr_app (
                         (Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "a"))),
                         (Ast.Expr_var "b")))
                      )),
                   (Ast.Expr_var "c")))
                )),
             (Ast.Expr_app (
                (Ast.Expr_app ((Ast.Expr_var ">="),
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "x"))),
                      (Ast.Expr_app (
                         (Ast.Expr_app ((Ast.Expr_var "-"), (Ast.Expr_var "y"))),
                         (Ast.Expr_var "w")))
                      ))
                   )),
                (Ast.Expr_var "k")))
             ))
          )),
       (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "="), (Ast.Expr_var "a"))),
          (Ast.Expr_var "b")))
       )) |}]
;;

let%expect_test "simple expressions" =
  let cases =
    [ "5"
    ; "5 - 6"
    ; "5, 6"
    ; "x::xs"
    ; "[5; 6; 7 + 8]"
    ; "1::[2;3]"
    ; "if true then a else b"
    ; "fun x -> fun y -> x+y"
    ; "[fun x -> x + 1; fun x -> x + y]"
    ; "let f = fun x y -> x + y in f z 56"
    ; "(fun y -> y) 1 "
    ]
  in
  List.iter
    (fun i -> show_res ~input:i ~parser:expr ~to_string:show_expr |> print_endline)
    cases;
  [%expect
    {|
    (Ast.Expr_const (Ast.Const_int 5))
    (Ast.Expr_app (
       (Ast.Expr_app ((Ast.Expr_var "-"), (Ast.Expr_const (Ast.Const_int 5)))),
       (Ast.Expr_const (Ast.Const_int 6))))
    (Ast.Expr_tuple ((Ast.Expr_const (Ast.Const_int 5)),
       (Ast.Expr_const (Ast.Const_int 6)), []))
    (Ast.Expr_cons ((Ast.Expr_var "x"), (Ast.Expr_var "xs")))
    (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 5)),
       (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 6)),
          (Ast.Expr_cons (
             (Ast.Expr_app (
                (Ast.Expr_app ((Ast.Expr_var "+"),
                   (Ast.Expr_const (Ast.Const_int 7)))),
                (Ast.Expr_const (Ast.Const_int 8)))),
             (Ast.Expr_const Ast.Const_nil)))
          ))
       ))
    (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 1)),
       (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 2)),
          (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 3)),
             (Ast.Expr_const Ast.Const_nil)))
          ))
       ))
    (Ast.Expr_ite ((Ast.Expr_const (Ast.Const_bool true)), (Ast.Expr_var "a"),
       (Ast.Expr_var "b")))
    (Ast.Expr_fun ((Ast.Pat_var "x"),
       (Ast.Expr_fun ((Ast.Pat_var "y"),
          (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "x"))),
             (Ast.Expr_var "y")))
          ))
       ))
    (Ast.Expr_cons (
       (Ast.Expr_fun ((Ast.Pat_var "x"),
          (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "x"))),
             (Ast.Expr_const (Ast.Const_int 1))))
          )),
       (Ast.Expr_cons (
          (Ast.Expr_fun ((Ast.Pat_var "x"),
             (Ast.Expr_app (
                (Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "x"))),
                (Ast.Expr_var "y")))
             )),
          (Ast.Expr_const Ast.Const_nil)))
       ))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "f"),
        (Ast.Expr_fun ((Ast.Pat_var "x"),
           (Ast.Expr_fun ((Ast.Pat_var "y"),
              (Ast.Expr_app (
                 (Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "x"))),
                 (Ast.Expr_var "y")))
              ))
           ))),
       (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "f"), (Ast.Expr_var "z"))),
          (Ast.Expr_const (Ast.Const_int 56))))
       ))
    (Ast.Expr_app ((Ast.Expr_fun ((Ast.Pat_var "y"), (Ast.Expr_var "y"))),
       (Ast.Expr_const (Ast.Const_int 1)))) |}]
;;

let%expect_test "let bindings" =
  let cases =
    [ "let a = 5 in a"
    ; "let a = 5 in let b = 6 in a + b"
    ; "let f x y = x::y in f 5 6"
    ; "let f g h = g, h in f"
    ; "let a = (fun x -> fun y -> x + y) in c "
    ; "let a = (f) a b in 0"
    ]
  in
  List.iter
    (fun input -> show_res ~input ~parser:expr ~to_string:show_expr |> print_endline)
    cases;
  [%expect
    {|
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "a"), (Ast.Expr_const (Ast.Const_int 5))),
       (Ast.Expr_var "a")))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "a"), (Ast.Expr_const (Ast.Const_int 5))),
       (Ast.Expr_let (Ast.NonRecursive,
          ((Ast.Pat_var "b"), (Ast.Expr_const (Ast.Const_int 6))),
          (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "a"))),
             (Ast.Expr_var "b")))
          ))
       ))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "f"),
        (Ast.Expr_fun ((Ast.Pat_var "x"),
           (Ast.Expr_fun ((Ast.Pat_var "y"),
              (Ast.Expr_cons ((Ast.Expr_var "x"), (Ast.Expr_var "y")))))
           ))),
       (Ast.Expr_app (
          (Ast.Expr_app ((Ast.Expr_var "f"), (Ast.Expr_const (Ast.Const_int 5)))),
          (Ast.Expr_const (Ast.Const_int 6))))
       ))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "f"),
        (Ast.Expr_fun ((Ast.Pat_var "g"),
           (Ast.Expr_fun ((Ast.Pat_var "h"),
              (Ast.Expr_tuple ((Ast.Expr_var "g"), (Ast.Expr_var "h"), []))))
           ))),
       (Ast.Expr_var "f")))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "a"),
        (Ast.Expr_fun ((Ast.Pat_var "x"),
           (Ast.Expr_fun ((Ast.Pat_var "y"),
              (Ast.Expr_app (
                 (Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "x"))),
                 (Ast.Expr_var "y")))
              ))
           ))),
       (Ast.Expr_var "c")))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "a"),
        (Ast.Expr_app ((Ast.Expr_app ((Ast.Expr_var "f"), (Ast.Expr_var "a"))),
           (Ast.Expr_var "b")))),
       (Ast.Expr_const (Ast.Const_int 0)))) |}]
;;

let%expect_test "match" =
  let cases =
    [ "match x with | 1 -> 1 | 2 -> 2 | _ -> 42"
    ; "match l with | x::y::rest -> 1 | x::y -> 2 | _ -> 3"
    ; "match (a, b) with | (5, 6) -> 1 | (_, 6) -> 6 | (5, _) -> 5 | (_, _)  -> 1337"
    ; "match wnPsvbc_Wek_ilur with | (true : unit -> int)  -> false ::true "
    ]
  in
  List.iter
    (fun input -> show_res ~input ~parser:expr ~to_string:show_expr |> print_endline)
    cases;
  [%expect
    {|
    (Ast.Expr_match ((Ast.Expr_var "x"),
       [((Ast.Pat_const (Ast.Const_int 1)), (Ast.Expr_const (Ast.Const_int 1)));
         ((Ast.Pat_const (Ast.Const_int 2)), (Ast.Expr_const (Ast.Const_int 2)));
         (Ast.Pat_wildcard, (Ast.Expr_const (Ast.Const_int 42)))]
       ))
    (Ast.Expr_match ((Ast.Expr_var "l"),
       [((Ast.Pat_cons ((Ast.Pat_var "x"),
            (Ast.Pat_cons ((Ast.Pat_var "y"), (Ast.Pat_var "rest"))))),
         (Ast.Expr_const (Ast.Const_int 1)));
         ((Ast.Pat_cons ((Ast.Pat_var "x"), (Ast.Pat_var "y"))),
          (Ast.Expr_const (Ast.Const_int 2)));
         (Ast.Pat_wildcard, (Ast.Expr_const (Ast.Const_int 3)))]
       ))
    (Ast.Expr_match (
       (Ast.Expr_tuple ((Ast.Expr_var "a"), (Ast.Expr_var "b"), [])),
       [((Ast.Pat_tuple ((Ast.Pat_const (Ast.Const_int 5)),
            (Ast.Pat_const (Ast.Const_int 6)), [])),
         (Ast.Expr_const (Ast.Const_int 1)));
         ((Ast.Pat_tuple (Ast.Pat_wildcard, (Ast.Pat_const (Ast.Const_int 6)),
             [])),
          (Ast.Expr_const (Ast.Const_int 6)));
         ((Ast.Pat_tuple ((Ast.Pat_const (Ast.Const_int 5)), Ast.Pat_wildcard,
             [])),
          (Ast.Expr_const (Ast.Const_int 5)));
         ((Ast.Pat_tuple (Ast.Pat_wildcard, Ast.Pat_wildcard, [])),
          (Ast.Expr_const (Ast.Const_int 1337)))
         ]
       ))
    (Ast.Expr_match ((Ast.Expr_var "wnPsvbc_Wek_ilur"),
       [((Ast.Pat_constrained ((Ast.Pat_const (Ast.Const_bool true)),
            (Ast.Typ_fun (Ast.Typ_unit, Ast.Typ_int)))),
         (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_bool false)),
            (Ast.Expr_const (Ast.Const_bool true)))))
         ]
       )) |}]
;;

let%expect_test "complex expr" =
  let cases =
    [ "let f a b = if a > 0 then [a; a] else match b with | x::y::_ -> y | _ -> 42 in f \
       5 [6; 7]"
    ; "let f a = 5 + a in f 6"
    ; "let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f)) in\n\
      \      fix 3"
    ]
  in
  List.iter
    (fun input -> show_res ~input ~parser:expr ~to_string:show_expr |> print_endline)
    cases;
  [%expect
    {|
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "f"),
        (Ast.Expr_fun ((Ast.Pat_var "a"),
           (Ast.Expr_fun ((Ast.Pat_var "b"),
              (Ast.Expr_ite (
                 (Ast.Expr_app (
                    (Ast.Expr_app ((Ast.Expr_var ">"), (Ast.Expr_var "a"))),
                    (Ast.Expr_const (Ast.Const_int 0)))),
                 (Ast.Expr_cons ((Ast.Expr_var "a"),
                    (Ast.Expr_cons ((Ast.Expr_var "a"),
                       (Ast.Expr_const Ast.Const_nil)))
                    )),
                 (Ast.Expr_match ((Ast.Expr_var "b"),
                    [((Ast.Pat_cons ((Ast.Pat_var "x"),
                         (Ast.Pat_cons ((Ast.Pat_var "y"), Ast.Pat_wildcard)))),
                      (Ast.Expr_var "y"));
                      (Ast.Pat_wildcard, (Ast.Expr_const (Ast.Const_int 42)))]
                    ))
                 ))
              ))
           ))),
       (Ast.Expr_app (
          (Ast.Expr_app ((Ast.Expr_var "f"), (Ast.Expr_const (Ast.Const_int 5)))),
          (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 6)),
             (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 7)),
                (Ast.Expr_const Ast.Const_nil)))
             ))
          ))
       ))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "f"),
        (Ast.Expr_fun ((Ast.Pat_var "a"),
           (Ast.Expr_app (
              (Ast.Expr_app ((Ast.Expr_var "+"),
                 (Ast.Expr_const (Ast.Const_int 5)))),
              (Ast.Expr_var "a")))
           ))),
       (Ast.Expr_app ((Ast.Expr_var "f"), (Ast.Expr_const (Ast.Const_int 6))))))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "fix"),
        (Ast.Expr_fun ((Ast.Pat_var "f"),
           (Ast.Expr_app (
              (Ast.Expr_fun ((Ast.Pat_var "x"),
                 (Ast.Expr_app ((Ast.Expr_var "f"),
                    (Ast.Expr_fun ((Ast.Pat_var "f"),
                       (Ast.Expr_app (
                          (Ast.Expr_app ((Ast.Expr_var "x"), (Ast.Expr_var "x"))),
                          (Ast.Expr_var "f")))
                       ))
                    ))
                 )),
              (Ast.Expr_fun ((Ast.Pat_var "x"),
                 (Ast.Expr_app ((Ast.Expr_var "f"),
                    (Ast.Expr_fun ((Ast.Pat_var "f"),
                       (Ast.Expr_app (
                          (Ast.Expr_app ((Ast.Expr_var "x"), (Ast.Expr_var "x"))),
                          (Ast.Expr_var "f")))
                       ))
                    ))
                 ))
              ))
           ))),
       (Ast.Expr_app ((Ast.Expr_var "fix"), (Ast.Expr_const (Ast.Const_int 3))))
       )) |}]
;;

let%expect_test "fold" =
  let input =
    "let rec fold l folder init =\n\
    \         match l with\n\
    \         | x::xs ->\n\
    \            let acc = folder init x in\n\
    \            fold xs folder acc\n\
    \         | [] -> init"
  in
  show_res ~input ~parser:program ~to_string:show_structure |> print_endline;
  [%expect
    {|
    [(Ast.Str_value (Ast.Recursive,
        [((Ast.Pat_var "fold"),
          (Ast.Expr_fun ((Ast.Pat_var "l"),
             (Ast.Expr_fun ((Ast.Pat_var "folder"),
                (Ast.Expr_fun ((Ast.Pat_var "init"),
                   (Ast.Expr_match ((Ast.Expr_var "l"),
                      [((Ast.Pat_cons ((Ast.Pat_var "x"), (Ast.Pat_var "xs"))),
                        (Ast.Expr_let (Ast.NonRecursive,
                           ((Ast.Pat_var "acc"),
                            (Ast.Expr_app (
                               (Ast.Expr_app ((Ast.Expr_var "folder"),
                                  (Ast.Expr_var "init"))),
                               (Ast.Expr_var "x")))),
                           (Ast.Expr_app (
                              (Ast.Expr_app (
                                 (Ast.Expr_app ((Ast.Expr_var "fold"),
                                    (Ast.Expr_var "xs"))),
                                 (Ast.Expr_var "folder"))),
                              (Ast.Expr_var "acc")))
                           )));
                        ((Ast.Pat_const Ast.Const_nil), (Ast.Expr_var "init"))]
                      ))
                   ))
                ))
             )))
          ]
        ))
      ] |}]
;;

let%expect_test "factorial" =
  let input = "let f n = if n > 0 then\n          n * f (n - 1)\n          else 1" in
  show_res ~input ~parser:program ~to_string:show_structure |> print_endline;
  [%expect
    {|
    [(Ast.Str_value (Ast.NonRecursive,
        [((Ast.Pat_var "f"),
          (Ast.Expr_fun ((Ast.Pat_var "n"),
             (Ast.Expr_ite (
                (Ast.Expr_app (
                   (Ast.Expr_app ((Ast.Expr_var ">"), (Ast.Expr_var "n"))),
                   (Ast.Expr_const (Ast.Const_int 0)))),
                (Ast.Expr_app (
                   (Ast.Expr_app ((Ast.Expr_var "*"), (Ast.Expr_var "n"))),
                   (Ast.Expr_app ((Ast.Expr_var "f"),
                      (Ast.Expr_app (
                         (Ast.Expr_app ((Ast.Expr_var "-"), (Ast.Expr_var "n"))),
                         (Ast.Expr_const (Ast.Const_int 1))))
                      ))
                   )),
                (Ast.Expr_const (Ast.Const_int 1))))
             )))
          ]
        ))
      ] |}]
;;

let%expect_test "even_odd" =
  let input =
    "let rec is_even n =\n\
    \         if n = 0 then true\n\
    \         else if n = 1 then false\n\
    \         else is_odd (n - 1)\n\
    \      and is_odd n =\n\
    \         if n = 1 then true\n\
    \         else if n = 0 then false\n\
    \         else is_odd (n - 1)"
  in
  show_res ~input ~parser:program ~to_string:show_structure |> print_endline;
  [%expect
    {|
    [(Ast.Str_value (Ast.Recursive,
        [((Ast.Pat_var "is_even"),
          (Ast.Expr_fun ((Ast.Pat_var "n"),
             (Ast.Expr_ite (
                (Ast.Expr_app (
                   (Ast.Expr_app ((Ast.Expr_var "="), (Ast.Expr_var "n"))),
                   (Ast.Expr_const (Ast.Const_int 0)))),
                (Ast.Expr_const (Ast.Const_bool true)),
                (Ast.Expr_ite (
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "="), (Ast.Expr_var "n"))),
                      (Ast.Expr_const (Ast.Const_int 1)))),
                   (Ast.Expr_const (Ast.Const_bool false)),
                   (Ast.Expr_app ((Ast.Expr_var "is_odd"),
                      (Ast.Expr_app (
                         (Ast.Expr_app ((Ast.Expr_var "-"), (Ast.Expr_var "n"))),
                         (Ast.Expr_const (Ast.Const_int 1))))
                      ))
                   ))
                ))
             )));
          ((Ast.Pat_var "is_odd"),
           (Ast.Expr_fun ((Ast.Pat_var "n"),
              (Ast.Expr_ite (
                 (Ast.Expr_app (
                    (Ast.Expr_app ((Ast.Expr_var "="), (Ast.Expr_var "n"))),
                    (Ast.Expr_const (Ast.Const_int 1)))),
                 (Ast.Expr_const (Ast.Const_bool true)),
                 (Ast.Expr_ite (
                    (Ast.Expr_app (
                       (Ast.Expr_app ((Ast.Expr_var "="), (Ast.Expr_var "n"))),
                       (Ast.Expr_const (Ast.Const_int 0)))),
                    (Ast.Expr_const (Ast.Const_bool false)),
                    (Ast.Expr_app ((Ast.Expr_var "is_odd"),
                       (Ast.Expr_app (
                          (Ast.Expr_app ((Ast.Expr_var "-"), (Ast.Expr_var "n"))),
                          (Ast.Expr_const (Ast.Const_int 1))))
                       ))
                    ))
                 ))
              )))
          ]
        ))
      ] |}]
;;

let%expect_test "typed" =
  let cases =
    [ "let _start () () a () b _c () d __ =\n\
      \  let () = print_int (a+b) in\n\
      \  let () = print_int __ in\n\
      \  a*b / _c + d"
    ; "let somefun \n\
      \        (a : int)\n\
      \        (b: int * int * bool)\n\
      \        (c: (int * int) list)\n\
      \        (f : (int -> int) -> int -> int) : int =\n\
      \         todo"
    ]
  in
  List.iter
    (fun i ->
       show_res ~input:i ~parser:program ~to_string:show_structure |> print_endline)
    cases;
  [%expect
    {|
    [(Ast.Str_value (Ast.NonRecursive,
        [((Ast.Pat_var "_start"),
          (Ast.Expr_fun ((Ast.Pat_const Ast.Const_unit),
             (Ast.Expr_fun ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_fun ((Ast.Pat_var "a"),
                   (Ast.Expr_fun ((Ast.Pat_const Ast.Const_unit),
                      (Ast.Expr_fun ((Ast.Pat_var "b"),
                         (Ast.Expr_fun ((Ast.Pat_var "_c"),
                            (Ast.Expr_fun ((Ast.Pat_const Ast.Const_unit),
                               (Ast.Expr_fun ((Ast.Pat_var "d"),
                                  (Ast.Expr_fun ((Ast.Pat_var "__"),
                                     (Ast.Expr_let (Ast.NonRecursive,
                                        ((Ast.Pat_const Ast.Const_unit),
                                         (Ast.Expr_app (
                                            (Ast.Expr_var "print_int"),
                                            (Ast.Expr_app (
                                               (Ast.Expr_app ((Ast.Expr_var "+"),
                                                  (Ast.Expr_var "a"))),
                                               (Ast.Expr_var "b")))
                                            ))),
                                        (Ast.Expr_let (Ast.NonRecursive,
                                           ((Ast.Pat_const Ast.Const_unit),
                                            (Ast.Expr_app (
                                               (Ast.Expr_var "print_int"),
                                               (Ast.Expr_var "__")))),
                                           (Ast.Expr_app (
                                              (Ast.Expr_app ((Ast.Expr_var "+"),
                                                 (Ast.Expr_app (
                                                    (Ast.Expr_app (
                                                       (Ast.Expr_var "/"),
                                                       (Ast.Expr_app (
                                                          (Ast.Expr_app (
                                                             (Ast.Expr_var "*"),
                                                             (Ast.Expr_var "a"))),
                                                          (Ast.Expr_var "b")))
                                                       )),
                                                    (Ast.Expr_var "_c")))
                                                 )),
                                              (Ast.Expr_var "d")))
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
             )))
          ]
        ))
      ]
    [(Ast.Str_value (Ast.NonRecursive,
        [((Ast.Pat_var "somefun"),
          (Ast.Expr_fun ((Ast.Pat_constrained ((Ast.Pat_var "a"), Ast.Typ_int)),
             (Ast.Expr_fun (
                (Ast.Pat_constrained ((Ast.Pat_var "b"),
                   (Ast.Typ_tuple (Ast.Typ_int, Ast.Typ_int, [Ast.Typ_bool])))),
                (Ast.Expr_fun (
                   (Ast.Pat_constrained ((Ast.Pat_var "c"),
                      (Ast.Typ_list
                         (Ast.Typ_tuple (Ast.Typ_int, Ast.Typ_int, [])))
                      )),
                   (Ast.Expr_fun (
                      (Ast.Pat_constrained ((Ast.Pat_var "f"),
                         (Ast.Typ_fun ((Ast.Typ_fun (Ast.Typ_int, Ast.Typ_int)),
                            (Ast.Typ_fun (Ast.Typ_int, Ast.Typ_int))))
                         )),
                      (Ast.Expr_constrained ((Ast.Expr_var "todo"), Ast.Typ_int))
                      ))
                   ))
                ))
             )))
          ]
        ))
      ] |}]
;;

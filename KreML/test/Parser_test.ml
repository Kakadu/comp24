open Kreml_lib.Parser
open Kreml_lib.Ast

let%expect_test "patterns test" =
  let cases = ["123"; "true"; "false"
    ; "x"; "some_long_name"; "letrec"; "_"
    ; "(a)"
    ; "a, b"; "a ,  b,  c"; "a, (b,c), true"; "((a, a), b), c"
    ; "(a, b)"; "((a, b), c, (5, true))"
    ; "x::xs"; "(a, (b, c))::rest"; "x::y::rest"; "1::y::_::xs"
    ; "x::xs, y::ys"; "(x)::(xs)"; "a,b::xs"; "(a,b)::xs"
    ; "a : int"; "a, b : int * bool"; "(a, b : int * int)"; "f : int -> int"; "f : int -> int -> bool"; "a : (int -> int) -> int"]
   in
    List.iter (fun i -> print_endline (show_res ~input:i ~parser:pattern ~to_string:show_pattern)) cases;
  [%expect {|
    (Ast.Pat_const (Ast.Const_int 123))
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


let%expect_test "operations" =
  let inputs = ["a"; "a+b"; "a+b+c+d+e";
    "a+b*c"; "a/b*c"; "a*b*c-d";
    "a<=b"; "x <= z + w";
    "(a+b)*c-(x + y) >= u";
    "a + b = c && x + (y-w) >= k || a = b"] in
  List.iter (fun i -> show_res ~input:i ~parser:(expr_with_ops ident_as_expr) ~to_string:show_expr |> print_endline ) inputs;
  [%expect {|
    (Ast.Expr_var "a")
    (Ast.Expr_app ((Ast.Expr_var "b"),
       (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "+")))))
    (Ast.Expr_app ((Ast.Expr_var "e"),
       (Ast.Expr_app (
          (Ast.Expr_app ((Ast.Expr_var "d"),
             (Ast.Expr_app (
                (Ast.Expr_app ((Ast.Expr_var "c"),
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "b"),
                         (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "+")))
                         )),
                      (Ast.Expr_var "+")))
                   )),
                (Ast.Expr_var "+")))
             )),
          (Ast.Expr_var "+")))
       ))
    (Ast.Expr_app (
       (Ast.Expr_app ((Ast.Expr_var "c"),
          (Ast.Expr_app ((Ast.Expr_var "b"), (Ast.Expr_var "*"))))),
       (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "+")))))
    (Ast.Expr_app ((Ast.Expr_var "c"),
       (Ast.Expr_app (
          (Ast.Expr_app ((Ast.Expr_var "b"),
             (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "/"))))),
          (Ast.Expr_var "*")))
       ))
    (Ast.Expr_app ((Ast.Expr_var "d"),
       (Ast.Expr_app (
          (Ast.Expr_app ((Ast.Expr_var "c"),
             (Ast.Expr_app (
                (Ast.Expr_app ((Ast.Expr_var "b"),
                   (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "*"))))),
                (Ast.Expr_var "*")))
             )),
          (Ast.Expr_var "-")))
       ))
    (Ast.Expr_app ((Ast.Expr_var "b"),
       (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "<=")))))
    (Ast.Expr_app (
       (Ast.Expr_app ((Ast.Expr_var "w"),
          (Ast.Expr_app ((Ast.Expr_var "z"), (Ast.Expr_var "+"))))),
       (Ast.Expr_app ((Ast.Expr_var "x"), (Ast.Expr_var "<=")))))
    (Ast.Expr_app ((Ast.Expr_var "u"),
       (Ast.Expr_app (
          (Ast.Expr_app (
             (Ast.Expr_app ((Ast.Expr_var "y"),
                (Ast.Expr_app ((Ast.Expr_var "x"), (Ast.Expr_var "+"))))),
             (Ast.Expr_app (
                (Ast.Expr_app ((Ast.Expr_var "c"),
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "b"),
                         (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "+")))
                         )),
                      (Ast.Expr_var "*")))
                   )),
                (Ast.Expr_var "-")))
             )),
          (Ast.Expr_var ">=")))
       ))
    (Ast.Expr_app (
       (Ast.Expr_app ((Ast.Expr_var "b"),
          (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "="))))),
       (Ast.Expr_app (
          (Ast.Expr_app (
             (Ast.Expr_app ((Ast.Expr_var "k"),
                (Ast.Expr_app (
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "w"),
                         (Ast.Expr_app ((Ast.Expr_var "y"), (Ast.Expr_var "-")))
                         )),
                      (Ast.Expr_app ((Ast.Expr_var "x"), (Ast.Expr_var "+"))))),
                   (Ast.Expr_var ">=")))
                )),
             (Ast.Expr_app (
                (Ast.Expr_app ((Ast.Expr_var "c"),
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "b"),
                         (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "+")))
                         )),
                      (Ast.Expr_var "=")))
                   )),
                (Ast.Expr_var "&&")))
             )),
          (Ast.Expr_var "||")))
       )) |}]

let%expect_test "simple expressions" =
   let cases = ["5"; "5, 6"; "x::xs"; "[5; 6; 7 + 8]"; "1::[2;3]"; "if true then a else b";] in
   List.iter (fun i -> show_res ~input:i ~parser:expr ~to_string:show_expr |> print_endline) cases;
  [%expect {|
    (Ast.Expr_const (Ast.Const_int 5))
    (Ast.Expr_tuple ((Ast.Expr_const (Ast.Const_int 5)),
       (Ast.Expr_const (Ast.Const_int 6)), []))
    (Ast.Expr_cons ((Ast.Expr_var "x"), (Ast.Expr_var "xs")))
    (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 5)),
       (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 6)),
          (Ast.Expr_cons (
             (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 8)),
                (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 7)),
                   (Ast.Expr_var "+")))
                )),
             Ast.Expr_nil))
          ))
       ))
    (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 1)),
       (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 2)),
          (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 3)), Ast.Expr_nil))))
       ))
    (Ast.Expr_ite ((Ast.Expr_const (Ast.Const_bool true)), (Ast.Expr_var "a"),
       (Ast.Expr_var "b"))) |}]

let%expect_test "let bindings" =
   let cases = ["let a = 5 in a"; "let a = 5 in let b = 6 in a + b"; "let f g h = g, h in f"] in
   List.iter (fun input -> show_res ~input ~parser:expr ~to_string:show_expr |> print_endline) cases;
  [%expect {|
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "a"), (Ast.Expr_const (Ast.Const_int 5))),
       (Ast.Expr_var "a")))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "a"), (Ast.Expr_const (Ast.Const_int 5))),
       (Ast.Expr_let (Ast.NonRecursive,
          ((Ast.Pat_var "b"), (Ast.Expr_const (Ast.Const_int 6))),
          (Ast.Expr_app ((Ast.Expr_var "b"),
             (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var "+")))))
          ))
       ))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "f"),
        (Ast.Expr_fun ((Ast.Pat_var "g"),
           (Ast.Expr_fun ((Ast.Pat_var "h"),
              (Ast.Expr_tuple ((Ast.Expr_var "g"), (Ast.Expr_var "h"), []))))
           ))),
       (Ast.Expr_var "f"))) |}]

let%expect_test "match" = 
   let cases = ["match x with | 1 -> 1 | 2 -> 2 | _ -> 42";
      "match l with | x::y::rest -> 1 | x::y -> 2 | _ -> 3";
      "match (a, b) with | (5, 6) -> 1 | (_, 6) -> 6 | (5, _) -> 5 | (_, _)  -> 1337"] in
   List.iter (fun input -> show_res ~input ~parser:expr ~to_string:show_expr |> print_endline) cases;
  [%expect {|
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
       )) |}]

let%expect_test "complex expr" =
   let cases = ["let f a b = if a > 0 then [a; a] else match b with | x::y::_ -> y | _ -> 42 in f 5 [6; 7]"] in
   List.iter (fun input -> show_res ~input ~parser:expr ~to_string: show_expr |> print_endline) cases;
  [%expect {|
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var "f"),
        (Ast.Expr_fun ((Ast.Pat_var "a"),
           (Ast.Expr_fun ((Ast.Pat_var "b"),
              (Ast.Expr_ite (
                 (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 0)),
                    (Ast.Expr_app ((Ast.Expr_var "a"), (Ast.Expr_var ">"))))),
                 (Ast.Expr_cons ((Ast.Expr_var "a"),
                    (Ast.Expr_cons ((Ast.Expr_var "a"), Ast.Expr_nil)))),
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
          (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 6)),
             (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 7)), Ast.Expr_nil)))),
          (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 5)), (Ast.Expr_var "f")))
          ))
       )) |}]

let%expect_test "fold" =
   let input = 
      "let fold l folder init =
         match l with
         | x::xs ->
            let acc = folder init x in
            fold xs folder acc
         | [] -> init"
   in
   show_res ~input ~parser:program ~to_string:show_structure |> print_endline;
  [%expect {|
    [(Ast.Str_value (Ast.NonRecursive,
        [((Ast.Pat_var "fold"),
          (Ast.Expr_fun ((Ast.Pat_var "l"),
             (Ast.Expr_fun ((Ast.Pat_var "folder"),
                (Ast.Expr_fun ((Ast.Pat_var "init"),
                   (Ast.Expr_match ((Ast.Expr_var "l"),
                      [((Ast.Pat_cons ((Ast.Pat_var "x"), (Ast.Pat_var "xs"))),
                        (Ast.Expr_let (Ast.NonRecursive,
                           ((Ast.Pat_var "acc"),
                            (Ast.Expr_app (
                               (Ast.Expr_app ((Ast.Expr_var "x"),
                                  (Ast.Expr_var "init"))),
                               (Ast.Expr_var "folder")))),
                           (Ast.Expr_app (
                              (Ast.Expr_app (
                                 (Ast.Expr_app ((Ast.Expr_var "acc"),
                                    (Ast.Expr_var "folder"))),
                                 (Ast.Expr_var "xs"))),
                              (Ast.Expr_var "fold")))
                           )));
                        (Ast.Pat_nil, (Ast.Expr_var "init"))]
                      ))
                   ))
                ))
             )))
          ]
        ))
      ] |}]

let%expect_test "factorial" =
   let input =
       "let f n = if n > 0 then
          n * f (n - 1)
          else 1" in
   show_res ~input ~parser:program ~to_string:show_structure |> print_endline;
  [%expect {|
    [(Ast.Str_value (Ast.NonRecursive,
        [((Ast.Pat_var "f"),
          (Ast.Expr_fun ((Ast.Pat_var "n"),
             (Ast.Expr_ite (
                (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 0)),
                   (Ast.Expr_app ((Ast.Expr_var "n"), (Ast.Expr_var ">"))))),
                (Ast.Expr_app (
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 1)),
                         (Ast.Expr_app ((Ast.Expr_var "n"), (Ast.Expr_var "-")))
                         )),
                      (Ast.Expr_var "f"))),
                   (Ast.Expr_app ((Ast.Expr_var "n"), (Ast.Expr_var "*"))))),
                (Ast.Expr_const (Ast.Const_int 1))))
             )))
          ]
        ))
      ] |}]

let%expect_test "even_odd" =
   let input =
      "let rec is_even n =
         if n = 0 then true
         else if n = 1 then false
         else is_odd (n - 1)
      and is_odd n =
         if n = 1 then true
         else if n = 0 then false
         else is_odd (n - 1)" in
   show_res ~input ~parser:program ~to_string:show_structure |> print_endline;
  [%expect {|
    [(Ast.Str_value (Ast.Recursive,
        [((Ast.Pat_var "is_even"),
          (Ast.Expr_fun ((Ast.Pat_var "n"),
             (Ast.Expr_ite (
                (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 0)),
                   (Ast.Expr_app ((Ast.Expr_var "n"), (Ast.Expr_var "="))))),
                (Ast.Expr_const (Ast.Const_bool true)),
                (Ast.Expr_ite (
                   (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 1)),
                      (Ast.Expr_app ((Ast.Expr_var "n"), (Ast.Expr_var "="))))),
                   (Ast.Expr_const (Ast.Const_bool false)),
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 1)),
                         (Ast.Expr_app ((Ast.Expr_var "n"), (Ast.Expr_var "-")))
                         )),
                      (Ast.Expr_var "is_odd")))
                   ))
                ))
             )));
          ((Ast.Pat_var "is_odd"),
           (Ast.Expr_fun ((Ast.Pat_var "n"),
              (Ast.Expr_ite (
                 (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 1)),
                    (Ast.Expr_app ((Ast.Expr_var "n"), (Ast.Expr_var "="))))),
                 (Ast.Expr_const (Ast.Const_bool true)),
                 (Ast.Expr_ite (
                    (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 0)),
                       (Ast.Expr_app ((Ast.Expr_var "n"), (Ast.Expr_var "="))))),
                    (Ast.Expr_const (Ast.Const_bool false)),
                    (Ast.Expr_app (
                       (Ast.Expr_app ((Ast.Expr_const (Ast.Const_int 1)),
                          (Ast.Expr_app ((Ast.Expr_var "n"), (Ast.Expr_var "-")))
                          )),
                       (Ast.Expr_var "is_odd")))
                    ))
                 ))
              )))
          ]
        ))
      ] |}]


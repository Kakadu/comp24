open Kreml_lib.Parser
open Kreml_lib.Ast

let%expect_test "patterns test" =
  let cases = ["123"; "true"; "false"; {|"word"|}
    ; "x"; "some_long_name"; "letrec"; "_"
    ; "(a)"
    ; "a, b"; "a ,  b,  c"; "a, (b,c), true"; "((a, a), b), c"
    ; "(a, b)"; "((a, b), c, (5, true))"
    ; "x::xs"; "(a, (b, c))::rest"; "x::y::rest"; "1::y::_::xs"
    ; "x::xs, y::ys"; "(x)::(xs)"; "a,b::xs"; "(a,b)::xs"]
   in
    List.iter (fun i -> print_endline (show_res ~input:i ~parser:pattern ~to_string:show_pattern)) cases;
  [%expect {|
    (Ast.Pat_const (Ast.Const_int 123))
    (Ast.Pat_const (Ast.Const_bool true))
    (Ast.Pat_const (Ast.Const_bool false))
    (Ast.Pat_const (Ast.Const_string "word"))
    (Ast.Pat_var (Ast.Id "x"))
    (Ast.Pat_var (Ast.Id "some_long_name"))
    (Ast.Pat_var (Ast.Id "letrec"))
    Ast.Pat_wildcard
    (Ast.Pat_var (Ast.Id "a"))
    (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "a")), (Ast.Pat_var (Ast.Id "b")), []))
    (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "a")), (Ast.Pat_var (Ast.Id "b")),
       [(Ast.Pat_var (Ast.Id "c"))]))
    (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "a")),
       (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "b")), (Ast.Pat_var (Ast.Id "c")),
          [])),
       [(Ast.Pat_const (Ast.Const_bool true))]))
    (Ast.Pat_tuple (
       (Ast.Pat_tuple (
          (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "a")), (Ast.Pat_var (Ast.Id "a")),
             [])),
          (Ast.Pat_var (Ast.Id "b")), [])),
       (Ast.Pat_var (Ast.Id "c")), []))
    (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "a")), (Ast.Pat_var (Ast.Id "b")), []))
    (Ast.Pat_tuple (
       (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "a")), (Ast.Pat_var (Ast.Id "b")),
          [])),
       (Ast.Pat_var (Ast.Id "c")),
       [(Ast.Pat_tuple ((Ast.Pat_const (Ast.Const_int 5)),
           (Ast.Pat_const (Ast.Const_bool true)), []))
         ]
       ))
    (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "x")), (Ast.Pat_var (Ast.Id "xs"))))
    (Ast.Pat_cons (
       (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "a")),
          (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "b")), (Ast.Pat_var (Ast.Id "c")),
             [])),
          [])),
       (Ast.Pat_var (Ast.Id "rest"))))
    (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "x")),
       (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "y")), (Ast.Pat_var (Ast.Id "rest"))))
       ))
    (Ast.Pat_cons ((Ast.Pat_const (Ast.Const_int 1)),
       (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "y")),
          (Ast.Pat_cons (Ast.Pat_wildcard, (Ast.Pat_var (Ast.Id "xs"))))))
       ))
    (Ast.Pat_tuple (
       (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "x")), (Ast.Pat_var (Ast.Id "xs")))),
       (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "y")), (Ast.Pat_var (Ast.Id "ys")))),
       []))
    (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "x")), (Ast.Pat_var (Ast.Id "xs"))))
    (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "a")),
       (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "b")), (Ast.Pat_var (Ast.Id "xs")))),
       []))
    (Ast.Pat_cons (
       (Ast.Pat_tuple ((Ast.Pat_var (Ast.Id "a")), (Ast.Pat_var (Ast.Id "b")),
          [])),
       (Ast.Pat_var (Ast.Id "xs")))) |}]


let%expect_test "operations" =
  let inputs = ["a"; "a+b"; "a+b+c+d+e";
    "a+b*c"; "a/b*c"; "a*b*c-d";
    "a<=b"; "x <= z + w";
    "(a+b)*c-(x + y) >= u";
    "a + b = c && x + (y-w) >= k || a = b"] in
  List.iter (fun i -> show_res ~input:i ~parser:(expr_with_ops ident_as_expr) ~to_string:show_expr |> print_endline ) inputs;
  [%expect {|
    (Ast.Expr_var (Ast.Id "a"))
    (Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
       ((Ast.Expr_var (Ast.Id "a")), [(Ast.Expr_var (Ast.Id "b"))])))
    (Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
       ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
           ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
               ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
                   ((Ast.Expr_var (Ast.Id "a")), [(Ast.Expr_var (Ast.Id "b"))]))),
                [(Ast.Expr_var (Ast.Id "c"))])
               )),
            [(Ast.Expr_var (Ast.Id "d"))])
           )),
        [(Ast.Expr_var (Ast.Id "e"))])
       ))
    (Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
       ((Ast.Expr_var (Ast.Id "a")),
        [(Ast.Expr_app ((Ast.Expr_var (Ast.Id "*")),
            ((Ast.Expr_var (Ast.Id "b")), [(Ast.Expr_var (Ast.Id "c"))])))
          ])
       ))
    (Ast.Expr_app ((Ast.Expr_var (Ast.Id "*")),
       ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "/")),
           ((Ast.Expr_var (Ast.Id "a")), [(Ast.Expr_var (Ast.Id "b"))]))),
        [(Ast.Expr_var (Ast.Id "c"))])
       ))
    (Ast.Expr_app ((Ast.Expr_var (Ast.Id "-")),
       ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "*")),
           ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "*")),
               ((Ast.Expr_var (Ast.Id "a")), [(Ast.Expr_var (Ast.Id "b"))]))),
            [(Ast.Expr_var (Ast.Id "c"))])
           )),
        [(Ast.Expr_var (Ast.Id "d"))])
       ))
    (Ast.Expr_app ((Ast.Expr_var (Ast.Id "<=")),
       ((Ast.Expr_var (Ast.Id "a")), [(Ast.Expr_var (Ast.Id "b"))])))
    (Ast.Expr_app ((Ast.Expr_var (Ast.Id "<=")),
       ((Ast.Expr_var (Ast.Id "x")),
        [(Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
            ((Ast.Expr_var (Ast.Id "z")), [(Ast.Expr_var (Ast.Id "w"))])))
          ])
       ))
    (Ast.Expr_app ((Ast.Expr_var (Ast.Id ">=")),
       ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "-")),
           ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "*")),
               ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
                   ((Ast.Expr_var (Ast.Id "a")), [(Ast.Expr_var (Ast.Id "b"))]))),
                [(Ast.Expr_var (Ast.Id "c"))])
               )),
            [(Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
                ((Ast.Expr_var (Ast.Id "x")), [(Ast.Expr_var (Ast.Id "y"))])))
              ])
           )),
        [(Ast.Expr_var (Ast.Id "u"))])
       ))
    (Ast.Expr_app ((Ast.Expr_var (Ast.Id "||")),
       ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "&&")),
           ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "=")),
               ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
                   ((Ast.Expr_var (Ast.Id "a")), [(Ast.Expr_var (Ast.Id "b"))]))),
                [(Ast.Expr_var (Ast.Id "c"))])
               )),
            [(Ast.Expr_app ((Ast.Expr_var (Ast.Id ">=")),
                ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
                    ((Ast.Expr_var (Ast.Id "x")),
                     [(Ast.Expr_app ((Ast.Expr_var (Ast.Id "-")),
                         ((Ast.Expr_var (Ast.Id "y")),
                          [(Ast.Expr_var (Ast.Id "w"))])
                         ))
                       ])
                    )),
                 [(Ast.Expr_var (Ast.Id "k"))])
                ))
              ])
           )),
        [(Ast.Expr_app ((Ast.Expr_var (Ast.Id "=")),
            ((Ast.Expr_var (Ast.Id "a")), [(Ast.Expr_var (Ast.Id "b"))])))
          ])
       )) |}]

let%expect_test "simple expressions" =
   let cases = ["5"; "5, 6"; "x::xs"; "[5; 6; 7 + 8]"; "1::[2;3]"; "if true then a else b";] in
   List.iter (fun i -> show_res ~input:i ~parser:expr ~to_string:show_expr |> print_endline) cases;
  [%expect {|
    (Ast.Expr_const (Ast.Const_int 5))
    (Ast.Expr_tuple ((Ast.Expr_const (Ast.Const_int 5)),
       (Ast.Expr_const (Ast.Const_int 6)), []))
    (Ast.Expr_cons
       (Some ((Ast.Expr_var (Ast.Id "x")), (Ast.Expr_var (Ast.Id "xs")))))
    (Ast.Expr_cons
       (Some ((Ast.Expr_const (Ast.Const_int 5)),
              (Ast.Expr_cons
                 (Some ((Ast.Expr_const (Ast.Const_int 6)),
                        (Ast.Expr_cons
                           (Some ((Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
                                     ((Ast.Expr_const (Ast.Const_int 7)),
                                      [(Ast.Expr_const (Ast.Const_int 8))])
                                     )),
                                  (Ast.Expr_cons None))))))))))
    (Ast.Expr_cons
       (Some ((Ast.Expr_const (Ast.Const_int 1)),
              (Ast.Expr_cons
                 (Some ((Ast.Expr_const (Ast.Const_int 2)),
                        (Ast.Expr_cons
                           (Some ((Ast.Expr_const (Ast.Const_int 3)),
                                  (Ast.Expr_cons None))))))))))   
    (Ast.Expr_ite ((Ast.Expr_const (Ast.Const_bool true)),
       (Ast.Expr_var (Ast.Id "a")), (Ast.Expr_var (Ast.Id "b")))) |}]

let%expect_test "let bindings" =
   let cases = ["let a = 5 in a"; "let a = 5 in let b = 6 in a + b"; "let f g h = g, h in f"] in
   List.iter (fun input -> show_res ~input ~parser:expr ~to_string:show_expr |> print_endline) cases;
  [%expect {|
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var (Ast.Id "a")), (Ast.Expr_const (Ast.Const_int 5))),
       (Ast.Expr_var (Ast.Id "a"))))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var (Ast.Id "a")), (Ast.Expr_const (Ast.Const_int 5))),
       (Ast.Expr_let (Ast.NonRecursive,
          ((Ast.Pat_var (Ast.Id "b")), (Ast.Expr_const (Ast.Const_int 6))),
          (Ast.Expr_app ((Ast.Expr_var (Ast.Id "+")),
             ((Ast.Expr_var (Ast.Id "a")), [(Ast.Expr_var (Ast.Id "b"))])))
          ))
       ))
    (Ast.Expr_let (Ast.NonRecursive,
       ((Ast.Pat_var (Ast.Id "f")),
        (Ast.Expr_fun ((Ast.Pat_var (Ast.Id "g")),
           (Ast.Expr_fun ((Ast.Pat_var (Ast.Id "h")),
              (Ast.Expr_tuple ((Ast.Expr_var (Ast.Id "g")),
                 (Ast.Expr_var (Ast.Id "h")), []))
              ))
           ))),
       (Ast.Expr_var (Ast.Id "f")))) |}]

let%expect_test "match" = 
   let cases = ["match x with | 1 -> 1 | 2 -> 2 | _ -> 42";
      "match l with | x::y::rest -> 1 | x::y -> 2 | _ -> 3";
      "match (a, b) with | (5, 6) -> 1 | (_, 6) -> 6 | (5, _) -> 5 | (_, _)  -> 1337"] in
   List.iter (fun input -> show_res ~input ~parser:expr ~to_string:show_expr |> print_endline) cases;
  [%expect {|
    (Ast.Expr_match ((Ast.Expr_var (Ast.Id "x")),
       [((Ast.Pat_const (Ast.Const_int 1)), (Ast.Expr_const (Ast.Const_int 1)));
         ((Ast.Pat_const (Ast.Const_int 2)), (Ast.Expr_const (Ast.Const_int 2)));
         (Ast.Pat_wildcard, (Ast.Expr_const (Ast.Const_int 42)))]
       ))
    (Ast.Expr_match ((Ast.Expr_var (Ast.Id "l")),
       [((Ast.Pat_cons ((Ast.Pat_var (Ast.Id "x")),
            (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "y")),
               (Ast.Pat_var (Ast.Id "rest"))))
            )),
         (Ast.Expr_const (Ast.Const_int 1)));
         ((Ast.Pat_cons ((Ast.Pat_var (Ast.Id "x")), (Ast.Pat_var (Ast.Id "y")))),
          (Ast.Expr_const (Ast.Const_int 2)));
         (Ast.Pat_wildcard, (Ast.Expr_const (Ast.Const_int 3)))]
       ))
    (Ast.Expr_match (
       (Ast.Expr_tuple ((Ast.Expr_var (Ast.Id "a")), (Ast.Expr_var (Ast.Id "b")),
          [])),
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
       ((Ast.Pat_var (Ast.Id "f")),
        (Ast.Expr_fun ((Ast.Pat_var (Ast.Id "a")),
           (Ast.Expr_fun ((Ast.Pat_var (Ast.Id "b")),
              (Ast.Expr_ite (
                 (Ast.Expr_app ((Ast.Expr_var (Ast.Id ">")),
                    ((Ast.Expr_var (Ast.Id "a")),
                     [(Ast.Expr_const (Ast.Const_int 0))])
                    )),
                 (Ast.Expr_cons
                    (Some ((Ast.Expr_var (Ast.Id "a")),
                           (Ast.Expr_cons
                              (Some ((Ast.Expr_var (Ast.Id "a")),
                                     (Ast.Expr_cons None))))))),
                 (Ast.Expr_match ((Ast.Expr_var (Ast.Id "b")),
                    [((Ast.Pat_cons ((Ast.Pat_var (Ast.Id "x")),
                         (Ast.Pat_cons ((Ast.Pat_var (Ast.Id "y")),
                            Ast.Pat_wildcard))
                         )),
                      (Ast.Expr_var (Ast.Id "y")));
                      (Ast.Pat_wildcard, (Ast.Expr_const (Ast.Const_int 42)))]
                    ))
                 ))
              ))
           ))),
       (Ast.Expr_app ((Ast.Expr_var (Ast.Id "f")),
          ((Ast.Expr_const (Ast.Const_int 5)),
           [(Ast.Expr_cons
               (Some ((Ast.Expr_const (Ast.Const_int 6)),
                      (Ast.Expr_cons
                         (Some ((Ast.Expr_const (Ast.Const_int 7)),
                                (Ast.Expr_cons None)))))))
             ])
          ))
       )) |}]

let%expect_test "factorial" =
   let input =
       "let f n = if n > 0 then
          n * f (n - 1)
          else 1" in
   show_res ~input ~parser:program ~to_string:show_structure |> print_endline


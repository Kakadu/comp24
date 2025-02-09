Most of the tests for parser are located in XaxaML/lib/expr_tests/parser_tests.ml
  $ ./run_parser.exe << EOF
  > let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  > let rec typed_fac (n : int): int  = if n <= 1 then 1 else n * fac (n - 1)
  [(Let_decl
      (Rec
         [((P_val "fac"), None,
           (E_fun ((P_val "n"), [],
              (E_ite (
                 (E_app ((E_app ((E_ident "<="), (E_ident "n"))),
                    (E_const (C_int 1)))),
                 (E_const (C_int 1)),
                 (E_app ((E_app ((E_ident "*"), (E_ident "n"))),
                    (E_app ((E_ident "fac"),
                       (E_app ((E_app ((E_ident "-"), (E_ident "n"))),
                          (E_const (C_int 1))))
                       ))
                    ))
                 ))
              )))
           ]));
    (Let_decl
       (Rec
          [((P_val "typed_fac"),
            (Some (RT_arr ((RT_prim "int"), (RT_prim "int")))),
            (E_fun ((P_typed ((P_val "n"), (RT_prim "int"))), [],
               (E_ite (
                  (E_app ((E_app ((E_ident "<="), (E_ident "n"))),
                     (E_const (C_int 1)))),
                  (E_const (C_int 1)),
                  (E_app ((E_app ((E_ident "*"), (E_ident "n"))),
                     (E_app ((E_ident "fac"),
                        (E_app ((E_app ((E_ident "-"), (E_ident "n"))),
                           (E_const (C_int 1))))
                        ))
                     ))
                  ))
               )))
            ]))
    ]
  $ ./run_parser.exe << EOF
  > let sum : int -> int -> int = fun a b -> a + b
  > let rec f a b = 1 and g c d = false
  [(Let_decl
      (Non_rec
         ((P_val "sum"),
          (Some (RT_arr ((RT_prim "int"),
                   (RT_arr ((RT_prim "int"), (RT_prim "int")))))),
          (E_fun ((P_val "a"), [(P_val "b")],
             (E_app ((E_app ((E_ident "+"), (E_ident "a"))), (E_ident "b"))))))));
    (Let_decl
       (Rec
          [((P_val "f"), None,
            (E_fun ((P_val "a"), [(P_val "b")], (E_const (C_int 1)))));
            ((P_val "g"), None,
             (E_fun ((P_val "c"), [(P_val "d")], (E_const (C_bool false)))))
            ]))
    ]
  $ ./run_parser.exe << EOF
  > let lst : int list = [1; 2; 3]
  > let f : bool * ('a -> 'a list) = 1
  [(Let_decl
      (Non_rec
         ((P_val "lst"), (Some (RT_list (RT_prim "int"))),
          (E_cons_list ((E_const (C_int 1)),
             (E_cons_list ((E_const (C_int 2)),
                (E_cons_list ((E_const (C_int 3)), (E_const C_empty_list)))))
             )))));
    (Let_decl
       (Non_rec
          ((P_val "f"),
           (Some (RT_tuple ((RT_prim "bool"),
                    [(RT_arr ((RT_var "a"), (RT_list (RT_var "a"))))]))),
           (E_const (C_int 1)))))
    ]
  $ ./run_parser.exe << EOF
  > let () = ()
  > let x = 1
  > let () = print_int x
  [(Let_decl (Non_rec ((P_const C_unit), None, (E_const C_unit))));
    (Let_decl (Non_rec ((P_val "x"), None, (E_const (C_int 1)))));
    (Let_decl
       (Non_rec
          ((P_const C_unit), None,
           (E_app ((E_ident "print_int"), (E_ident "x"))))))
    ]
  $ ./run_parser.exe << EOF
  > let ( + ) c d = ( - ) c d
  > let dec = ( + ) 1
  [(Let_decl
      (Non_rec
         ((P_val "+"), None,
          (E_fun ((P_val "c"), [(P_val "d")],
             (E_app ((E_app ((E_ident "-"), (E_ident "c"))), (E_ident "d"))))))));
    (Let_decl
       (Non_rec
          ((P_val "dec"), None, (E_app ((E_ident "+"), (E_const (C_int 1)))))))
    ]

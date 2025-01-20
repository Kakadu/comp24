  $ dune exec parser < manytests/do_not_type/001.ml
  let recfac = 
  (fun n -> (if ((<=)  n) 1 then 1  else  (*)  n (fac (((-)  n) 1))))
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "recfac"),
           (Ast.Expr_fun ((Ast.Pat_var "n"),
              (Ast.Expr_ite (
                 (Ast.Expr_app (
                    (Ast.Expr_app ((Ast.Expr_var "<="), (Ast.Expr_var "n"))),
                    (Ast.Expr_const (Ast.Const_int 1)))),
                 (Ast.Expr_const (Ast.Const_int 1)),
                 (Ast.Expr_app (
                    (Ast.Expr_app ((Ast.Expr_var "*"), (Ast.Expr_var "n"))),
                    (Ast.Expr_app ((Ast.Expr_var "fac"),
                       (Ast.Expr_app (
                          (Ast.Expr_app ((Ast.Expr_var "-"), (Ast.Expr_var "n")
                             )),
                          (Ast.Expr_const (Ast.Const_int 1))))
                       ))
                    ))
                 ))
              )))
           ]
         ))
       ]

  $ dune exec parser < manytests/do_not_type/002if.ml
  let main =  (if true then 1  else  false)
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "main"),
           (Ast.Expr_ite ((Ast.Expr_const (Ast.Const_bool true)),
              (Ast.Expr_const (Ast.Const_int 1)),
              (Ast.Expr_const (Ast.Const_bool false)))))
           ]
         ))
       ]

  $ dune exec parser < manytests/do_not_type/003occurs.ml
  let fix = 
  (fun f ->
    ((fun x -> f ((fun f -> (x  x) f)))) ((fun x -> f ((fun f -> (x  x) f)))) )
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "fix"),
           (Ast.Expr_fun ((Ast.Pat_var "f"),
              (Ast.Expr_app (
                 (Ast.Expr_fun ((Ast.Pat_var "x"),
                    (Ast.Expr_app ((Ast.Expr_var "f"),
                       (Ast.Expr_fun ((Ast.Pat_var "f"),
                          (Ast.Expr_app (
                             (Ast.Expr_app ((Ast.Expr_var "x"),
                                (Ast.Expr_var "x"))),
                             (Ast.Expr_var "f")))
                          ))
                       ))
                    )),
                 (Ast.Expr_fun ((Ast.Pat_var "x"),
                    (Ast.Expr_app ((Ast.Expr_var "f"),
                       (Ast.Expr_fun ((Ast.Pat_var "f"),
                          (Ast.Expr_app (
                             (Ast.Expr_app ((Ast.Expr_var "x"),
                                (Ast.Expr_var "x"))),
                             (Ast.Expr_var "f")))
                          ))
                       ))
                    ))
                 ))
              )))
           ]
         ))
       ]

  $ dune exec parser < manytests/do_not_type/004let_poly.ml
  let temp =  ((fun f -> (f  1, f  true))) ((fun x -> x)) 
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "temp"),
           (Ast.Expr_app (
              (Ast.Expr_fun ((Ast.Pat_var "f"),
                 (Ast.Expr_tuple (
                    (Ast.Expr_app ((Ast.Expr_var "f"),
                       (Ast.Expr_const (Ast.Const_int 1)))),
                    (Ast.Expr_app ((Ast.Expr_var "f"),
                       (Ast.Expr_const (Ast.Const_bool true)))),
                    []))
                 )),
              (Ast.Expr_fun ((Ast.Pat_var "x"), (Ast.Expr_var "x"))))))
           ]
         ))
       ]

  $ dune exec parser < manytests/do_not_type/015tuples.ml
  let rec (a, b)  =  (a, b)
   
   [(Ast.Str_value (Ast.Recursive,
         [((Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), [])),
           (Ast.Expr_tuple ((Ast.Expr_var "a"), (Ast.Expr_var "b"), [])))]
         ))
       ]

  $ dune exec parser < manytests/typed/001fac.ml
  let rec fac  = 
  (fun n -> (if ((<=)  n) 1 then 1  else  (*)  n (fac (((-)  n) 1))))
  let main =  (let () = print_int (fac  4) in  0)
   
   [(Ast.Str_value (Ast.Recursive,
         [((Ast.Pat_var "fac"),
           (Ast.Expr_fun ((Ast.Pat_var "n"),
              (Ast.Expr_ite (
                 (Ast.Expr_app (
                    (Ast.Expr_app ((Ast.Expr_var "<="), (Ast.Expr_var "n"))),
                    (Ast.Expr_const (Ast.Const_int 1)))),
                 (Ast.Expr_const (Ast.Const_int 1)),
                 (Ast.Expr_app (
                    (Ast.Expr_app ((Ast.Expr_var "*"), (Ast.Expr_var "n"))),
                    (Ast.Expr_app ((Ast.Expr_var "fac"),
                       (Ast.Expr_app (
                          (Ast.Expr_app ((Ast.Expr_var "-"), (Ast.Expr_var "n")
                             )),
                          (Ast.Expr_const (Ast.Const_int 1))))
                       ))
                    ))
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_app ((Ast.Expr_var "print_int"),
                   (Ast.Expr_app ((Ast.Expr_var "fac"),
                      (Ast.Expr_const (Ast.Const_int 4))))
                   ))),
               (Ast.Expr_const (Ast.Const_int 0)))))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/002fac.ml
  let rec fac_cps  = 
  (fun n ->
    (fun k ->
      (if ((=)  n) 1 then k  1  else  
        fac_cps (((-)  n) 1) ((fun p -> k (((*)  p) n))))))
  let main = 
  (let () = print_int (fac_cps  4 ((fun print_int -> print_int))) in  0)
   
   [(Ast.Str_value (Ast.Recursive,
         [((Ast.Pat_var "fac_cps"),
           (Ast.Expr_fun ((Ast.Pat_var "n"),
              (Ast.Expr_fun ((Ast.Pat_var "k"),
                 (Ast.Expr_ite (
                    (Ast.Expr_app (
                       (Ast.Expr_app ((Ast.Expr_var "="), (Ast.Expr_var "n"))),
                       (Ast.Expr_const (Ast.Const_int 1)))),
                    (Ast.Expr_app ((Ast.Expr_var "k"),
                       (Ast.Expr_const (Ast.Const_int 1)))),
                    (Ast.Expr_app (
                       (Ast.Expr_app ((Ast.Expr_var "fac_cps"),
                          (Ast.Expr_app (
                             (Ast.Expr_app ((Ast.Expr_var "-"),
                                (Ast.Expr_var "n"))),
                             (Ast.Expr_const (Ast.Const_int 1))))
                          )),
                       (Ast.Expr_fun ((Ast.Pat_var "p"),
                          (Ast.Expr_app ((Ast.Expr_var "k"),
                             (Ast.Expr_app (
                                (Ast.Expr_app ((Ast.Expr_var "*"),
                                   (Ast.Expr_var "p"))),
                                (Ast.Expr_var "n")))
                             ))
                          ))
                       ))
                    ))
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_app ((Ast.Expr_var "print_int"),
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "fac_cps"),
                         (Ast.Expr_const (Ast.Const_int 4)))),
                      (Ast.Expr_fun ((Ast.Pat_var "print_int"),
                         (Ast.Expr_var "print_int")))
                      ))
                   ))),
               (Ast.Expr_const (Ast.Const_int 0)))))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/003fib.ml
  let rec fib_acc  = 
  (fun a ->
    (fun b ->
      (fun n ->
        (if ((=)  n) 1 then b  else  
          (let n1 = ((-)  n) 1 in 
           (let ab = ((+)  a) b in  ((fib_acc  b) ab) n1))))))
  let rec fib  = 
  (fun n ->
    (if ((<)  n) 2 then n  else  (+) (fib (((-)  n) 1)) (fib (((-)  n) 2))))
  let main = 
  (let () = print_int (((fib_acc  0) 1) 4) in 
   (let () = print_int (fib  4) in  0))
   
   [(Ast.Str_value (Ast.Recursive,
         [((Ast.Pat_var "fib_acc"),
           (Ast.Expr_fun ((Ast.Pat_var "a"),
              (Ast.Expr_fun ((Ast.Pat_var "b"),
                 (Ast.Expr_fun ((Ast.Pat_var "n"),
                    (Ast.Expr_ite (
                       (Ast.Expr_app (
                          (Ast.Expr_app ((Ast.Expr_var "="), (Ast.Expr_var "n")
                             )),
                          (Ast.Expr_const (Ast.Const_int 1)))),
                       (Ast.Expr_var "b"),
                       (Ast.Expr_let (Ast.NonRecursive,
                          ((Ast.Pat_var "n1"),
                           (Ast.Expr_app (
                              (Ast.Expr_app ((Ast.Expr_var "-"),
                                 (Ast.Expr_var "n"))),
                              (Ast.Expr_const (Ast.Const_int 1))))),
                          (Ast.Expr_let (Ast.NonRecursive,
                             ((Ast.Pat_var "ab"),
                              (Ast.Expr_app (
                                 (Ast.Expr_app ((Ast.Expr_var "+"),
                                    (Ast.Expr_var "a"))),
                                 (Ast.Expr_var "b")))),
                             (Ast.Expr_app (
                                (Ast.Expr_app (
                                   (Ast.Expr_app ((Ast.Expr_var "fib_acc"),
                                      (Ast.Expr_var "b"))),
                                   (Ast.Expr_var "ab"))),
                                (Ast.Expr_var "n1")))
                             ))
                          ))
                       ))
                    ))
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.Recursive,
          [((Ast.Pat_var "fib"),
            (Ast.Expr_fun ((Ast.Pat_var "n"),
               (Ast.Expr_ite (
                  (Ast.Expr_app (
                     (Ast.Expr_app ((Ast.Expr_var "<"), (Ast.Expr_var "n"))),
                     (Ast.Expr_const (Ast.Const_int 2)))),
                  (Ast.Expr_var "n"),
                  (Ast.Expr_app (
                     (Ast.Expr_app ((Ast.Expr_var "+"),
                        (Ast.Expr_app ((Ast.Expr_var "fib"),
                           (Ast.Expr_app (
                              (Ast.Expr_app ((Ast.Expr_var "-"),
                                 (Ast.Expr_var "n"))),
                              (Ast.Expr_const (Ast.Const_int 1))))
                           ))
                        )),
                     (Ast.Expr_app ((Ast.Expr_var "fib"),
                        (Ast.Expr_app (
                           (Ast.Expr_app ((Ast.Expr_var "-"),
                              (Ast.Expr_var "n"))),
                           (Ast.Expr_const (Ast.Const_int 2))))
                        ))
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_app ((Ast.Expr_var "print_int"),
                   (Ast.Expr_app (
                      (Ast.Expr_app (
                         (Ast.Expr_app ((Ast.Expr_var "fib_acc"),
                            (Ast.Expr_const (Ast.Const_int 0)))),
                         (Ast.Expr_const (Ast.Const_int 1)))),
                      (Ast.Expr_const (Ast.Const_int 4))))
                   ))),
               (Ast.Expr_let (Ast.NonRecursive,
                  ((Ast.Pat_const Ast.Const_unit),
                   (Ast.Expr_app ((Ast.Expr_var "print_int"),
                      (Ast.Expr_app ((Ast.Expr_var "fib"),
                         (Ast.Expr_const (Ast.Const_int 4))))
                      ))),
                  (Ast.Expr_const (Ast.Const_int 0))))
               )))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/004manyargs.ml
  let wrap =  (fun f -> (if ((=)  1) 1 then f  else  f))
  let test3 = 
  (fun a ->
    (fun b ->
      (fun c ->
        (let a = print_int  a in 
         (let b = print_int  b in  (let c = print_int  c in  0))))))
  let test10 = 
  (fun a ->
    (fun b ->
      (fun c ->
        (fun d ->
          (fun e ->
            (fun f ->
              (fun g ->
                (fun h ->
                  (fun i ->
                    (fun j ->
                      ((+) (((+) (((+) (((+) (((+) (((+) (((+) (((+) (((+)  a) b)) c)) d)) e)) f)) g)) h)) i)) j))))))))))
  let main = 
  (let rez = ((((((((((wrap  test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000 in 
   (let () = print_int  rez in  (let temp2 = (((wrap  test3) 1) 10) 100 in  0)))
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "wrap"),
           (Ast.Expr_fun ((Ast.Pat_var "f"),
              (Ast.Expr_ite (
                 (Ast.Expr_app (
                    (Ast.Expr_app ((Ast.Expr_var "="),
                       (Ast.Expr_const (Ast.Const_int 1)))),
                    (Ast.Expr_const (Ast.Const_int 1)))),
                 (Ast.Expr_var "f"), (Ast.Expr_var "f")))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "test3"),
            (Ast.Expr_fun ((Ast.Pat_var "a"),
               (Ast.Expr_fun ((Ast.Pat_var "b"),
                  (Ast.Expr_fun ((Ast.Pat_var "c"),
                     (Ast.Expr_let (Ast.NonRecursive,
                        ((Ast.Pat_var "a"),
                         (Ast.Expr_app ((Ast.Expr_var "print_int"),
                            (Ast.Expr_var "a")))),
                        (Ast.Expr_let (Ast.NonRecursive,
                           ((Ast.Pat_var "b"),
                            (Ast.Expr_app ((Ast.Expr_var "print_int"),
                               (Ast.Expr_var "b")))),
                           (Ast.Expr_let (Ast.NonRecursive,
                              ((Ast.Pat_var "c"),
                               (Ast.Expr_app ((Ast.Expr_var "print_int"),
                                  (Ast.Expr_var "c")))),
                              (Ast.Expr_const (Ast.Const_int 0))))
                           ))
                        ))
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "test10"),
            (Ast.Expr_fun ((Ast.Pat_var "a"),
               (Ast.Expr_fun ((Ast.Pat_var "b"),
                  (Ast.Expr_fun ((Ast.Pat_var "c"),
                     (Ast.Expr_fun ((Ast.Pat_var "d"),
                        (Ast.Expr_fun ((Ast.Pat_var "e"),
                           (Ast.Expr_fun ((Ast.Pat_var "f"),
                              (Ast.Expr_fun ((Ast.Pat_var "g"),
                                 (Ast.Expr_fun ((Ast.Pat_var "h"),
                                    (Ast.Expr_fun ((Ast.Pat_var "i"),
                                       (Ast.Expr_fun ((Ast.Pat_var "j"),
                                          (Ast.Expr_app (
                                             (Ast.Expr_app ((Ast.Expr_var "+"),
                                                (Ast.Expr_app (
                                                   (Ast.Expr_app (
                                                      (Ast.Expr_var "+"),
                                                      (Ast.Expr_app (
                                                         (Ast.Expr_app (
                                                            (Ast.Expr_var "+"),
                                                            (Ast.Expr_app (
                                                               (Ast.Expr_app (
                                                                  (Ast.Expr_var
                                                                     "+"),
                                                                  (Ast.Expr_app (
                                                                     (Ast.Expr_app (
                                                                      (Ast.Expr_var
                                                                      "+"),
                                                                      (Ast.Expr_app (
                                                                      (Ast.Expr_app (
                                                                      (Ast.Expr_var
                                                                      "+"),
                                                                      (Ast.Expr_app (
                                                                      (Ast.Expr_app (
                                                                      (Ast.Expr_var
                                                                      "+"),
                                                                      (Ast.Expr_app (
                                                                      (Ast.Expr_app (
                                                                      (Ast.Expr_var
                                                                      "+"),
                                                                      (Ast.Expr_app (
                                                                      (Ast.Expr_app (
                                                                      (Ast.Expr_var
                                                                      "+"),
                                                                      (Ast.Expr_var
                                                                      "a"))),
                                                                      (Ast.Expr_var
                                                                      "b"))))),
                                                                      (Ast.Expr_var
                                                                      "c"))))),
                                                                      (Ast.Expr_var
                                                                      "d"))))),
                                                                      (Ast.Expr_var
                                                                      "e"))))),
                                                                     (Ast.Expr_var
                                                                      "f")
                                                                     ))
                                                                  )),
                                                               (Ast.Expr_var
                                                                  "g")
                                                               ))
                                                            )),
                                                         (Ast.Expr_var "h")))
                                                      )),
                                                   (Ast.Expr_var "i")))
                                                )),
                                             (Ast.Expr_var "j")))
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
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_var "rez"),
                (Ast.Expr_app (
                   (Ast.Expr_app (
                      (Ast.Expr_app (
                         (Ast.Expr_app (
                            (Ast.Expr_app (
                               (Ast.Expr_app (
                                  (Ast.Expr_app (
                                     (Ast.Expr_app (
                                        (Ast.Expr_app (
                                           (Ast.Expr_app (
                                              (Ast.Expr_app (
                                                 (Ast.Expr_var "wrap"),
                                                 (Ast.Expr_var "test10"))),
                                              (Ast.Expr_const (Ast.Const_int 1))
                                              )),
                                           (Ast.Expr_const (Ast.Const_int 10))
                                           )),
                                        (Ast.Expr_const (Ast.Const_int 100)))),
                                     (Ast.Expr_const (Ast.Const_int 1000)))),
                                  (Ast.Expr_const (Ast.Const_int 10000)))),
                               (Ast.Expr_const (Ast.Const_int 100000)))),
                            (Ast.Expr_const (Ast.Const_int 1000000)))),
                         (Ast.Expr_const (Ast.Const_int 10000000)))),
                      (Ast.Expr_const (Ast.Const_int 100000000)))),
                   (Ast.Expr_const (Ast.Const_int 1000000000))))),
               (Ast.Expr_let (Ast.NonRecursive,
                  ((Ast.Pat_const Ast.Const_unit),
                   (Ast.Expr_app ((Ast.Expr_var "print_int"),
                      (Ast.Expr_var "rez")))),
                  (Ast.Expr_let (Ast.NonRecursive,
                     ((Ast.Pat_var "temp2"),
                      (Ast.Expr_app (
                         (Ast.Expr_app (
                            (Ast.Expr_app (
                               (Ast.Expr_app ((Ast.Expr_var "wrap"),
                                  (Ast.Expr_var "test3"))),
                               (Ast.Expr_const (Ast.Const_int 1)))),
                            (Ast.Expr_const (Ast.Const_int 10)))),
                         (Ast.Expr_const (Ast.Const_int 100))))),
                     (Ast.Expr_const (Ast.Const_int 0))))
                  ))
               )))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/005fix.ml
  let rec fix  =  (fun f -> (fun x -> (f (fix  f)) x))
  let fac = 
  (fun self ->
    (fun n -> (if ((<=)  n) 1 then 1  else  (*)  n (self (((-)  n) 1)))))
  let main =  (let () = print_int ((fix  fac) 6) in  0)
   
   [(Ast.Str_value (Ast.Recursive,
         [((Ast.Pat_var "fix"),
           (Ast.Expr_fun ((Ast.Pat_var "f"),
              (Ast.Expr_fun ((Ast.Pat_var "x"),
                 (Ast.Expr_app (
                    (Ast.Expr_app ((Ast.Expr_var "f"),
                       (Ast.Expr_app ((Ast.Expr_var "fix"), (Ast.Expr_var "f")
                          ))
                       )),
                    (Ast.Expr_var "x")))
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "fac"),
            (Ast.Expr_fun ((Ast.Pat_var "self"),
               (Ast.Expr_fun ((Ast.Pat_var "n"),
                  (Ast.Expr_ite (
                     (Ast.Expr_app (
                        (Ast.Expr_app ((Ast.Expr_var "<="), (Ast.Expr_var "n")
                           )),
                        (Ast.Expr_const (Ast.Const_int 1)))),
                     (Ast.Expr_const (Ast.Const_int 1)),
                     (Ast.Expr_app (
                        (Ast.Expr_app ((Ast.Expr_var "*"), (Ast.Expr_var "n"))),
                        (Ast.Expr_app ((Ast.Expr_var "self"),
                           (Ast.Expr_app (
                              (Ast.Expr_app ((Ast.Expr_var "-"),
                                 (Ast.Expr_var "n"))),
                              (Ast.Expr_const (Ast.Const_int 1))))
                           ))
                        ))
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_app ((Ast.Expr_var "print_int"),
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "fix"), (Ast.Expr_var "fac")
                         )),
                      (Ast.Expr_const (Ast.Const_int 6))))
                   ))),
               (Ast.Expr_const (Ast.Const_int 0)))))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/006partial.ml
  let foo = 
  (fun b ->
    (if b then (fun foo -> ((+)  foo) 2)  else  (fun foo -> ((*)  foo) 10)))
  let foo =  (fun x -> foo  true (foo  false (foo  true ((foo  false) x))))
  let main =  (let () = print_int (foo  11) in  0)
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "foo"),
           (Ast.Expr_fun ((Ast.Pat_var "b"),
              (Ast.Expr_ite ((Ast.Expr_var "b"),
                 (Ast.Expr_fun ((Ast.Pat_var "foo"),
                    (Ast.Expr_app (
                       (Ast.Expr_app ((Ast.Expr_var "+"), (Ast.Expr_var "foo")
                          )),
                       (Ast.Expr_const (Ast.Const_int 2))))
                    )),
                 (Ast.Expr_fun ((Ast.Pat_var "foo"),
                    (Ast.Expr_app (
                       (Ast.Expr_app ((Ast.Expr_var "*"), (Ast.Expr_var "foo")
                          )),
                       (Ast.Expr_const (Ast.Const_int 10))))
                    ))
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "foo"),
            (Ast.Expr_fun ((Ast.Pat_var "x"),
               (Ast.Expr_app (
                  (Ast.Expr_app ((Ast.Expr_var "foo"),
                     (Ast.Expr_const (Ast.Const_bool true)))),
                  (Ast.Expr_app (
                     (Ast.Expr_app ((Ast.Expr_var "foo"),
                        (Ast.Expr_const (Ast.Const_bool false)))),
                     (Ast.Expr_app (
                        (Ast.Expr_app ((Ast.Expr_var "foo"),
                           (Ast.Expr_const (Ast.Const_bool true)))),
                        (Ast.Expr_app (
                           (Ast.Expr_app ((Ast.Expr_var "foo"),
                              (Ast.Expr_const (Ast.Const_bool false)))),
                           (Ast.Expr_var "x")))
                        ))
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_app ((Ast.Expr_var "print_int"),
                   (Ast.Expr_app ((Ast.Expr_var "foo"),
                      (Ast.Expr_const (Ast.Const_int 11))))
                   ))),
               (Ast.Expr_const (Ast.Const_int 0)))))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/006partial2.ml
  let foo = 
  (fun a ->
    (fun b ->
      (fun c ->
        (let () = print_int  a in 
         (let () = print_int  b in 
          (let () = print_int  c in  (+)  a (((*)  b) c)))))))
  let main = 
  (let foo = foo  1 in 
   (let foo = foo  2 in 
    (let foo = foo  3 in  (let () = print_int  foo in  0))))
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "foo"),
           (Ast.Expr_fun ((Ast.Pat_var "a"),
              (Ast.Expr_fun ((Ast.Pat_var "b"),
                 (Ast.Expr_fun ((Ast.Pat_var "c"),
                    (Ast.Expr_let (Ast.NonRecursive,
                       ((Ast.Pat_const Ast.Const_unit),
                        (Ast.Expr_app ((Ast.Expr_var "print_int"),
                           (Ast.Expr_var "a")))),
                       (Ast.Expr_let (Ast.NonRecursive,
                          ((Ast.Pat_const Ast.Const_unit),
                           (Ast.Expr_app ((Ast.Expr_var "print_int"),
                              (Ast.Expr_var "b")))),
                          (Ast.Expr_let (Ast.NonRecursive,
                             ((Ast.Pat_const Ast.Const_unit),
                              (Ast.Expr_app ((Ast.Expr_var "print_int"),
                                 (Ast.Expr_var "c")))),
                             (Ast.Expr_app (
                                (Ast.Expr_app ((Ast.Expr_var "+"),
                                   (Ast.Expr_var "a"))),
                                (Ast.Expr_app (
                                   (Ast.Expr_app ((Ast.Expr_var "*"),
                                      (Ast.Expr_var "b"))),
                                   (Ast.Expr_var "c")))
                                ))
                             ))
                          ))
                       ))
                    ))
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_var "foo"),
                (Ast.Expr_app ((Ast.Expr_var "foo"),
                   (Ast.Expr_const (Ast.Const_int 1))))),
               (Ast.Expr_let (Ast.NonRecursive,
                  ((Ast.Pat_var "foo"),
                   (Ast.Expr_app ((Ast.Expr_var "foo"),
                      (Ast.Expr_const (Ast.Const_int 2))))),
                  (Ast.Expr_let (Ast.NonRecursive,
                     ((Ast.Pat_var "foo"),
                      (Ast.Expr_app ((Ast.Expr_var "foo"),
                         (Ast.Expr_const (Ast.Const_int 3))))),
                     (Ast.Expr_let (Ast.NonRecursive,
                        ((Ast.Pat_const Ast.Const_unit),
                         (Ast.Expr_app ((Ast.Expr_var "print_int"),
                            (Ast.Expr_var "foo")))),
                        (Ast.Expr_const (Ast.Const_int 0))))
                     ))
                  ))
               )))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/006partial3.ml
  let foo = 
  (fun a ->
    (let () = print_int  a in 
     (fun b -> (let () = print_int  b in  (fun c -> print_int  c)))))
  let main =  (let () = ((foo  4) 8) 9 in  0)
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "foo"),
           (Ast.Expr_fun ((Ast.Pat_var "a"),
              (Ast.Expr_let (Ast.NonRecursive,
                 ((Ast.Pat_const Ast.Const_unit),
                  (Ast.Expr_app ((Ast.Expr_var "print_int"), (Ast.Expr_var "a")
                     ))),
                 (Ast.Expr_fun ((Ast.Pat_var "b"),
                    (Ast.Expr_let (Ast.NonRecursive,
                       ((Ast.Pat_const Ast.Const_unit),
                        (Ast.Expr_app ((Ast.Expr_var "print_int"),
                           (Ast.Expr_var "b")))),
                       (Ast.Expr_fun ((Ast.Pat_var "c"),
                          (Ast.Expr_app ((Ast.Expr_var "print_int"),
                             (Ast.Expr_var "c")))
                          ))
                       ))
                    ))
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_app (
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "foo"),
                         (Ast.Expr_const (Ast.Const_int 4)))),
                      (Ast.Expr_const (Ast.Const_int 8)))),
                   (Ast.Expr_const (Ast.Const_int 9))))),
               (Ast.Expr_const (Ast.Const_int 0)))))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/007order.ml
  let _start = 
  (fun (()) ->
    (fun (()) ->
      (fun a ->
        (fun (()) ->
          (fun b ->
            (fun _c ->
              (fun (()) ->
                (fun d ->
                  (fun __ ->
                    (let () = print_int (((+)  a) b) in 
                     (let () = print_int  __ in 
                      ((+) (((/) (((*)  a) b)) _c)) d))))) ))) )) ) 
  let main = 
  print_int ((((((_start (print_int  1) (print_int  2)) 3 (print_int  4)) 100) 1000 (
               print_int  -1)) 10000) -555555)
   
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
                                                (Ast.Expr_app (
                                                   (Ast.Expr_var "+"),
                                                   (Ast.Expr_var "a"))),
                                                (Ast.Expr_var "b")))
                                             ))),
                                         (Ast.Expr_let (Ast.NonRecursive,
                                            ((Ast.Pat_const Ast.Const_unit),
                                             (Ast.Expr_app (
                                                (Ast.Expr_var "print_int"),
                                                (Ast.Expr_var "__")))),
                                            (Ast.Expr_app (
                                               (Ast.Expr_app (
                                                  (Ast.Expr_var "+"),
                                                  (Ast.Expr_app (
                                                     (Ast.Expr_app (
                                                        (Ast.Expr_var "/"),
                                                        (Ast.Expr_app (
                                                           (Ast.Expr_app (
                                                              (Ast.Expr_var "*"),
                                                              (Ast.Expr_var "a")
                                                              )),
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
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_app ((Ast.Expr_var "print_int"),
               (Ast.Expr_app (
                  (Ast.Expr_app (
                     (Ast.Expr_app (
                        (Ast.Expr_app (
                           (Ast.Expr_app (
                              (Ast.Expr_app (
                                 (Ast.Expr_app (
                                    (Ast.Expr_app (
                                       (Ast.Expr_app ((Ast.Expr_var "_start"),
                                          (Ast.Expr_app (
                                             (Ast.Expr_var "print_int"),
                                             (Ast.Expr_const (Ast.Const_int 1))
                                             ))
                                          )),
                                       (Ast.Expr_app (
                                          (Ast.Expr_var "print_int"),
                                          (Ast.Expr_const (Ast.Const_int 2))))
                                       )),
                                    (Ast.Expr_const (Ast.Const_int 3)))),
                                 (Ast.Expr_app ((Ast.Expr_var "print_int"),
                                    (Ast.Expr_const (Ast.Const_int 4))))
                                 )),
                              (Ast.Expr_const (Ast.Const_int 100)))),
                           (Ast.Expr_const (Ast.Const_int 1000)))),
                        (Ast.Expr_app ((Ast.Expr_var "print_int"),
                           (Ast.Expr_const (Ast.Const_int -1))))
                        )),
                     (Ast.Expr_const (Ast.Const_int 10000)))),
                  (Ast.Expr_const (Ast.Const_int -555555))))
               )))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/008ascription.ml
  let addi =  (fun f -> (fun g -> (fun x -> ((f  x (((g  x) : bool))) : int))))
  let main = 
  (let () = print_int ((addi ((fun x ->
                                (fun b ->
                                  (if b then ((+)  x) 1  else  ((*)  x) 2)))) (
                        (fun _start -> ((=) (((/)  _start) 2)) 0))) 4) in 
   0)
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "addi"),
           (Ast.Expr_fun ((Ast.Pat_var "f"),
              (Ast.Expr_fun ((Ast.Pat_var "g"),
                 (Ast.Expr_fun ((Ast.Pat_var "x"),
                    (Ast.Expr_constrained (
                       (Ast.Expr_app (
                          (Ast.Expr_app ((Ast.Expr_var "f"), (Ast.Expr_var "x")
                             )),
                          (Ast.Expr_constrained (
                             (Ast.Expr_app ((Ast.Expr_var "g"),
                                (Ast.Expr_var "x"))),
                             Ast.Typ_bool))
                          )),
                       Ast.Typ_int))
                    ))
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_app ((Ast.Expr_var "print_int"),
                   (Ast.Expr_app (
                      (Ast.Expr_app (
                         (Ast.Expr_app ((Ast.Expr_var "addi"),
                            (Ast.Expr_fun ((Ast.Pat_var "x"),
                               (Ast.Expr_fun ((Ast.Pat_var "b"),
                                  (Ast.Expr_ite ((Ast.Expr_var "b"),
                                     (Ast.Expr_app (
                                        (Ast.Expr_app ((Ast.Expr_var "+"),
                                           (Ast.Expr_var "x"))),
                                        (Ast.Expr_const (Ast.Const_int 1)))),
                                     (Ast.Expr_app (
                                        (Ast.Expr_app ((Ast.Expr_var "*"),
                                           (Ast.Expr_var "x"))),
                                        (Ast.Expr_const (Ast.Const_int 2))))
                                     ))
                                  ))
                               ))
                            )),
                         (Ast.Expr_fun ((Ast.Pat_var "_start"),
                            (Ast.Expr_app (
                               (Ast.Expr_app ((Ast.Expr_var "="),
                                  (Ast.Expr_app (
                                     (Ast.Expr_app ((Ast.Expr_var "/"),
                                        (Ast.Expr_var "_start"))),
                                     (Ast.Expr_const (Ast.Const_int 2))))
                                  )),
                               (Ast.Expr_const (Ast.Const_int 0))))
                            ))
                         )),
                      (Ast.Expr_const (Ast.Const_int 4))))
                   ))),
               (Ast.Expr_const (Ast.Const_int 0)))))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/009let_poly.ml
  let temp =  (let f = (fun x -> x) in  (f  1, f  true))
   
   [(Ast.Str_value (Ast.NonRecursive,
         [((Ast.Pat_var "temp"),
           (Ast.Expr_let (Ast.NonRecursive,
              ((Ast.Pat_var "f"),
               (Ast.Expr_fun ((Ast.Pat_var "x"), (Ast.Expr_var "x")))),
              (Ast.Expr_tuple (
                 (Ast.Expr_app ((Ast.Expr_var "f"),
                    (Ast.Expr_const (Ast.Const_int 1)))),
                 (Ast.Expr_app ((Ast.Expr_var "f"),
                    (Ast.Expr_const (Ast.Const_bool true)))),
                 []))
              )))
           ]
         ))
       ]

  $ dune exec parser < manytests/typed/015tuples.ml
  let rec fix  =  (fun f -> (fun x -> (f (fix  f)) x))
  let map =  (fun f -> (fun p -> (let (a, b) = p in  (f  a, f  b))))
  let fixpoly = 
  (fun l ->
    (fix ((fun self ->
            (fun l -> (map ((fun li -> (fun x -> (li (self  l)) x)))) l)))) l)
  let feven = 
  (fun p ->
    (fun n ->
      (let (e, o) = p in  (if ((=)  n) 0 then 1  else  o (((-)  n) 1)))))
  let fodd = 
  (fun p ->
    (fun n ->
      (let (e, o) = p in  (if ((=)  n) 0 then 0  else  e (((-)  n) 1)))))
  let tie =  fixpoly ((feven, fodd))
  let rec meven  =  (fun n -> (if ((=)  n) 0 then 1  else  modd (((-)  n) 1)))
   and modd = (fun n -> (if ((=)  n) 0 then 1  else  meven (((-)  n) 1)))
  let main = 
  (let () = print_int (modd  1) in 
   (let () = print_int (meven  2) in 
    (let (even, odd) = tie in 
     (let () = print_int (odd  3) in  (let () = print_int (even  4) in  0)))))
   
   [(Ast.Str_value (Ast.Recursive,
         [((Ast.Pat_var "fix"),
           (Ast.Expr_fun ((Ast.Pat_var "f"),
              (Ast.Expr_fun ((Ast.Pat_var "x"),
                 (Ast.Expr_app (
                    (Ast.Expr_app ((Ast.Expr_var "f"),
                       (Ast.Expr_app ((Ast.Expr_var "fix"), (Ast.Expr_var "f")
                          ))
                       )),
                    (Ast.Expr_var "x")))
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "map"),
            (Ast.Expr_fun ((Ast.Pat_var "f"),
               (Ast.Expr_fun ((Ast.Pat_var "p"),
                  (Ast.Expr_let (Ast.NonRecursive,
                     ((Ast.Pat_tuple ((Ast.Pat_var "a"), (Ast.Pat_var "b"), 
                         [])),
                      (Ast.Expr_var "p")),
                     (Ast.Expr_tuple (
                        (Ast.Expr_app ((Ast.Expr_var "f"), (Ast.Expr_var "a"))),
                        (Ast.Expr_app ((Ast.Expr_var "f"), (Ast.Expr_var "b"))),
                        []))
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "fixpoly"),
            (Ast.Expr_fun ((Ast.Pat_var "l"),
               (Ast.Expr_app (
                  (Ast.Expr_app ((Ast.Expr_var "fix"),
                     (Ast.Expr_fun ((Ast.Pat_var "self"),
                        (Ast.Expr_fun ((Ast.Pat_var "l"),
                           (Ast.Expr_app (
                              (Ast.Expr_app ((Ast.Expr_var "map"),
                                 (Ast.Expr_fun ((Ast.Pat_var "li"),
                                    (Ast.Expr_fun ((Ast.Pat_var "x"),
                                       (Ast.Expr_app (
                                          (Ast.Expr_app ((Ast.Expr_var "li"),
                                             (Ast.Expr_app (
                                                (Ast.Expr_var "self"),
                                                (Ast.Expr_var "l")))
                                             )),
                                          (Ast.Expr_var "x")))
                                       ))
                                    ))
                                 )),
                              (Ast.Expr_var "l")))
                           ))
                        ))
                     )),
                  (Ast.Expr_var "l")))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "feven"),
            (Ast.Expr_fun ((Ast.Pat_var "p"),
               (Ast.Expr_fun ((Ast.Pat_var "n"),
                  (Ast.Expr_let (Ast.NonRecursive,
                     ((Ast.Pat_tuple ((Ast.Pat_var "e"), (Ast.Pat_var "o"), 
                         [])),
                      (Ast.Expr_var "p")),
                     (Ast.Expr_ite (
                        (Ast.Expr_app (
                           (Ast.Expr_app ((Ast.Expr_var "="),
                              (Ast.Expr_var "n"))),
                           (Ast.Expr_const (Ast.Const_int 0)))),
                        (Ast.Expr_const (Ast.Const_int 1)),
                        (Ast.Expr_app ((Ast.Expr_var "o"),
                           (Ast.Expr_app (
                              (Ast.Expr_app ((Ast.Expr_var "-"),
                                 (Ast.Expr_var "n"))),
                              (Ast.Expr_const (Ast.Const_int 1))))
                           ))
                        ))
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "fodd"),
            (Ast.Expr_fun ((Ast.Pat_var "p"),
               (Ast.Expr_fun ((Ast.Pat_var "n"),
                  (Ast.Expr_let (Ast.NonRecursive,
                     ((Ast.Pat_tuple ((Ast.Pat_var "e"), (Ast.Pat_var "o"), 
                         [])),
                      (Ast.Expr_var "p")),
                     (Ast.Expr_ite (
                        (Ast.Expr_app (
                           (Ast.Expr_app ((Ast.Expr_var "="),
                              (Ast.Expr_var "n"))),
                           (Ast.Expr_const (Ast.Const_int 0)))),
                        (Ast.Expr_const (Ast.Const_int 0)),
                        (Ast.Expr_app ((Ast.Expr_var "e"),
                           (Ast.Expr_app (
                              (Ast.Expr_app ((Ast.Expr_var "-"),
                                 (Ast.Expr_var "n"))),
                              (Ast.Expr_const (Ast.Const_int 1))))
                           ))
                        ))
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "tie"),
            (Ast.Expr_app ((Ast.Expr_var "fixpoly"),
               (Ast.Expr_tuple ((Ast.Expr_var "feven"), (Ast.Expr_var "fodd"),
                  []))
               )))
            ]
          ));
       (Ast.Str_value (Ast.Recursive,
          [((Ast.Pat_var "meven"),
            (Ast.Expr_fun ((Ast.Pat_var "n"),
               (Ast.Expr_ite (
                  (Ast.Expr_app (
                     (Ast.Expr_app ((Ast.Expr_var "="), (Ast.Expr_var "n"))),
                     (Ast.Expr_const (Ast.Const_int 0)))),
                  (Ast.Expr_const (Ast.Const_int 1)),
                  (Ast.Expr_app ((Ast.Expr_var "modd"),
                     (Ast.Expr_app (
                        (Ast.Expr_app ((Ast.Expr_var "-"), (Ast.Expr_var "n"))),
                        (Ast.Expr_const (Ast.Const_int 1))))
                     ))
                  ))
               )));
            ((Ast.Pat_var "modd"),
             (Ast.Expr_fun ((Ast.Pat_var "n"),
                (Ast.Expr_ite (
                   (Ast.Expr_app (
                      (Ast.Expr_app ((Ast.Expr_var "="), (Ast.Expr_var "n"))),
                      (Ast.Expr_const (Ast.Const_int 0)))),
                   (Ast.Expr_const (Ast.Const_int 1)),
                   (Ast.Expr_app ((Ast.Expr_var "meven"),
                      (Ast.Expr_app (
                         (Ast.Expr_app ((Ast.Expr_var "-"), (Ast.Expr_var "n")
                            )),
                         (Ast.Expr_const (Ast.Const_int 1))))
                      ))
                   ))
                )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_app ((Ast.Expr_var "print_int"),
                   (Ast.Expr_app ((Ast.Expr_var "modd"),
                      (Ast.Expr_const (Ast.Const_int 1))))
                   ))),
               (Ast.Expr_let (Ast.NonRecursive,
                  ((Ast.Pat_const Ast.Const_unit),
                   (Ast.Expr_app ((Ast.Expr_var "print_int"),
                      (Ast.Expr_app ((Ast.Expr_var "meven"),
                         (Ast.Expr_const (Ast.Const_int 2))))
                      ))),
                  (Ast.Expr_let (Ast.NonRecursive,
                     ((Ast.Pat_tuple ((Ast.Pat_var "even"),
                         (Ast.Pat_var "odd"), [])),
                      (Ast.Expr_var "tie")),
                     (Ast.Expr_let (Ast.NonRecursive,
                        ((Ast.Pat_const Ast.Const_unit),
                         (Ast.Expr_app ((Ast.Expr_var "print_int"),
                            (Ast.Expr_app ((Ast.Expr_var "odd"),
                               (Ast.Expr_const (Ast.Const_int 3))))
                            ))),
                        (Ast.Expr_let (Ast.NonRecursive,
                           ((Ast.Pat_const Ast.Const_unit),
                            (Ast.Expr_app ((Ast.Expr_var "print_int"),
                               (Ast.Expr_app ((Ast.Expr_var "even"),
                                  (Ast.Expr_const (Ast.Const_int 4))))
                               ))),
                           (Ast.Expr_const (Ast.Const_int 0))))
                        ))
                     ))
                  ))
               )))
            ]
          ))
       ]

  $ dune exec parser < manytests/typed/016lists.ml
  let rec length  = 
  (fun xs -> (match xs with | [] -> 0 | (h::tl)  -> (+)  1 (length  tl) ))
  let length_tail = 
  (let rec helper = (fun acc ->
                      (fun xs ->
                        (match xs with | [] -> acc | (h::tl)  -> (helper (
                                                                  ((+)  acc) 1)) tl
                                                    ))) in 
   helper  0)
  let rec map  = 
  (fun f ->
    (fun xs ->
      (match xs with | [] -> [] | (a::[])  -> (f  a)::([])  | (a::(b::[]) )  -> 
                                                            (f  a)::((f  b)::([]) ) 
                                                             | (a::(b::
                                                                   (c::[]) ) )  -> 
                                                             (f  a)::((f  b)::(
                                                                      (f  c)::([]) ) ) 
                                                              | (a::(b::
                                                                    (c::
                                                                    (d::tl) ) ) )  -> 
                                                              (f  a)::(
                                                                (f  b)::(
                                                                  (f  c)::(
                                                                    (f  d)::(
                                                                      (map  f) tl) ) ) ) 
                                                               )))
  let rec append  = 
  (fun xs ->
    (fun ys ->
      (match xs with | [] -> ys | (x::xs)  -> (x)::((append  xs) ys)  )))
  let concat = 
  (let rec helper = (fun xs ->
                      (match xs with | [] -> [] | (h::tl)  -> append  h (
                                                              helper  tl)
                                                 )) in 
   helper)
  let rec iter  = 
  (fun f ->
    (fun xs ->
      (match xs with | [] -> () | (h::tl)  -> (let () = f  h in  (iter  f) tl) )))
  let rec cartesian  = 
  (fun xs ->
    (fun ys ->
      (match xs with | [] -> [] | (h::tl)  -> append ((map ((fun a -> (h, a)))) ys) (
                                              (cartesian  tl) ys)
                                 )))
  let main = 
  (let () = iter  print_int ((1)::((2)::((3)::([]) ) ) ) in 
   (let () = print_int (length (cartesian ((1)::((2)::([]) ) ) ((1)::((2)::(
                                                                      (3)::(
                                                                      (4)::([]) ) ) ) ))) in 
    0))
   
   [(Ast.Str_value (Ast.Recursive,
         [((Ast.Pat_var "length"),
           (Ast.Expr_fun ((Ast.Pat_var "xs"),
              (Ast.Expr_match ((Ast.Expr_var "xs"),
                 [((Ast.Pat_const Ast.Const_nil),
                   (Ast.Expr_const (Ast.Const_int 0)));
                   ((Ast.Pat_cons ((Ast.Pat_var "h"), (Ast.Pat_var "tl"))),
                    (Ast.Expr_app (
                       (Ast.Expr_app ((Ast.Expr_var "+"),
                          (Ast.Expr_const (Ast.Const_int 1)))),
                       (Ast.Expr_app ((Ast.Expr_var "length"),
                          (Ast.Expr_var "tl")))
                       )))
                   ]
                 ))
              )))
           ]
         ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "length_tail"),
            (Ast.Expr_let (Ast.Recursive,
               ((Ast.Pat_var "helper"),
                (Ast.Expr_fun ((Ast.Pat_var "acc"),
                   (Ast.Expr_fun ((Ast.Pat_var "xs"),
                      (Ast.Expr_match ((Ast.Expr_var "xs"),
                         [((Ast.Pat_const Ast.Const_nil), (Ast.Expr_var "acc"));
                           ((Ast.Pat_cons ((Ast.Pat_var "h"),
                               (Ast.Pat_var "tl"))),
                            (Ast.Expr_app (
                               (Ast.Expr_app ((Ast.Expr_var "helper"),
                                  (Ast.Expr_app (
                                     (Ast.Expr_app ((Ast.Expr_var "+"),
                                        (Ast.Expr_var "acc"))),
                                     (Ast.Expr_const (Ast.Const_int 1))))
                                  )),
                               (Ast.Expr_var "tl"))))
                           ]
                         ))
                      ))
                   ))),
               (Ast.Expr_app ((Ast.Expr_var "helper"),
                  (Ast.Expr_const (Ast.Const_int 0))))
               )))
            ]
          ));
       (Ast.Str_value (Ast.Recursive,
          [((Ast.Pat_var "map"),
            (Ast.Expr_fun ((Ast.Pat_var "f"),
               (Ast.Expr_fun ((Ast.Pat_var "xs"),
                  (Ast.Expr_match ((Ast.Expr_var "xs"),
                     [((Ast.Pat_const Ast.Const_nil),
                       (Ast.Expr_const Ast.Const_nil));
                       ((Ast.Pat_cons ((Ast.Pat_var "a"),
                           (Ast.Pat_const Ast.Const_nil))),
                        (Ast.Expr_cons (
                           (Ast.Expr_app ((Ast.Expr_var "f"),
                              (Ast.Expr_var "a"))),
                           (Ast.Expr_const Ast.Const_nil))));
                       ((Ast.Pat_cons ((Ast.Pat_var "a"),
                           (Ast.Pat_cons ((Ast.Pat_var "b"),
                              (Ast.Pat_const Ast.Const_nil)))
                           )),
                        (Ast.Expr_cons (
                           (Ast.Expr_app ((Ast.Expr_var "f"),
                              (Ast.Expr_var "a"))),
                           (Ast.Expr_cons (
                              (Ast.Expr_app ((Ast.Expr_var "f"),
                                 (Ast.Expr_var "b"))),
                              (Ast.Expr_const Ast.Const_nil)))
                           )));
                       ((Ast.Pat_cons ((Ast.Pat_var "a"),
                           (Ast.Pat_cons ((Ast.Pat_var "b"),
                              (Ast.Pat_cons ((Ast.Pat_var "c"),
                                 (Ast.Pat_const Ast.Const_nil)))
                              ))
                           )),
                        (Ast.Expr_cons (
                           (Ast.Expr_app ((Ast.Expr_var "f"),
                              (Ast.Expr_var "a"))),
                           (Ast.Expr_cons (
                              (Ast.Expr_app ((Ast.Expr_var "f"),
                                 (Ast.Expr_var "b"))),
                              (Ast.Expr_cons (
                                 (Ast.Expr_app ((Ast.Expr_var "f"),
                                    (Ast.Expr_var "c"))),
                                 (Ast.Expr_const Ast.Const_nil)))
                              ))
                           )));
                       ((Ast.Pat_cons ((Ast.Pat_var "a"),
                           (Ast.Pat_cons ((Ast.Pat_var "b"),
                              (Ast.Pat_cons ((Ast.Pat_var "c"),
                                 (Ast.Pat_cons ((Ast.Pat_var "d"),
                                    (Ast.Pat_var "tl")))
                                 ))
                              ))
                           )),
                        (Ast.Expr_cons (
                           (Ast.Expr_app ((Ast.Expr_var "f"),
                              (Ast.Expr_var "a"))),
                           (Ast.Expr_cons (
                              (Ast.Expr_app ((Ast.Expr_var "f"),
                                 (Ast.Expr_var "b"))),
                              (Ast.Expr_cons (
                                 (Ast.Expr_app ((Ast.Expr_var "f"),
                                    (Ast.Expr_var "c"))),
                                 (Ast.Expr_cons (
                                    (Ast.Expr_app ((Ast.Expr_var "f"),
                                       (Ast.Expr_var "d"))),
                                    (Ast.Expr_app (
                                       (Ast.Expr_app ((Ast.Expr_var "map"),
                                          (Ast.Expr_var "f"))),
                                       (Ast.Expr_var "tl")))
                                    ))
                                 ))
                              ))
                           )))
                       ]
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.Recursive,
          [((Ast.Pat_var "append"),
            (Ast.Expr_fun ((Ast.Pat_var "xs"),
               (Ast.Expr_fun ((Ast.Pat_var "ys"),
                  (Ast.Expr_match ((Ast.Expr_var "xs"),
                     [((Ast.Pat_const Ast.Const_nil), (Ast.Expr_var "ys"));
                       ((Ast.Pat_cons ((Ast.Pat_var "x"), (Ast.Pat_var "xs"))),
                        (Ast.Expr_cons ((Ast.Expr_var "x"),
                           (Ast.Expr_app (
                              (Ast.Expr_app ((Ast.Expr_var "append"),
                                 (Ast.Expr_var "xs"))),
                              (Ast.Expr_var "ys")))
                           )))
                       ]
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "concat"),
            (Ast.Expr_let (Ast.Recursive,
               ((Ast.Pat_var "helper"),
                (Ast.Expr_fun ((Ast.Pat_var "xs"),
                   (Ast.Expr_match ((Ast.Expr_var "xs"),
                      [((Ast.Pat_const Ast.Const_nil),
                        (Ast.Expr_const Ast.Const_nil));
                        ((Ast.Pat_cons ((Ast.Pat_var "h"), (Ast.Pat_var "tl"))),
                         (Ast.Expr_app (
                            (Ast.Expr_app ((Ast.Expr_var "append"),
                               (Ast.Expr_var "h"))),
                            (Ast.Expr_app ((Ast.Expr_var "helper"),
                               (Ast.Expr_var "tl")))
                            )))
                        ]
                      ))
                   ))),
               (Ast.Expr_var "helper"))))
            ]
          ));
       (Ast.Str_value (Ast.Recursive,
          [((Ast.Pat_var "iter"),
            (Ast.Expr_fun ((Ast.Pat_var "f"),
               (Ast.Expr_fun ((Ast.Pat_var "xs"),
                  (Ast.Expr_match ((Ast.Expr_var "xs"),
                     [((Ast.Pat_const Ast.Const_nil),
                       (Ast.Expr_const Ast.Const_unit));
                       ((Ast.Pat_cons ((Ast.Pat_var "h"), (Ast.Pat_var "tl"))),
                        (Ast.Expr_let (Ast.NonRecursive,
                           ((Ast.Pat_const Ast.Const_unit),
                            (Ast.Expr_app ((Ast.Expr_var "f"),
                               (Ast.Expr_var "h")))),
                           (Ast.Expr_app (
                              (Ast.Expr_app ((Ast.Expr_var "iter"),
                                 (Ast.Expr_var "f"))),
                              (Ast.Expr_var "tl")))
                           )))
                       ]
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.Recursive,
          [((Ast.Pat_var "cartesian"),
            (Ast.Expr_fun ((Ast.Pat_var "xs"),
               (Ast.Expr_fun ((Ast.Pat_var "ys"),
                  (Ast.Expr_match ((Ast.Expr_var "xs"),
                     [((Ast.Pat_const Ast.Const_nil),
                       (Ast.Expr_const Ast.Const_nil));
                       ((Ast.Pat_cons ((Ast.Pat_var "h"), (Ast.Pat_var "tl"))),
                        (Ast.Expr_app (
                           (Ast.Expr_app ((Ast.Expr_var "append"),
                              (Ast.Expr_app (
                                 (Ast.Expr_app ((Ast.Expr_var "map"),
                                    (Ast.Expr_fun ((Ast.Pat_var "a"),
                                       (Ast.Expr_tuple ((Ast.Expr_var "h"),
                                          (Ast.Expr_var "a"), []))
                                       ))
                                    )),
                                 (Ast.Expr_var "ys")))
                              )),
                           (Ast.Expr_app (
                              (Ast.Expr_app ((Ast.Expr_var "cartesian"),
                                 (Ast.Expr_var "tl"))),
                              (Ast.Expr_var "ys")))
                           )))
                       ]
                     ))
                  ))
               )))
            ]
          ));
       (Ast.Str_value (Ast.NonRecursive,
          [((Ast.Pat_var "main"),
            (Ast.Expr_let (Ast.NonRecursive,
               ((Ast.Pat_const Ast.Const_unit),
                (Ast.Expr_app (
                   (Ast.Expr_app ((Ast.Expr_var "iter"),
                      (Ast.Expr_var "print_int"))),
                   (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 1)),
                      (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 2)),
                         (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 3)),
                            (Ast.Expr_const Ast.Const_nil)))
                         ))
                      ))
                   ))),
               (Ast.Expr_let (Ast.NonRecursive,
                  ((Ast.Pat_const Ast.Const_unit),
                   (Ast.Expr_app ((Ast.Expr_var "print_int"),
                      (Ast.Expr_app ((Ast.Expr_var "length"),
                         (Ast.Expr_app (
                            (Ast.Expr_app ((Ast.Expr_var "cartesian"),
                               (Ast.Expr_cons (
                                  (Ast.Expr_const (Ast.Const_int 1)),
                                  (Ast.Expr_cons (
                                     (Ast.Expr_const (Ast.Const_int 2)),
                                     (Ast.Expr_const Ast.Const_nil)))
                                  ))
                               )),
                            (Ast.Expr_cons ((Ast.Expr_const (Ast.Const_int 1)),
                               (Ast.Expr_cons (
                                  (Ast.Expr_const (Ast.Const_int 2)),
                                  (Ast.Expr_cons (
                                     (Ast.Expr_const (Ast.Const_int 3)),
                                     (Ast.Expr_cons (
                                        (Ast.Expr_const (Ast.Const_int 4)),
                                        (Ast.Expr_const Ast.Const_nil)))
                                     ))
                                  ))
                               ))
                            ))
                         ))
                      ))),
                  (Ast.Expr_const (Ast.Const_int 0))))
               )))
            ]
          ))
       ]











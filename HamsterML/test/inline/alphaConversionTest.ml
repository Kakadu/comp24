open HamsterML.Ast
open HamsterML.AC
open ParserTest

let alpha_conv_pattern (s : string) =
  let _, res =
    HamsterML.Utils.R.run (convert_pattern NameEnv.empty "arg_" (parse_pattern s))
  in
  res
;;

let alpha_conv_expr (s : string) =
  let _, res = HamsterML.Utils.R.run (convert_expr NameEnv.empty (parse_expr s)) in
  res
;;

let alpha_conv_prog (s : string) =
  let res = HamsterML.Utils.R.run (convert_prog (parse_prog s)) in
  res
;;

let pp_alpha_conv_prog (s : string) =
  let open HamsterML.PrinterAst in
  s |> alpha_conv_prog |> pretty_print_prog |> print_string
;;

let%test _ =
  alpha_conv_pattern "a :: b :: a"
  = ListConcat (Var "arg_0", ListConcat (Var "arg_1", Var "arg_0"))
;;

let%test _ =
  alpha_conv_pattern "a, b, c, a"
  = Tuple (Var "arg_0", Var "arg_1", [ Var "arg_2"; Var "arg_0" ])
;;

let%test _ =
  alpha_conv_pattern "[a; b; a; c]"
  = List [ Var "arg_0"; Var "arg_1"; Var "arg_0"; Var "arg_2" ]
;;

let%test _ =
  alpha_conv_expr "fun a -> fun b -> fun a -> a + b"
  = Fun
      ( [ Var "arg_0" ]
      , Fun
          ( [ Var "arg_1" ]
          , Fun
              ( [ Var "arg_2" ]
              , Application
                  (Application (EOperation (Binary ADD), EVar "arg_2"), EVar "arg_1") ) )
      )
;;

let%test _ =
  alpha_conv_expr "let a = let a a a = a + 1 in a 1 2"
  = Let
      ( Nonrecursive
      , [ ( Var "var_3"
          , []
          , Let
              ( Nonrecursive
              , [ ( Var "var_2"
                  , [ Var "arg_0"; Var "arg_1" ]
                  , Application
                      (Application (EOperation (Binary ADD), EVar "arg_1"), EConst (Int 1))
                  )
                ]
              , Some
                  (Application (Application (EVar "var_2", EConst (Int 1)), EConst (Int 2)))
              ) )
        ]
      , None )
;;

let%test _ =
  alpha_conv_expr "let rec f a b = let g a b = f a b in g a b"
  = Let
      ( Recursive
      , [ ( Var "var_0"
          , [ Var "arg_1"; Var "arg_2" ]
          , Let
              ( Nonrecursive
              , [ ( Var "var_5"
                  , [ Var "arg_3"; Var "arg_4" ]
                  , Application (Application (EVar "var_0", EVar "arg_3"), EVar "arg_4") )
                ]
              , Some
                  (Application (Application (EVar "var_5", EVar "arg_1"), EVar "arg_2"))
              ) )
        ]
      , None )
;;

let%test _ =
  alpha_conv_expr "let (+) a b = a - b"
  = Let
      ( Nonrecursive
      , [ ( Var "var_2"
          , [ Var "arg_0"; Var "arg_1" ]
          , Application (Application (EOperation (Binary SUB), EVar "arg_0"), EVar "arg_1")
          )
        ]
      , None )
;;

let%test _ =
  alpha_conv_expr "let z x y = let (+) a b = a - b in x + y"
  = Let
      ( Nonrecursive
      , [ ( Var "var_5"
          , [ Var "arg_0"; Var "arg_1" ]
          , Let
              ( Nonrecursive
              , [ ( Var "var_4"
                  , [ Var "arg_2"; Var "arg_3" ]
                  , Application
                      (Application (EOperation (Binary SUB), EVar "arg_2"), EVar "arg_3")
                  )
                ]
              , Some
                  (Application (Application (EVar "var_4", EVar "arg_0"), EVar "arg_1"))
              ) )
        ]
      , None )
;;

let%test _ =
  alpha_conv_prog {| let f a b = a + b let f a b = f 1 2 |}
  = [ Let
        ( Nonrecursive
        , [ ( Var "var_2"
            , [ Var "arg_0"; Var "arg_1" ]
            , Application
                (Application (EOperation (Binary ADD), EVar "arg_0"), EVar "arg_1") )
          ]
        , None )
    ; Let
        ( Nonrecursive
        , [ ( Var "var_5"
            , [ Var "arg_3"; Var "arg_4" ]
            , Application (Application (EVar "var_2", EConst (Int 1)), EConst (Int 2)) )
          ]
        , None )
    ]
;;

let%test _ =
  alpha_conv_prog {| let f a b = a + b let rec f a b = f 1 2 |}
  = [ Let
        ( Nonrecursive
        , [ ( Var "var_2"
            , [ Var "arg_0"; Var "arg_1" ]
            , Application
                (Application (EOperation (Binary ADD), EVar "arg_0"), EVar "arg_1") )
          ]
        , None )
    ; Let
        ( Recursive
        , [ ( Var "var_3"
            , [ Var "arg_4"; Var "arg_5" ]
            , Application (Application (EVar "var_3", EConst (Int 1)), EConst (Int 2)) )
          ]
        , None )
    ]
;;

let%test _ =
  alpha_conv_prog {| let test = 5 let test_fun x = x + test let test = 6 |}
  = [ Let (Nonrecursive, [ Var "var_0", [], EConst (Int 5) ], None)
    ; Let
        ( Nonrecursive
        , [ ( Var "var_2"
            , [ Var "arg_1" ]
            , Application
                (Application (EOperation (Binary ADD), EVar "arg_1"), EVar "var_0") )
          ]
        , None )
    ; Let (Nonrecursive, [ Var "var_3", [], EConst (Int 6) ], None)
    ]
;;

let%test _ =
  alpha_conv_prog "let rec f x = g x and g = f 1"
  = [ Let
        ( Recursive
        , [ Var "var_0", [ Var "arg_2" ], Application (EVar "var_1", EVar "arg_2")
          ; Var "var_1", [], Application (EVar "var_0", EConst (Int 1))
          ]
        , None )
    ]
;;

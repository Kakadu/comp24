open HamsterML.LL
open ParserTest

let lambda_lift_prog (s : string) = R.run (ll_prog (parse_prog s))

let%test _ =
  lambda_lift_prog "let a = 1 + 1"
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "LL_fun_0"
            , []
            , LLApplication
                ( LLApplication (LLOperation (Binary ADD), LLConst (Int 1))
                , LLConst (Int 1) ) )
          ]
        , None )
    ]
;;

let%test _ =
  lambda_lift_prog "let a = 10 let b = a"
  = [ LLLet (Nonrecursive, [ Var "LL_fun_0", [], LLConst (Int 10) ], None)
    ; LLLet (Nonrecursive, [ Var "LL_fun_1", [], LLVar "LL_fun_0" ], None)
    ]
;;

let%test _ =
  lambda_lift_prog "let a = 10 and b = 20"
  = [ LLLet
        ( Nonrecursive
        , [ Var "LL_fun_0", [], LLConst (Int 10); Var "LL_fun_1", [], LLConst (Int 20) ]
        , None )
    ]
;;

let%test _ =
  lambda_lift_prog "let rec a = 1 + 1 in a + 2"
  = [ LLLet
        ( Recursive
        , [ ( Var "LL_fun_0"
            , []
            , LLApplication
                ( LLApplication (LLOperation (Binary ADD), LLConst (Int 1))
                , LLConst (Int 1) ) )
          ]
        , Some
            (LLApplication
               (LLApplication (LLOperation (Binary ADD), LLVar "LL_fun_0"), LLConst (Int 2))) )
    ]
;;

(*
   let sum x y = let f a b = a + b in f x y
   ---
   let ll_fun_1 a b = a + b
   let ll_fun_0 x y = ll_fun_1 x y
*)

let%test _ =
  lambda_lift_prog "let sum x y = let f a b = a + b in f x y"
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "LL_fun_1"
            , [ Var "a"; Var "b" ]
            , LLApplication
                (LLApplication (LLOperation (Binary ADD), LLVar "a"), LLVar "b") )
          ]
        , None )
    ; LLLet
        ( Nonrecursive
        , [ ( Var "LL_fun_0"
            , [ Var "x"; Var "y" ]
            , LLApplication (LLApplication (LLVar "LL_fun_1", LLVar "x"), LLVar "y") )
          ]
        , None )
    ]
;;

(* let rec fac n = if n<=1 then 1 else n * fac (n-1)
   ---
   let rec ll_fun_0 n = if n<=1 then 1 else n * ll_fun_0 (n-1)
*)

let%test _ =
  lambda_lift_prog "let rec fac n = if n<=1 then 1 else n * fac (n-1)"
  = [ LLLet
        ( Recursive
        , [ ( Var "LL_fun_0"
            , [ Var "n" ]
            , LLIf
                ( LLApplication
                    (LLApplication (LLOperation (Binary LTE), LLVar "n"), LLConst (Int 1))
                , LLConst (Int 1)
                , Some
                    (LLApplication
                       ( LLApplication (LLOperation (Binary MUL), LLVar "n")
                       , LLApplication
                           ( LLVar "LL_fun_0"
                           , LLApplication
                               ( LLApplication (LLOperation (Binary SUB), LLVar "n")
                               , LLConst (Int 1) ) ) )) ) )
          ]
        , None )
    ]
;;

(*
   let sum x y = x + y let main x y = sum x y
   ---
   let ll_fun_0 x y = x + y
   let ll_fun_1 x y = ll_fun_0 x y
*)

let%test _ =
  lambda_lift_prog "let sum x y = x + y let main x y = sum x y"
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "LL_fun_0"
            , [ Var "x"; Var "y" ]
            , LLApplication
                (LLApplication (LLOperation (Binary ADD), LLVar "x"), LLVar "y") )
          ]
        , None )
    ; LLLet
        ( Nonrecursive
        , [ ( Var "LL_fun_1"
            , [ Var "x"; Var "y" ]
            , LLApplication (LLApplication (LLVar "LL_fun_0", LLVar "x"), LLVar "y") )
          ]
        , None )
    ]
;;

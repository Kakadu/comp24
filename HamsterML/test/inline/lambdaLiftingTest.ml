open HamsterML.LL
open ParserTest

let lambda_lift_prog (s : string) =
  let open HamsterML.Utils.R in
  let prog = parse_prog s in
  let alpha_convert prog = run @@ HamsterML.AC.convert_prog prog in
  let lambda_lift prog = run @@ ll_prog prog in
  prog |> alpha_convert |> lambda_lift
;;

let%test _ =
  lambda_lift_prog "let a = 1 + 1"
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "ll_var_0"
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
  = [ LLLet (Nonrecursive, [ Var "ll_var_0", [], LLConst (Int 10) ], None)
    ; LLLet (Nonrecursive, [ Var "ll_var_1", [], LLVar "ll_var_0" ], None)
    ]
;;

let%test _ =
  lambda_lift_prog "let a = 10 and b = 20"
  = [ LLLet
        ( Nonrecursive
        , [ Var "ll_var_0", [], LLConst (Int 10); Var "ll_var_1", [], LLConst (Int 20) ]
        , None )
    ]
;;

let%test _ =
  lambda_lift_prog "let rec a = 1 + 1 in a + 2"
  = [ LLLet
        ( Recursive
        , [ ( Var "ll_var_0"
            , []
            , LLApplication
                ( LLApplication (LLOperation (Binary ADD), LLConst (Int 1))
                , LLConst (Int 1) ) )
          ]
        , Some
            (LLApplication
               ( LLApplication (LLOperation (Binary ADD), LLVar "ll_var_0")
               , LLConst (Int 2) )) )
    ]
;;

(*
   let sum x y = let f a b = a + b in f x y
   ---
   let ll_var_1 a b = a + b
   let ll_var_0 x y = ll_var_1 x y
*)

let%test _ =
  lambda_lift_prog "let sum x y = let f a b = a + b in f x y"
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "ll_var_1"
            , [ Var "arg_2"; Var "arg_3" ]
            , LLApplication
                (LLApplication (LLOperation (Binary ADD), LLVar "arg_2"), LLVar "arg_3") )
          ]
        , None )
    ; LLLet
        ( Nonrecursive
        , [ ( Var "ll_var_0"
            , [ Var "arg_0"; Var "arg_1" ]
            , LLApplication
                (LLApplication (LLVar "ll_var_1", LLVar "arg_0"), LLVar "arg_1") )
          ]
        , None )
    ]
;;

(* let rec fac n = if n<=1 then 1 else n * fac (n-1)
   ---
   let rec ll_var_0 n = if n<=1 then 1 else n * ll_var_0 (n-1)
*)

let%test _ =
  lambda_lift_prog "let rec fac n = if n<=1 then 1 else n * fac (n-1)"
  = [ LLLet
        ( Recursive
        , [ ( Var "ll_var_0"
            , [ Var "n" ]
            , LLIf
                ( LLApplication
                    (LLApplication (LLOperation (Binary LTE), LLVar "n"), LLConst (Int 1))
                , LLConst (Int 1)
                , Some
                    (LLApplication
                       ( LLApplication (LLOperation (Binary MUL), LLVar "n")
                       , LLApplication
                           ( LLVar "ll_var_0"
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
   let ll_var_0 x y = x + y
   let ll_var_1 x y = ll_var_0 x y
*)

let%test _ =
  lambda_lift_prog "let sum x y = x + y let main x y = sum x y"
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "ll_var_0"
            , [ Var "x"; Var "y" ]
            , LLApplication
                (LLApplication (LLOperation (Binary ADD), LLVar "x"), LLVar "y") )
          ]
        , None )
    ; LLLet
        ( Nonrecursive
        , [ ( Var "ll_var_1"
            , [ Var "x"; Var "y" ]
            , LLApplication (LLApplication (LLVar "ll_var_0", LLVar "x"), LLVar "y") )
          ]
        , None )
    ]
;;

(*
   let rec f x = g x and g x = x + 1
   ---
   let rec ll_var_0 x = ll_var_1 x and ll_var_1 x = x + 1
*)

let%test _ =
  lambda_lift_prog "let rec f x = g x and g x = x + 1"
  = [ LLLet
        ( Recursive
        , [ Var "ll_var_0", [ Var "x" ], LLApplication (LLVar "ll_var_1", LLVar "x")
          ; ( Var "ll_var_1"
            , [ Var "x" ]
            , LLApplication
                (LLApplication (LLOperation (Binary ADD), LLVar "x"), LLConst (Int 1)) )
          ]
        , None )
    ]
;;

(* let rofl (a,b) = 1,2
   ---
   let ll_var_0 ll_arg_1 = match ll_arg_1 with (a, b) -> (1,2)
*)

let%test _ =
  lambda_lift_prog "let rofl (a,b) = 1,2"
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "ll_var_0"
            , [ Var "LL_arg_1" ]
            , LLMatch
                ( LLVar "LL_arg_1"
                , [ ( Tuple (Var "a", Var "b", [])
                    , LLTuple (LLConst (Int 1), LLConst (Int 2), []) )
                  ] ) )
          ]
        , None )
    ]
;;

let%test _ =
  lambda_lift_prog "let f x = print_int x"
  = [ LLLet
        ( Nonrecursive
        , [ Var "ll_var_0", [ Var "x" ], LLApplication (LLVar "print_int", LLVar "x") ]
        , None )
    ]
;;

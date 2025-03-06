open HamsterML.LL
open ParserTest

let lambda_lift_prog (s : string) =
  let open HamsterML.Utils.R in
  let prog = parse_prog s in
  let alpha_convert prog = run @@ HamsterML.AC.convert_prog prog in
  let lambda_lift prog = run @@ ll_prog prog in
  prog |> alpha_convert |> lambda_lift
;;

let pp_lambda_lift_prog (s : string) =
  let open HamsterML.PrinterLL in
  lambda_lift_prog s |> pretty_print_ll_prog |> print_string
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

let%test _ =
  lambda_lift_prog "let rec fac n = if n<=1 then 1 else n * fac (n-1)"
  = [ LLLet
        ( Recursive
        , [ ( Var "ll_var_0"
            , [ Var "arg_1" ]
            , LLIf
                ( LLApplication
                    ( LLApplication (LLOperation (Binary LTE), LLVar "arg_1")
                    , LLConst (Int 1) )
                , LLConst (Int 1)
                , Some
                    (LLApplication
                       ( LLApplication (LLOperation (Binary MUL), LLVar "arg_1")
                       , LLApplication
                           ( LLVar "ll_var_0"
                           , LLApplication
                               ( LLApplication (LLOperation (Binary SUB), LLVar "arg_1")
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
            , [ Var "arg_0"; Var "arg_1" ]
            , LLApplication
                (LLApplication (LLOperation (Binary ADD), LLVar "arg_0"), LLVar "arg_1") )
          ]
        , None )
    ; LLLet
        ( Nonrecursive
        , [ ( Var "ll_var_1"
            , [ Var "arg_3"; Var "arg_4" ]
            , LLApplication
                (LLApplication (LLVar "ll_var_0", LLVar "arg_3"), LLVar "arg_4") )
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
        , [ ( Var "ll_var_0"
            , [ Var "arg_2" ]
            , LLApplication (LLVar "ll_var_1", LLVar "arg_2") )
          ; ( Var "ll_var_1"
            , [ Var "arg_3" ]
            , LLApplication
                (LLApplication (LLOperation (Binary ADD), LLVar "arg_3"), LLConst (Int 1))
            )
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
            , [ Var "ll_arg_1" ]
            , LLMatch
                ( LLVar "ll_arg_1"
                , [ ( Tuple (Var "arg_0", Var "arg_1", [])
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
        , [ ( Var "ll_var_0"
            , [ Var "arg_0" ]
            , LLApplication (LLVar "print_int", LLVar "arg_0") )
          ]
        , None )
    ]
;;

let%test _ =
  lambda_lift_prog "let main = let () = print_int 10 in 0"
  = [ LLLet
        ( Nonrecursive
        , [ Const Unit, [], LLApplication (LLVar "print_int", LLConst (Int 10)) ]
        , None )
    ; LLLet (Nonrecursive, [ Var "ll_var_0", [], LLConst (Int 0) ], None)
    ]
;;

let%test _ =
  lambda_lift_prog "let f x = x + 1 let main = let () = print_int (f 1) in 0"
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "ll_var_0"
            , [ Var "arg_0" ]
            , LLApplication
                (LLApplication (LLOperation (Binary ADD), LLVar "arg_0"), LLConst (Int 1))
            )
          ]
        , None )
    ; LLLet
        ( Nonrecursive
        , [ ( Const Unit
            , []
            , LLApplication
                (LLVar "print_int", LLApplication (LLVar "ll_var_0", LLConst (Int 1))) )
          ]
        , None )
    ; LLLet (Nonrecursive, [ Var "ll_var_1", [], LLConst (Int 0) ], None)
    ]
;;

let%test _ =
  lambda_lift_prog
    {|let rec fac_cps n k =
      if n=1 then k 1 else
      fac_cps (n-1) ((fun p k n -> k (p*n)) k n) |}
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "ll_lam_1"
            , [ Var "arg_3"; Var "arg_4"; Var "arg_5" ]
            , LLApplication
                ( LLVar "arg_4"
                , LLApplication
                    ( LLApplication (LLOperation (Binary MUL), LLVar "arg_3")
                    , LLVar "arg_5" ) ) )
          ]
        , None )
    ; LLLet
        ( Recursive
        , [ ( Var "ll_var_0"
            , [ Var "arg_1"; Var "arg_2" ]
            , LLIf
                ( LLApplication
                    ( LLApplication (LLOperation (Binary EQ), LLVar "arg_1")
                    , LLConst (Int 1) )
                , LLApplication (LLVar "arg_2", LLConst (Int 1))
                , Some
                    (LLApplication
                       ( LLApplication
                           ( LLVar "ll_var_0"
                           , LLApplication
                               ( LLApplication (LLOperation (Binary SUB), LLVar "arg_1")
                               , LLConst (Int 1) ) )
                       , LLApplication
                           (LLApplication (LLVar "ll_lam_1", LLVar "arg_2"), LLVar "arg_1")
                       )) ) )
          ]
        , None )
    ]
;;

let%test _ =
  lambda_lift_prog "let f = (fun x -> x + 1) 2"
  = [ LLLet
        ( Nonrecursive
        , [ ( Var "ll_lam_1"
            , [ Var "arg_0" ]
            , LLApplication
                (LLApplication (LLOperation (Binary ADD), LLVar "arg_0"), LLConst (Int 1))
            )
          ]
        , None )
    ; LLLet
        ( Nonrecursive
        , [ Var "ll_var_0", [], LLApplication (LLVar "ll_lam_1", LLConst (Int 2)) ]
        , None )
    ]
;;

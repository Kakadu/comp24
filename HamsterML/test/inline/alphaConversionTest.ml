open HamsterML.Ast
open HamsterML.AC
open ParserTest

let alpha_conv_pattern (s : string) =
  let _, res = HamsterML.Utils.R.run (convert_pattern NameEnv.empty "arg_" (parse_pattern s)) in
  res
;;

let alpha_conv_expr (s : string) =
  let _, res = HamsterML.Utils.R.run (convert_expr NameEnv.empty (parse_expr s)) in
  res
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

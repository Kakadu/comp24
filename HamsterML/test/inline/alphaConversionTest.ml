open HamsterML.Ast
open HamsterML.AC
open ParserTest

let alpha_conv_pattern (s : string) =
  let _, res = HamsterML.Utils.R.run (ac_pattern NameEnv.empty (parse_pattern s)) in
  res
;;

let%test _ =
  alpha_conv_pattern "a :: b :: a"
  = ListConcat (Var "var_0", ListConcat (Var "var_1", Var "var_0"))
;;

let%test _ =
  alpha_conv_pattern "a, b, c, a"
  = Tuple (Var "var_0", Var "var_1", [ Var "var_2"; Var "var_0" ])
;;

let%test _ =
  alpha_conv_pattern "[a; b; a; c]" = List [ Var "var_0"; Var "var_1"; Var "var_0"; Var "var_2" ]
;;

open HamsterML.AC
open ParserTest

let alpha_conv_prog (s : string) =
  let res = HamsterML.Utils.R.run (convert_prog (parse_prog s)) in
  res
;;

let pp_alpha_conv_prog (s : string) =
  let open HamsterML.PrinterAst in
  s |> alpha_conv_prog |> pretty_print_prog |> print_string
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let main = a :: b :: a |};
  [%expect {| let var_0 = a::b::a |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let main = a, b, c, a, 1, 2, 3 |};
  [%expect {| let var_0 = (a, b, c, a, 1, 2, 3) |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let main = [a; b; a; c] |};
  [%expect {| let var_0 = [a; b; a; c] |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let main = fun a -> fun b -> fun a -> a + b |};
  [%expect {| let var_3 = (fun arg_0 -> (fun arg_1 -> (fun arg_2 -> (arg_2 + arg_1)))) |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let a = let a a a = a + 1 in a 1 2 |};
  [%expect {| let var_3 = let var_2 arg_0 arg_1 = (arg_1 + 1) in ((var_2 1) 2)  |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let rec f a b = let g a b = f a b in g a b |};
  [%expect
    {| let rec var_0 arg_1 arg_2 = let var_5 arg_3 arg_4 = ((var_0 arg_3) arg_4) in ((var_5 arg_1) arg_2)  |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let (+) a b = a - b |};
  [%expect {| let var_2 arg_0 arg_1 = (arg_0 - arg_1)  |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let z x y = let (+) a b = a - b in x + y |};
  [%expect
    {| let var_5 arg_0 arg_1 = let var_4 arg_2 arg_3 = (arg_2 - arg_3) in ((var_4 arg_0) arg_1) |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let f a b = a + b let f a b = f 1 2 |};
  [%expect
    {| 
  let var_2 arg_0 arg_1 = (arg_0 + arg_1)
  let var_5 arg_3 arg_4 = ((var_2 1) 2)  
    |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let f a b = a + b let rec f a b = f 1 2 |};
  [%expect
    {| 
    let var_2 arg_0 arg_1 = (arg_0 + arg_1)
    let rec var_3 arg_4 arg_5 = ((var_3 1) 2) 
    |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let test = 5 let test_fun x = x + test let test = 6 |};
  [%expect {| 
  let var_0 = 5
  let var_2 arg_1 = (arg_1 + var_0)
  let var_3 = 6  |}]
;;

let%expect_test _ =
  pp_alpha_conv_prog {| let rec f x = g x and g = f 1 |};
  [%expect {| let rec var_0 arg_2 = (var_1 arg_2) and var_1 = (var_0 1) |}]
;;

open HamsterML.CC
open ParserTest

let closure_conv_prog (s : string) =
  let open HamsterML.Utils.R in
  let prog = parse_prog s in
  let alpha_convert prog = run @@ HamsterML.AC.convert_prog prog in
  let closure_convert prog = cc_prog prog in
  prog |> alpha_convert |> closure_convert
;;

let pp_closure_conv_prog (s : string) =
  let open HamsterML.PrinterAst in
  closure_conv_prog s |> pretty_print_prog |> print_string
;;

let%expect_test _ =
  pp_closure_conv_prog {| let a = 1 |};
  [%expect {| let var_0 = 1 |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f = fun x y -> x + y |};
  [%expect {| let var_2 arg_0 arg_1 = ((( + ) arg_0) arg_1) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f = fun x -> fun y -> x + y |};
  [%expect {| let var_2 arg_1 arg_0 = ((( + ) arg_0) arg_1) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f = fun x y -> fun a b -> a b x y |};
  [%expect {| let var_4 arg_2 arg_3 arg_0 arg_1 = (((arg_2 arg_3) arg_0) arg_1) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f = fun x -> fun y -> fun a -> fun b -> a b x y |};
  [%expect {| let var_4 arg_3 arg_2 arg_1 arg_0 = (((arg_2 arg_3) arg_0) arg_1) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let f a b = let g = a in g + b |};
  [%expect
    {| let var_3 arg_0 arg_1 = let var_2 arg_0 = arg_0 in ((( + ) (var_2 arg_0)) arg_1) |}]
;;

(* 001fac.ml *)

let%expect_test _ =
  pp_closure_conv_prog {| let rec fac n = if n<=1 then 1 else n * fac (n-1) |};
  [%expect
    {| let rec var_0 arg_1 = if ((( <= ) arg_1) 1) then 1 else ((( * ) arg_1) (var_0 ((( - ) arg_1) 1))) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog {| let main = let () = print_int (fac 4) in 0 |};
  [%expect {| let var_0 fac = let () fac = (print_int (fac 4)) in 0 |}]
;;

(* 002fac.ml *)

let%expect_test _ =
  pp_closure_conv_prog
    {| let rec fac_cps n k = if n=1 then k 1 else fac_cps (n-1) (fun p -> k (p*n)) |};
  [%expect
    {| let rec var_0 arg_1 arg_2 = if ((( = ) arg_1) 1) then (arg_2 1) else ((var_0 ((( - ) arg_1) 1)) (((fun arg_1 arg_2 arg_3 -> (arg_2 ((( * ) arg_3) arg_1))) arg_1) arg_2)) |}]
;;

let%expect_test _ =
  pp_closure_conv_prog
    {| let main = let () = print_int (fac_cps 4 (fun print_int -> print_int)) in 0 |};
  [%expect {| not yet implemented |}]
;;

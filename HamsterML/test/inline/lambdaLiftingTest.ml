open HamsterML.LL
open ClosureConversionTest

let lambda_lift_prog (s : string) =
  let open HamsterML.Utils.R in
  let lambda_lift prog = run @@ ll_prog prog in
  s |> closure_conv_prog |> lambda_lift
;;

let pp_lambda_lift_prog (s : string) =
  let open HamsterML.PrinterLL in
  lambda_lift_prog s |> pretty_print_ll_prog |> print_string
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 1 |};
  [%expect {| let ll_var_0 = 1 |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 1 + 1 |};
  [%expect {| let ll_var_0 = (1 + 1) |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 10 let b = a |};
  [%expect {| 
    let ll_var_0 = 10
    let ll_var_1 = ll_var_0 
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 1 let a = a + 1 let rec a x = a 1 |};
  [%expect
    {| 
    let ll_var_0 = 1
    let ll_var_1 = (ll_var_0 + 1)
    let rec ll_var_2 arg_3 = (ll_var_2 1)
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let a = 10 and b = 20 |};
  [%expect {| 
    let ll_var_0 = 10 and ll_var_1 = 20
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let rec a = 1 + 1 in a + 2 |};
  [%expect {| 
    let rec ll_var_0 = (1 + 1) in (ll_var_0 + 2) 
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let sum x y = let f a b = a + b in f x y|};
  [%expect
    {| 
    let ll_var_1 arg_2 arg_3 = (arg_2 + arg_3)
    let ll_var_0 arg_0 arg_1 = ((ll_var_1 arg_0) arg_1)
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let sum x y = let res = x + y in res |};
  [%expect {| 
    let ll_var_0 arg_0 arg_1 = let ll_var_1 = (arg_0 + arg_1) in ll_var_1
    |}]
;;

let%expect_test "fac" =
  pp_lambda_lift_prog {| let rec fac n = if n<=1 then 1 else n * fac (n-1) |};
  [%expect
    {| 
    let rec ll_var_0 arg_1 = if (arg_1 <= 1) then 1 else (arg_1 * (ll_var_0 (arg_1 - 1)))
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let sum x y = x + y let main = sum |};
  [%expect
    {| 
    let ll_var_0 arg_0 arg_1 = (arg_0 + arg_1)
    let ll_var_1 = ll_var_0
    |}]
;;

let%expect_test "mutual rec" =
  pp_lambda_lift_prog {| let rec f x = g x and g x = x + 1 |};
  [%expect
    {| 
    let rec ll_var_0 arg_2 = (ll_var_1 arg_2) and ll_var_1 arg_3 = (arg_3 + 1)
    |}]
;;

let%expect_test "weird arguments elimination" =
  pp_lambda_lift_prog {| let rofl (a,b) _ () = 1 |};
  [%expect
    {| 
    let ll_var_0 ll_arg_1 ll_arg_2 ll_arg_3 = match ll_arg_3 with
    | () -> match ll_arg_2 with
    | _ -> match ll_arg_1 with
    | (arg_0, arg_1) -> 1
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let f x = print_int x |};
  [%expect {| 
    let ll_var_0 arg_0 = (print_int arg_0)
    |}]
;;

let%expect_test _ =
  pp_lambda_lift_prog {| let main = let () = print_int 10 in 0 |};
  [%expect {| 
    let ll_var_0 = let () = (print_int 10) in 0
    |}]
;;

let%expect_test "fac_cps" =
  pp_lambda_lift_prog
    {| let rec fac_cps n k = if n=1 then k 1 else fac_cps (n-1) (fun p -> k (p*n)) |};
  [%expect
    {| 
    let ll_lam_1 arg_1 arg_2 arg_3 = (arg_2 (arg_3 * arg_1))
    let rec ll_var_0 arg_1 arg_2 = if (arg_1 = 1) then (arg_2 1) else ((ll_var_0 (arg_1 - 1)) ((ll_lam_1 arg_1) arg_2))
    |}]
;;

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
  [%expect {| let var_0 = let () = (print_int (fac 4)) in 0 |}]
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
  [%expect {| let var_1 = let () = (print_int ((fac_cps 4) (fun arg_0 -> arg_0))) in 0 |}]
;;

(* 003fib.ml *)

let%expect_test _ =
  pp_closure_conv_prog
    {| 
      let rec fib_acc a b n = if n=1 then b else let n1 = n-1 in let ab = a+b in fib_acc b ab n1
      let rec fib n = if n<2 then n else fib (n - 1) + fib (n - 2)
      let main = let () = print_int (fib_acc 0 1 4) in let () = print_int (fib 4) in 0 
    |};
  [%expect
    {| 
      let rec var_0 arg_1 arg_2 arg_3 = if ((( = ) arg_3) 1) then arg_2 else let var_4 arg_3 = ((( - ) arg_3) 1) in let var_5 arg_1 arg_2 = ((( + ) arg_1) arg_2) in (((var_0 arg_2) ((var_5 arg_1) arg_2)) (var_4 arg_3))
      let rec var_6 arg_7 = if ((( < ) arg_7) 2) then arg_7 else ((( + ) (var_6 ((( - ) arg_7) 1))) (var_6 ((( - ) arg_7) 2)))
      let var_8 = let () = (print_int (((var_0 0) 1) 4)) in let () = (print_int (var_6 4)) in 0
    |}]
;;

let%expect_test _ =
  pp_closure_conv_prog
    {|
      let wrap f = if 1 = 1 then f else f

      let test3 a b c =
        let a = print_int a in
        let b = print_int b in
        let c = print_int c in
        0

      let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

      let main =
        let rez =
            (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
              1000000000)
        in
        let () = print_int rez in
        let temp2 = wrap test3 1 10 100 in
        0
    |};
  [%expect {| not yet implemented |}]
;;

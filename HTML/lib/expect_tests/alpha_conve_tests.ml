module AlphaConvTests = struct
  open Common.Counter.R

  let do_alpha_conv_test s =
    let+ actual = Parser.parse_program s in
    let+ actual_cc = Anf.Cc_ll.closure_convert actual in
    let+ actual_alpha = Anf.Alpha_conve.alpha_convert_prog actual_cc in
    Format.printf
      "---CC---\n\n%a\n\n---Alpha conv.---\n\n%a\n"
      AstLib.Pp_ast.pp_prog
      actual_cc
      AstLib.Pp_ast.pp_prog
      actual_alpha;
    Ok ()
  ;;

  let alpha_conv_test s =
    match do_alpha_conv_test s with
    | Ok _ -> ()
    | Error err -> Format.printf "Error: %s\n" err
  ;;
end

let%expect_test "variable shadowing in fun" =
  AlphaConvTests.alpha_conv_test
    {|
    let x =5 
    let x =
      let rec fac x 
      = if x <=1  then 1 else n * fac (x-1) in 
      let fac_ = fac 10 in
      fac fac_
  |};
  [%expect
    {|
    ---CC---

    let x = 5;;
    let rec cc_ll_0 n x = if (x <= 1) then 1 else (n * cc_ll_0 (x - 1));;
    let x = let fac = (cc_ll_0 n)
    in let fac_ = (fac 10)
    in (fac fac_)

    ---Alpha conv.---

    let x_1 = 5;;
    let rec cc_ll_0_1 n x = if (x <= 1) then 1 else (n * cc_ll_0_1 (x - 1));;
    let x_2 = let fac_l1 = (cc_ll_0_1 n)
    in let fac__l1 = (fac_l1 10)
    in (fac_l1 fac__l1) |}]
;;

let%expect_test "variable shadowing in match expression" =
  AlphaConvTests.alpha_conv_test
    {|
    let x = 5 
    let x = match 1 with 
    | x -> x
  |};
  [%expect
    {|
    ---CC---

    let x = 5;;
    let x = match 1 with
    | x -> x

    ---Alpha conv.---

    let x_1 = 5;;
    let x_2 = match 1 with
    | x -> x |}]
;;

let%expect_test "sequential variable shadowing in lets" =
  AlphaConvTests.alpha_conv_test {|
    let x = 3
    let x = x
    let x = x
  |};
  [%expect
    {|
    ---CC---

    let x = 3;;
    let x = x;;
    let x = x

    ---Alpha conv.---

    let x_1 = 3;;
    let x_2 = x_1;;
    let x_3 = x_2 |}]
;;

let%expect_test "variable shadowing in if-else branches" =
  AlphaConvTests.alpha_conv_test
    {|
  let x = 0
  let x = if x >= 0 then let x = 1 in x+1 else x 
  let x = x
  
  |};
  [%expect
    {|
    ---CC---

    let x = 0;;
    let x = if (x >= 0) then let x = 1
    in (x + 1) else x;;
    let x = x

    ---Alpha conv.---

    let x_1 = 0;;
    let x_2 = if (x_1 >= 0) then let x_l1 = 1
    in (x_l1 + 1) else x_1;;
    let x_3 = x_2 |}]
;;

let%expect_test "lexical scoping in nested lets with functions" =
  AlphaConvTests.alpha_conv_test
    {|
    let a = let x = 1 in
    let f y = x + y in 
    let x = 2 in 
    let x = 2 in 
    f x
  |};
  [%expect
    {|
    ---CC---

    let cc_ll_0 x y = (x + y);;
    let a = let x = 1
    in let f = (cc_ll_0 x)
    in let x = 2
    in let x = 2
    in (f x)

    ---Alpha conv.---

    let cc_ll_0_1 x y = (x + y);;
    let a_1 = let x_l1 = 1
    in let f_l1 = (cc_ll_0_1 x_l1)
    in let x_l2 = 2
    in let x_l3 = 2
    in (f_l1 x_l3) |}]
;;

let%expect_test "deeply nested variable shadowing chain" =
  AlphaConvTests.alpha_conv_test
    {|
    let a = let x = 1 in
    let x = 2 in 
    let x = 3 in 
    let x = 4 in 
    let x = 5 in 
    f x
  |};
  [%expect
    {|
    ---CC---

    let a = let x = 1
    in let x = 2
    in let x = 3
    in let x = 4
    in let x = 5
    in (f x)

    ---Alpha conv.---

    let a_1 = let x_l1 = 1
    in let x_l2 = 2
    in let x_l3 = 3
    in let x_l4 = 4
    in let x_l5 = 5
    in (f x_l5) |}]
;;

let%expect_test "recursive function" =
  AlphaConvTests.alpha_conv_test
    {|
    let rec fac n = if n<=1 then 1 else n * fac (n-1)

    let main =
      let () = print_int (fac 4) in
      0
  |};
  [%expect
    {|
    ---CC---

    let rec fac n = if (n <= 1) then 1 else (n * fac (n - 1));;
    let main = let () = (print_int (fac 4))
    in 0

    ---Alpha conv.---

    let rec fac_1 n = if (n <= 1) then 1 else (n * fac_1 (n - 1));;
    let main_1 = let () = (print_int (fac_1 4))
    in 0 |}]
;;

let%expect_test "mutual recursive function" =
  AlphaConvTests.alpha_conv_test
    {|
    let rec meven n = if n = 0 then 1 else modd (n - 1)
    and modd n = if n = 0 then 1 else meven (n - 1)

    let modd = modd 3
    let meven = meven modd
  |};
  [%expect
    {|
    ---CC---

    let rec meven n = if (n = 0) then 1 else modd (n - 1)
    and modd n = if (n = 0) then 1 else meven (n - 1);;
    let modd = (modd 3);;
    let meven = (meven modd)

    ---Alpha conv.---

    let rec meven_1 n = if (n = 0) then 1 else modd_1 (n - 1)
    and modd_1 n = if (n = 0) then 1 else meven_1 (n - 1);;
    let modd_2 = (modd_1 3);;
    let meven_2 = (meven_1 modd_2) |}]
;;

let%expect_test "CPS transformation" =
  AlphaConvTests.alpha_conv_test
    {|
    let rec fac_cps n k =
  if n=1 then k 1 else
  fac_cps (n-1) (fun p -> k (p*n))

  let main =
    fac_cps 4 (fun x -> x) 


  |};
  [%expect
    {|
    ---CC---

    let cc_ll_0 n k p = k (p * n);;
    let rec fac_cps n k = if (n = 1) then (k 1) else (fac_cps (n - 1) ((cc_ll_0 n) k));;
    let cc_ll_1 x = x;;
    let main = ((fac_cps 4) cc_ll_1)

    ---Alpha conv.---

    let cc_ll_0_1 n k p = k (p * n);;
    let rec fac_cps_1 n k = if (n = 1) then (k 1) else (fac_cps_1 (n - 1) ((cc_ll_0_1 n) k));;
    let cc_ll_1_1 x = x;;
    let main_1 = ((fac_cps_1 4) cc_ll_1_1) |}]
;;

module AlphaConvTests = struct
  open Common.Counter.R

  let do_alpha_conv_test s =
    let+ actual = Parser.parse_program s in
    let+ actual_pe = Patelim.Elim.p_elim_decls actual in
    let+ actual_cc = Anf.Cc_ll.closure_convert actual_pe in
    let+ actual_anf = Anf.Anf_conv.run actual_cc in
    let+ actual_alpha_conv = Anf.Alpha_conv.alpha_convert_prog actual_anf in
    Format.printf
      "---ANF---\n\n%a\n\n---Alpha conv.---\n\n%a\n"
      Anf.Pp_anf_ast.pp_anf_prog
      actual_anf
      Anf.Pp_anf_ast.pp_anf_prog
      actual_alpha_conv;
    Ok ()
  ;;

  let alpha_conv_test s =
    match do_alpha_conv_test s with
    | Ok _ -> ()
    | Error err -> Format.printf "Error: %s\n" err
  ;;
end

let%expect_test "sanity check" =
  AlphaConvTests.alpha_conv_test
    {|
    let rec fac n = if n<=1 then 1 else n * fac (n-1)

    let main =
      let () = print_int (fac 4) in
      0
  |};
  [%expect
    {|
    ---ANF---

    let rec fac n = let app_0 = (n <= 1) in
    let if_1 = if app_0 then 1 else let app_2 = (n - 1) in
    let app_3 = fac app_2 in
    let app_4 = (n * app_3) in
    app_4 in
    if_1;;
    let main  = let app_0 = fac 4 in
    let app_1 = print_int app_0 in
    let EVALUATED_0 = app_1 in
    let app_2 = (EVALUATED_0 ( = ) ()) in
    let if_3 = if app_2 then () else let app_4 = RTE_ERROR_MATCH_FAILURE () in
    app_4 in
    let _ = if_3 in
    0

    ---Alpha conv.---

    let rec fac.1 n = let app_0.l1 = (n <= 1) in
    let if_1.l2 = if app_0.l1 then 1 else let app_2.l2 = (n - 1) in
    let app_3.l4 = fac.1 app_2.l2 in
    let app_4.l8 = (n * app_3.l4) in
    app_4.l8 in
    if_1.l2;;
    let main.1  = let app_0.l3 = fac.1 4 in
    let app_1.l4 = print_int app_0.l3 in
    let EVALUATED_0.l8 = app_1.l4 in
    let app_2.l18 = (EVALUATED_0.l8 ( = ) ()) in
    let if_3.l32 = if app_2.l18 then () else let app_4.l40 = RTE_ERROR_MATCH_FAILURE () in
    app_4.l40 in
    let _.l0 = if_3.l32 in
    0 |}]
;;

let%expect_test "sanity check" =
  AlphaConvTests.alpha_conv_test
    {|
  let x = 0
  let x = if x >= 0 then let x = 1 in x+1 else x 
  let x = x
  
  |};
  [%expect
    {|
    ---ANF---

    let x  = 0;;
    let x  = let app_0 = (x >= 0) in
    let if_1 = if app_0 then let x = 1 in
    let app_2 = (x + 1) in
    app_2 else x in
    if_1;;
    let x  = x

    ---Alpha conv.---

    let x.1  = 0;;
    let x.2  = let app_0.l2 = (x.1 >= 0) in
    let if_1.l4 = if app_0.l2 then let x.l4 = 1 in
    let app_2.l8 = (x.l4 + 1) in
    app_2.l8 else x.1 in
    if_1.l4;;
    let x.3  = x.2 |}]
;;

let%expect_test "sanity check" =
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
    ---ANF---

    let cc_ll_0 x y = let app_0 = (x + y) in
    app_0;;
    let a  = let x = 1 in
    let app_0 = cc_ll_0 x in
    let f = app_0 in
    let x = 2 in
    let x = 2 in
    let app_1 = f x in
    app_1

    ---Alpha conv.---

    let cc_ll_0.1 x y = let app_0.l1 = (x + y) in
    app_0.l1;;
    let a.1  = let x.l19 = 1 in
    let app_0.l5 = cc_ll_0.1 x.l19 in
    let f.l8 = app_0.l5 in
    let x.l33 = 2 in
    let x.l35 = 2 in
    let app_1.l63 = f.l8 x.l35 in
    app_1.l63 |}]
;;

let%expect_test "sanity check" =
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
    ---ANF---

    let a  = let x = 1 in
    let x = 2 in
    let x = 3 in
    let x = 4 in
    let x = 5 in
    let app_0 = f x in
    app_0

    ---Alpha conv.---

    let a.1  = let x.l1 = 1 in
    let x.l3 = 2 in
    let x.l7 = 3 in
    let x.l10 = 4 in
    let x.l12 = 5 in
    let app_0.l12 = f x.l12 in
    app_0.l12 |}]
;;

let%expect_test "sanity check" =
  AlphaConvTests.alpha_conv_test
    {|
    let rec meven n = if n = 0 then 1 else modd (n - 1)
    and modd n = if n = 0 then 1 else meven (n - 1)

    let modd = modd 3
    let meven = meven modd
  |};
  [%expect
    {|
    ---ANF---

    let rec meven n = let app_0 = (n = 0) in
    let if_1 = if app_0 then 1 else let app_2 = (n - 1) in
    let app_3 = modd app_2 in
    app_3 in
    if_1
    and modd n = let app_4 = (n = 0) in
    let if_5 = if app_4 then 1 else let app_6 = (n - 1) in
    let app_7 = meven app_6 in
    app_7 in
    if_5;;
    let modd  = let app_0 = modd 3 in
    app_0;;
    let meven  = let app_0 = meven modd in
    app_0

    ---Alpha conv.---

    let rec meven.1 n = let app_0.l4 = (n = 0) in
    let if_1.l4 = if app_0.l4 then 1 else let app_2.l4 = (n - 1) in
    let app_3.l8 = modd.1 app_2.l4 in
    app_3.l8 in
    if_1.l4
    and modd.1 n = let app_4.l2 = (n = 0) in
    let if_5.l4 = if app_4.l2 then 1 else let app_6.l4 = (n - 1) in
    let app_7.l8 = meven.1 app_6.l4 in
    app_7.l8 in
    if_5.l4;;
    let modd.2  = let app_0.l6 = modd.1 3 in
    app_0.l6;;
    let meven.2  = let app_0.l8 = meven.1 modd.2 in
    app_0.l8 |}]
;;

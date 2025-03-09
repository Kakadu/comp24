module PCElimTests = struct
  open Patelim
  open Common.Counter.R

  let test s =
    let+ actual = Parser.parse_program s in
    let+ actual_cc = Anf.Cc_ll.closure_convert actual in
    let+ actual_alpha = Anf.Alpha_conve.alpha_convert_prog actual_cc in
    let+ actual_pc = PC_elim.pc_elim actual_alpha in
    let prog = (List.map IR_utils.transform_back_decl) actual_pc in
    Format.printf "%a\n" AstLib.Pp_ast.pp_prog prog;
    Ok ()
  ;;

  let test s =
    match test s with
    | Ok _ -> ()
    | Error err -> Format.printf "Error: %s\n" err
  ;;
end

let%expect_test _ =
  PCElimTests.test {|
  let f 5 = x 
  |};
  [%expect
    {|
    let f_1 p0 = let _ = if (p0 = 5) then () else (rte_error_match_failure ())
    in x |}]
;;

let%expect_test _ =
  PCElimTests.test {|
    let f 5 z 10 = x + z
  |};
  [%expect
    {|
    let f_1 p0 = let _ = if (p0 = 5) then () else (rte_error_match_failure ())
    in (fun z -> (fun p1 -> let _ = if (p1 = 10) then () else (rte_error_match_failure ())
    in (x + z)))
    |}]
;;

let%expect_test "PC elimination doesn't affect patterns with vars" =
  PCElimTests.test {|
    let f x = x + 1
  |};
  [%expect {|
    let f_1 x = (x + 1) |}]
;;

let%expect_test "PC elimination with PM" =
  PCElimTests.test
    {|
    let f x = 
      match x with 
      | 5 :: xs -> 
        let f 5 = true 
        in f 6 
      | _ -> false
  |};
  [%expect
    {|
    let cc_ll_0_1 = true;;
    let f_1 x = let evaluated_0 = x
    in if (([] != evaluated_0) && (5 = (get_head evaluated_0))) then let xs = (get_tale evaluated_0)
    in let f_l1 = cc_ll_0_1
    in (f_l1 6) else false |}]
;;

let%expect_test "Const at the left side" =
  PCElimTests.test {|
   let 5 = x
  |};
  [%expect
    {|
    let _ = let evaluated_0 = x
    in if (evaluated_0 = 5) then () else (rte_error_match_failure ()) |}]
;;

let%expect_test "Complex structures are not supported yet" =
  PCElimTests.test {|
   let (a, b) = (2, 3)
  |};
  [%expect {| let (a_1, b_1) = (2, 3) |}]
;;

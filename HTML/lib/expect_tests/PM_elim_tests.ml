module PMElimTests = struct
  open Patelim
  open Common.Counter.R

  let test s =
    let+ actual = Parser.parse_program s in
    let+ actual_cc = Anf.Cc_ll.closure_convert actual in
    let+ actual_alpha = Anf.Alpha_conve.alpha_convert_prog actual_cc in
    let+ actual_pc = PM_elim.pm_elim actual_alpha in
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
  PMElimTests.test {|
  let test1 x = 
    match (x) with 
    | a::b -> a + b 
  |};
  [%expect
    {|
    let test1_1 x = let evaluated_0 = x
    in if ([] != evaluated_0) then let a = (get_head evaluated_0)
    in let b = (get_tale evaluated_0)
    in (a + b) else (rte_error_match_failure ()) |}]
;;

let%expect_test _ =
  PMElimTests.test
    {|
  let test1 x (y:'a)= 
    match (x) with 
    | a::b -> (match a with 
              | 5 -> 1
              | _ -> 0)
    | [] -> 400
  |};
  [%expect
    {|
    let test1_1 x y = let evaluated_0 = x
    in if ([] != evaluated_0) then let a = (get_head evaluated_0)
    in let b = (get_tale evaluated_0)
    in let evaluated_1 = a
    in if (5 = evaluated_1) then 1 else 0 else if ([] = evaluated_0) then 400 else (rte_error_match_failure ()) |}]
;;

let%expect_test _ =
  PMElimTests.test {|
    let a l = 
        match l with 
        | hd, tl -> hd
    |};
  [%expect
    {|
    let a_1 l = let evaluated_0 = l
    in let hd = (get_nth (0, evaluated_0))
    in let tl = (get_nth (1, evaluated_0))
    in hd |}]
;;

let%expect_test "typed pattern" =
  PMElimTests.test
    {|
    let a = 
        match l with 
          | ((a : int), 0) -> 3
    |};
  [%expect
    {|
    let a_1 = let evaluated_0 = l
    in if (0 = (get_nth (1, evaluated_0))) then let a_p1 = (get_nth (0, evaluated_0))
    in 3 else (rte_error_match_failure ()) |}]
;;

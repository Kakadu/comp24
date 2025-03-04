module PCElimTests = struct
  open Patelim

  let test s =
    match Parser.parse_program s with
    | Ok actual ->
      let prog = PC_elim.pc_elim actual in
      let prog = Result.map (List.map IR_utils.transform_back_decl) prog in
      (match prog with
       | Ok prog -> Format.printf "%a\n" AstLib.Pp_ast.pp_prog prog
       | Error err -> Format.printf "%s\n" err)
    | Error err -> Format.printf "%s\n" err
  ;;
end

let%expect_test _ =
  PCElimTests.test {|
  let f 5 = x 
  |};
  [%expect
    {|
    let f P0 = let _ = if (P0 ( = ) 5) then () else (RTE_ERROR_MATCH_FAILURE ())
    in x |}]
;;

let%expect_test _ =
  PCElimTests.test {|
    let f 5 z 10 = x + z
  |};
  [%expect
    {|
    let f P0 = let _ = if (P0 ( = ) 5) then () else (RTE_ERROR_MATCH_FAILURE ())
    in (fun z -> (fun P1 -> let _ = if (P1 ( = ) 10) then () else (RTE_ERROR_MATCH_FAILURE ())
    in (x + z)))
    |}]
;;

let%expect_test "PC elimination doesn't affect patterns with vars" =
  PCElimTests.test {|
    let f x = x + 1
  |};
  [%expect {|
    let f x = (x + 1) |}]
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
    let f x = let EVALUATED_0 = x
    in if (([] ( != ) EVALUATED_0) ( && ) (5 ( = ) (GET_HEAD EVALUATED_0))) then let xs = (GET_TALE EVALUATED_0)
    in let f P1 = let _ = if (P1 ( = ) 5) then () else (RTE_ERROR_MATCH_FAILURE ())
    in true
    in (f 6) else false |}]
;;

let%expect_test "Const at the left side" =
  PCElimTests.test {|
   let 5 = x
  |};
  [%expect
    {|
    let _ = let EVALUATED_0 = x
    in if (EVALUATED_0 ( = ) 5) then () else (RTE_ERROR_MATCH_FAILURE ()) |}]
;;

let%expect_test "Complex structures are not supported yet" =
  PCElimTests.test {|
   let (a, b) = (2, 3)
  |};
  [%expect {| let (a, b) = (2, 3) |}]
;;

module PMElimTests = struct
  let test s =
    match Parser.parse_program s with
    | Ok actual ->
      let prog = PM_elim.pm_elim actual in
      let prog = Result.map (List.map IR_utils.transform_back_decl) prog in
      (match prog with
       | Ok prog -> Format.printf "%a\n" AstLib.Pp_ast.pp_prog prog
       | Error err -> Format.printf "%s\n" err)
    | Error err -> Format.printf "%s\n" err
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
    let test1 x = let EVALUATED_0 = x
    in if ([] ( != ) EVALUATED_0) then let a = (GET_HEAD EVALUATED_0)
    in let b = (GET_TALE EVALUATED_0)
    in (a + b) else (RTE_ERROR_MATCH_FAILURE ()) |}]
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
    let test1 x (y : 'a) = let EVALUATED_0 = x
    in if ([] ( != ) EVALUATED_0) then let a = (GET_HEAD EVALUATED_0)
    in let b = (GET_TALE EVALUATED_0)
    in let EVALUATED_1 = a
    in if (5 ( = ) EVALUATED_1) then 1 else 0 else if ([] ( = ) EVALUATED_0) then 400 else (RTE_ERROR_MATCH_FAILURE ()) |}]
;;

let%expect_test _ =
  PMElimTests.test {|
    let a l = 
        match l with 
        | hd, tl -> hd
    |};
  [%expect
    {|
    let a l = let EVALUATED_0 = l
    in let hd = (GET_NTH (0, EVALUATED_0))
    in let tl = (GET_NTH (1, EVALUATED_0))
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
    let a = let EVALUATED_0 = l
    in if (0 ( = ) (GET_NTH (1, EVALUATED_0))) then let (a : int) = ((GET_NTH (0, EVALUATED_0)) : int)
    in 3 else (RTE_ERROR_MATCH_FAILURE ()) |}]
;;

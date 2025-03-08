(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module AnfTests = struct
  open Common.Counter.R

  let do_anf s =
    let+ actual = Parser.parse_program s in
    let+ actual_cc1 = Anf.Cc_ll.closure_convert actual in
    let+ actual_alpha = Anf.Alpha_conve.alpha_convert_prog actual_cc1 in
    let+ actual_pe = Patelim.Elim.p_elim_decls actual_alpha in
    let+ actual_cc2 = Anf.Cc_ll.closure_convert actual_pe in
    let+ actual_anf = Anf.Anf_conv.run actual_cc2 in
    Format.printf
      "---CC & LL.---\n\n\
       %a\n\n\
       ---Alpha conv.---\n\n\
       %a\n\n\
       ---Patelim---\n\n\
       %a\n\n\
       ---CC & LL---\n\n\
       %a\n\n\
       ---ANF---\n\n\
       %a\n"
      AstLib.Pp_ast.pp_prog
      actual_cc1
      AstLib.Pp_ast.pp_prog
      actual_alpha
      AstLib.Pp_ast.pp_prog
      actual_pe
      AstLib.Pp_ast.pp_prog
      actual_cc2
      Anf.Pp_anf_ast.pp_anf_prog
      actual_anf;
    Ok ()
  ;;

  let anf_test s =
    match do_anf s with
    | Ok _ -> ()
    | Error err -> Format.printf "Error: %s\n" err
  ;;
end

let%expect_test "sanity check" =
  AnfTests.anf_test {|let test1 x = let test2 y = x + y in test2|};
  [%expect
    {|
    ---CC & LL.---

    let cc_ll_0 x y = (x + y);;
    let test1 x = let test2 = (cc_ll_0 x)
    in test2

    ---Alpha conv.---

    let cc_ll_0_1 x y = (x + y);;
    let test1_1 x = let test2_l1 = (cc_ll_0_1 x)
    in test2_l1

    ---Patelim---

    let cc_ll_0_1 x y = (x + y);;
    let test1_1 x = let test2_l1 = (cc_ll_0_1 x)
    in test2_l1

    ---CC & LL---

    let cc_ll_0_1 x y = (x + y);;
    let test1_1 x = let test2_l1 = (cc_ll_0_1 x)
    in test2_l1

    ---ANF---

    let cc_ll_0_1 x y = let app_0 = (x + y) in
    app_0;;
    let test1_1 x = let app_0 = cc_ll_0_1 x in
    let test2_l1 = app_0 in
    test2_l1 |}]
;;

let%expect_test "sanity check" =
  AnfTests.anf_test
    {|let nested1 = let nested2 = 5 in 
  let nested3 = 6 in
  let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55|};
  [%expect
    {|
    ---CC & LL.---

    let cc_ll_0 nested3 nested2 i = (nested2 + nested3);;
    let cc_ll_1 nested3 nested2 x = (x + (((cc_ll_0 nested3) nested2) 8));;
    let nested1 = let nested2 = 5
    in let nested3 = 6
    in let nested4 = ((cc_ll_1 nested3) nested2)
    in (nested4 55)

    ---Alpha conv.---

    let cc_ll_0_1 nested3 nested2 i = (nested2 + nested3);;
    let cc_ll_1_1 nested3 nested2 x = (x + (((cc_ll_0_1 nested3) nested2) 8));;
    let nested1_1 = let nested2_l1 = 5
    in let nested3_l1 = 6
    in let nested4_l1 = ((cc_ll_1_1 nested3_l1) nested2_l1)
    in (nested4_l1 55)

    ---Patelim---

    let cc_ll_0_1 nested3 nested2 i = (nested2 + nested3);;
    let cc_ll_1_1 nested3 nested2 x = (x + (((cc_ll_0_1 nested3) nested2) 8));;
    let nested1_1 = let nested2_l1 = 5
    in let nested3_l1 = 6
    in let nested4_l1 = ((cc_ll_1_1 nested3_l1) nested2_l1)
    in (nested4_l1 55)

    ---CC & LL---

    let cc_ll_0_1 nested3 nested2 i = (nested2 + nested3);;
    let cc_ll_1_1 nested3 nested2 x = (x + (((cc_ll_0_1 nested3) nested2) 8));;
    let nested1_1 = let nested2_l1 = 5
    in let nested3_l1 = 6
    in let nested4_l1 = ((cc_ll_1_1 nested3_l1) nested2_l1)
    in (nested4_l1 55)

    ---ANF---

    let cc_ll_0_1 nested3 nested2 i = let app_0 = (nested2 + nested3) in
    app_0;;
    let cc_ll_1_1 nested3 nested2 x = let app_0 = cc_ll_0_1 nested3 nested2 8 in
    let app_1 = (x + app_0) in
    app_1;;
    let nested1_1  = let nested2_l1 = 5 in
    let nested3_l1 = 6 in
    let app_0 = cc_ll_1_1 nested3_l1 nested2_l1 in
    let nested4_l1 = app_0 in
    let app_1 = nested4_l1 55 in
    app_1 |}]
;;

let%expect_test "type preserve" =
  AnfTests.anf_test {|let x = (let y = 5 in y) : int|};
  [%expect
    {|
    ---CC & LL.---

    let x = (let y = 5
    in y : int)

    ---Alpha conv.---

    let x_1 = (let y_l1 = 5
    in y_l1 : int)

    ---Patelim---

    let x_1 = (let y_l1 = 5
    in y_l1 : int)

    ---CC & LL---

    let x_1 = (let y_l1 = 5
    in y_l1 : int)

    ---ANF---

    let x_1  = let y_l1 = 5 in
    (y_l1 : int) |}]
;;

let%expect_test "type preserve" =
  AnfTests.anf_test {|let x = (let y = 5 in y) : int|};
  [%expect
    {|
    ---CC & LL.---

    let x = (let y = 5
    in y : int)

    ---Alpha conv.---

    let x_1 = (let y_l1 = 5
    in y_l1 : int)

    ---Patelim---

    let x_1 = (let y_l1 = 5
    in y_l1 : int)

    ---CC & LL---

    let x_1 = (let y_l1 = 5
    in y_l1 : int)

    ---ANF---

    let x_1  = let y_l1 = 5 in
    (y_l1 : int) |}]
;;

let%expect_test "type preserve" =
  AnfTests.anf_test {|let x = (let y = 5 in y) : int|};
  [%expect
    {|
    ---CC & LL.---

    let x = (let y = 5
    in y : int)

    ---Alpha conv.---

    let x_1 = (let y_l1 = 5
    in y_l1 : int)

    ---Patelim---

    let x_1 = (let y_l1 = 5
    in y_l1 : int)

    ---CC & LL---

    let x_1 = (let y_l1 = 5
    in y_l1 : int)

    ---ANF---

    let x_1  = let y_l1 = 5 in
    (y_l1 : int) |}]
;;

let%expect_test "type preserve" =
  AnfTests.anf_test
    {|
  let _start () () a () b _c () d __ =
  let () = print_int (a+b) in
  let () = print_int __ in
  a*b / _c + d


let main =
  print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
  |};
  [%expect
    {|
    ---CC & LL.---

    let _start () () a () b _c () d __ = let () = print_int (a + b)
    in let () = (print_int __)
    in (((a * b) / _c) + d);;
    let main = (print_int (((((((((_start (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))

    ---Alpha conv.---

    let _start_1 () () a () b _c () d __ = let () = print_int (a + b)
    in let () = (print_int __)
    in (((a * b) / _c) + d);;
    let main_1 = (print_int (((((((((_start_1 (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))

    ---Patelim---

    let _start_1 p0 = let _ = if (p0 = ()) then () else (rte_error_match_failure ())
    in (fun p1 -> let _ = if (p1 = ()) then () else (rte_error_match_failure ())
    in (fun a -> (fun p2 -> let _ = if (p2 = ()) then () else (rte_error_match_failure ())
    in (fun b -> (fun _c -> (fun p3 -> let _ = if (p3 = ()) then () else (rte_error_match_failure ())
    in (fun d -> (fun __ -> let _ = let evaluated_4 = print_int (a + b)
    in if (evaluated_4 = ()) then () else (rte_error_match_failure ())
    in let _ = let evaluated_5 = (print_int __)
    in if (evaluated_5 = ()) then () else (rte_error_match_failure ())
    in (((a * b) / _c) + d)))))))));;
    let main_1 = (print_int (((((((((_start_1 (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))

    ---CC & LL---

    let cc_ll_0 b a _c d __ = let _ = let evaluated_4 = print_int (a + b)
    in if (evaluated_4 = ()) then () else (rte_error_match_failure ())
    in let _ = let evaluated_5 = (print_int __)
    in if (evaluated_5 = ()) then () else (rte_error_match_failure ())
    in (((a * b) / _c) + d);;
    let cc_ll_1 a b _c p3 = let _ = if (p3 = ()) then () else (rte_error_match_failure ())
    in (((cc_ll_0 b) a) _c);;
    let cc_ll_2 a p2 = let _ = if (p2 = ()) then () else (rte_error_match_failure ())
    in (cc_ll_1 a);;
    let cc_ll_3 p1 = let _ = if (p1 = ()) then () else (rte_error_match_failure ())
    in cc_ll_2;;
    let _start_1 p0 = let _ = if (p0 = ()) then () else (rte_error_match_failure ())
    in cc_ll_3;;
    let main_1 = (print_int (((((((((_start_1 (print_int 1)) (print_int 2)) 3) (print_int 4)) 100) 1000) (print_int (- 1))) 10000) (- 555555)))

    ---ANF---

    let cc_ll_0 b a _c d __ = let app_0 = (a + b) in
    let app_1 = print_int app_0 in
    let evaluated_4 = app_1 in
    let app_2 = (evaluated_4 = ()) in
    let if_3 = if app_2 then () else let app_4 = rte_error_match_failure () in
    app_4 in
    let _ = if_3 in
    let app_5 = print_int __ in
    let evaluated_5 = app_5 in
    let app_6 = (evaluated_5 = ()) in
    let if_7 = if app_6 then () else let app_8 = rte_error_match_failure () in
    app_8 in
    let _ = if_7 in
    let app_9 = (a * b) in
    let app_10 = (app_9 / _c) in
    let app_11 = (app_10 + d) in
    app_11;;
    let cc_ll_1 a b _c p3 = let app_0 = (p3 = ()) in
    let if_1 = if app_0 then () else let app_2 = rte_error_match_failure () in
    app_2 in
    let _ = if_1 in
    let app_3 = cc_ll_0 b a _c in
    app_3;;
    let cc_ll_2 a p2 = let app_0 = (p2 = ()) in
    let if_1 = if app_0 then () else let app_2 = rte_error_match_failure () in
    app_2 in
    let _ = if_1 in
    let app_3 = cc_ll_1 a in
    app_3;;
    let cc_ll_3 p1 = let app_0 = (p1 = ()) in
    let if_1 = if app_0 then () else let app_2 = rte_error_match_failure () in
    app_2 in
    let _ = if_1 in
    cc_ll_2;;
    let _start_1 p0 = let app_0 = (p0 = ()) in
    let if_1 = if app_0 then () else let app_2 = rte_error_match_failure () in
    app_2 in
    let _ = if_1 in
    cc_ll_3;;
    let main_1  = let app_0 = - 555555 in
    let app_1 = - 1 in
    let app_2 = print_int app_1 in
    let app_3 = print_int 4 in
    let app_4 = print_int 2 in
    let app_5 = print_int 1 in
    let app_6 = _start_1 app_5 app_4 3 app_3 100 1000 app_2 10000 app_0 in
    let app_7 = print_int app_6 in
    app_7 |}]
;;

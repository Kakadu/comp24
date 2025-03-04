(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module AnfTests = struct
  let anf_test s =
    match Parser.parse_program s with
    | Ok actual ->
      let prog_pe = Patelim.Elim.p_elim_decls actual in
      (match prog_pe with
       | Ok actual_pe ->
         let prog_cc = Anf.Cc_ll.closure_convert actual_pe in
         (match prog_cc with
          | Ok actual_cc ->
            let prog_anf = Anf.Anf_conv.run actual_cc in
            (match prog_anf with
             | Ok actual_anf ->
               Format.printf
                 "---PE---\n\n%a\n\n---CC & LL---\n\n%a\n\n---ANF---\n\n%a\n"
                 AstLib.Pp_ast.pp_prog
                 actual_pe
                 AstLib.Pp_ast.pp_prog
                 actual_cc
                 Anf.Pp_anf_ast.pp_anf_prog
                 actual_anf
             | Error err -> Format.printf "%s\n" err)
          | Error err -> Format.printf "%s\n" err)
       | Error err -> Format.printf "%s\n" err)
    | Error err -> Format.printf "%s\n" err
  ;;
end

let%expect_test "sanity check" =
  AnfTests.anf_test {|let test1 x = let test2 y = x + y in test2|};
  [%expect
    {|
    ---PE---

    let test1 x = let test2 y = (x + y)
    in test2

    ---CC & LL---

    let cc_ll_0 x y = (x + y);;
    let test1 x = let test2 = (cc_ll_0 x)
    in test2

    ---ANF---

    let cc_ll_0 x y = let app_0 = (x + y) in
    app_0;;
    let test1 x = let app_0 = cc_ll_0 x in
    let test2 = app_0 in
    test2 |}]
;;

let%expect_test "sanity check" =
  AnfTests.anf_test
    {|let nested1 = let nested2 = 5 in 
  let nested3 = 6 in
  let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55|};
  [%expect
    {|
    ---PE---

    let nested1 = let nested2 = 5
    in let nested3 = 6
    in let nested4 x = (x + ((fun i -> (nested2 + nested3)) 8))
    in (nested4 55)

    ---CC & LL---

    let cc_ll_0 nested3 nested2 i = (nested2 + nested3);;
    let cc_ll_1 nested3 nested2 x = (x + (((cc_ll_0 nested3) nested2) 8));;
    let nested1 = let nested2 = 5
    in let nested3 = 6
    in let nested4 = ((cc_ll_1 nested3) nested2)
    in (nested4 55)

    ---ANF---

    let cc_ll_0 nested3 nested2 i = let app_0 = (nested2 + nested3) in
    app_0;;
    let cc_ll_1 nested3 nested2 x = let app_0 = cc_ll_0 nested3 nested2 8 in
    let app_1 = (x + app_0) in
    app_1;;
    let nested1  = let nested2 = 5 in
    let nested3 = 6 in
    let app_0 = cc_ll_1 nested3 nested2 in
    let nested4 = app_0 in
    let app_1 = nested4 55 in
    app_1 |}]
;;

let%expect_test "type preserve" =
  AnfTests.anf_test {|let x = (let y = 5 in y) : int|};
  [%expect
    {|
    ---PE---

    let x = (let y = 5
    in y : int)

    ---CC & LL---

    let x = (let y = 5
    in y : int)

    ---ANF---

    let x  = let y = 5 in
    (y : int) |}]
;;

let%expect_test "type preserve" =
  AnfTests.anf_test {|let x = (let y = 5 in y) : int|};
  [%expect
    {|
    ---PE---

    let x = (let y = 5
    in y : int)

    ---CC & LL---

    let x = (let y = 5
    in y : int)

    ---ANF---

    let x  = let y = 5 in
    (y : int) |}]
;;

let%expect_test "type preserve" =
  AnfTests.anf_test {|let x = (let y = 5 in y) : int|};
  [%expect
    {|
    ---PE---

    let x = (let y = 5
    in y : int)

    ---CC & LL---

    let x = (let y = 5
    in y : int)

    ---ANF---

    let x  = let y = 5 in
    (y : int) |}]
;;

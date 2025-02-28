(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module AnfTests = struct
let anf_test s =
  match Parser.parse_program s with
  | Ok actual ->
    let prog = Patelim.Elim.p_elim_decls actual in
    (match prog with
    | Ok actual ->  
      let prog = Anf.Cc_ll.closure_convert actual in

      let prog = Anf.Anf_conv.anf_program prog in
    Format.printf "%a\n" Anf.Pp_anf_ast.pp_anf_prog prog
      | Error _ -> failwith "hui")

  
  | Error err -> Format.printf "%s\n" err
    ;;
end


let%expect_test "sanity check" =
  AnfTests.anf_test {|let test1 x = let test2 y = x + y in test2|};
  [%expect
    {|
    let cc_ll_0 x y = let app_0 = (x + y) in
    app_0;;
    let test1 x = let app_0 = cc_ll_0 x in
    app_0 |}]
;;

let%expect_test "sanity check" =
  AnfTests.anf_test
    {|let nested1 = let nested2 = 5 in 
  let nested3 = 6 in
  let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55|};
  [%expect
    {|
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

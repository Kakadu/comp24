(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module CcLlTests = struct
  let cc_ll_test s =
    match Parser.parse_program s with
    | Ok actual ->
      let prog = Anf.Cc_ll.closure_convert actual in
      (match prog with
       | Ok actual -> Format.printf "%a\n" AstLib.Pp_ast.pp_prog actual
       | Error err -> Format.printf "%s\n" err)
    | Error err -> Format.printf "%s\n" err
  ;;
end

let%expect_test "sanity check" =
  CcLlTests.cc_ll_test {|let foo a = let _ = 5 in 10|};
  [%expect {|
    let foo a = let _ = 5
    in 10 |}]
;;

let%expect_test "sanity check" =
  CcLlTests.cc_ll_test
    {|let rec fact_cps n cont =
  if (n = 0) then
    cont 1
   else
     fact_cps (n - 1) (fun acc ->  cont  (n * acc))
|};
  [%expect
    {|
    let cc_ll_0 n cont acc = cont (n * acc);;
    let rec fact_cps n cont = if (n = 0) then (cont 1) else (fact_cps (n - 1) ((cc_ll_0 n) cont)) |}]
;;

let%expect_test "sanity check" =
  CcLlTests.cc_ll_test
    {|let nested1 = let nested2 = 5 in 
  let nested3 = 6 in
  let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
|};
  [%expect
    {|
    let cc_ll_0 nested3 nested2 i = (nested2 + nested3);;
    let cc_ll_1 nested3 nested2 x = (x + (((cc_ll_0 nested3) nested2) 8));;
    let nested1 = let nested2 = 5
    in let nested3 = 6
    in let nested4 = ((cc_ll_1 nested3) nested2)
    in (nested4 55) |}]
;;

let%expect_test "sanity check" =
  CcLlTests.cc_ll_test
    {|let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0

|};
  [%expect
    {|
    let rec cc_ll_0 acc xs = match xs with
    | [] -> acc
    | h :: tl -> (cc_ll_0 (acc + 1) tl);;
    let length_tail = let helper = cc_ll_0
    in (helper 0) |}]
;;

let%expect_test "sanity check" =
  CcLlTests.cc_ll_test
    {|let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1)

|};
  [%expect
    {|
    let rec meven n = if (n = 0) then 1 else modd (n - 1)
    and modd n = if (n = 0) then 1 else meven (n - 1) |}]
;;

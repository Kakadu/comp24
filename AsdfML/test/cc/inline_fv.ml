(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)


open Base
open Lib

let test_fv code =
  let open Format in
  let fv =
    code
    |> Parser.parse_program
    |> Result.ok_or_failwith
    |> List.hd_exn
    |> Vars.free_vars_def
  in
  let pp_set ppf set = fprintf ppf "{ %s }" (String.concat ~sep:", " (Set.to_list set)) in
  printf "%a" pp_set fv
;;

let%expect_test _ =
  test_fv "let _ = fun x -> x";
  [%expect {| {  } |}]
;;

let%expect_test _ =
  test_fv "let _ = fun x -> y";
  [%expect {| { y } |}]
;;

let%expect_test _ =
  test_fv "let _ = fun x -> (x y)";
  [%expect {| { y } |}]
;;

let%expect_test _ =
  test_fv "let _ = fun x -> if x then (x y) else z";
  [%expect {| { y, z } |}]
;;

let%expect_test _ =
  test_fv "let _ = fun x y -> (x y)";
  [%expect {| {  } |}]
;;

let%expect_test _ =
  test_fv "let _ = let x = 42 in x";
  [%expect {| {  } |}]
;;

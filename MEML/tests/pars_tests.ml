(** Copyright 2023-2024, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib
open Ast
open Parser

(* TESTS  PARSER*)

let start_test parser show input =
  let res = start_parsing parser input in
  match res with
  | Ok res -> Format.printf "%s" (show res)
  | Error err -> Format.printf "%s" err
;;

(* Test const parser *)

let%expect_test _ =
  let test = "true" in
  start_test parse_const show_const test;
  [%expect {| (CBool true) |}]
;;

let%expect_test _ =
  let test = "\"itsastring\"" in
  start_test parse_const show_const test;
  [%expect {| (CString "itsastring") |}]
;;

let%expect_test _ =
  let test = "951753" in
  start_test parse_const show_const test;
  [%expect {| (CInt 951753) |}]
;;

let%expect_test _ =
  let test = "varida" in
  start_test parse_var show_pattern test;
  [%expect {| (PVar ("varida", TUnknown)) |}]
;;

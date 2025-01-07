(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Kreml_lib.Inferencer

let parse_program input =
  let open Stdlib.Format in
  match Kreml_lib.Parser.run input with
  | Ok structure ->
    (match Kreml_lib.Inferencer.run structure with
     | Result.Ok env -> TypeEnv.pp std_formatter env
     | Error e -> pp_error std_formatter e)
  | Error msg -> pp_print_string std_formatter msg
;;

let%expect_test "" =
  let poly =
    "let f =\n\
    \    let rec helper a b = a, b in\n\
    \    let temp = helper 5 6 in\n\
    \    let temp2 = helper true false in\n\
    \    helper\n"
  in
  parse_program poly;
  [%expect
    {|
    [ f -> [ 13; 14; ]13 -> 14 -> 13 * 14
    , print_int -> [ ]int -> unit
     ] |}]
;;

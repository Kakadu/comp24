(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AefjnvMl_lib
open Parser
open Inferencer
open Base.Result
open Top_utils.Ast_test_utils

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  match parse input >>= check_program with
  | Ok env -> Inferencer.PP.pp_program Format.std_formatter env
  | Error err -> print_error err
;;

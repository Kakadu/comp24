(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AefjnvMl_lib
open Parser
open Inferencer
open Base.Result

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  match parse input >>= check_program with
  | Ok env -> Inferencer.PP.pp_program Format.std_formatter env
  | Error err ->
    (match err with
     | Parser e -> Parser.PP.pp_error Format.std_formatter e
     | Infer e -> Inferencer.PP.pp_error Format.std_formatter e)
;;

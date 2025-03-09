(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AefjnvMl_lib
open Base.Result
open Middleend
open Top_utils.Ast_test_utils

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let ast'_t =
    let open Common.Naming in
    let*! ast = Parser.parse s in
    let*! ast' = Alpha_converter.rename_ast_with_uniq alpha_prefix ast in
    let ast' = Middleend.Closure_conversion.convert_program ast' in
    let* ast' = Alpha_converter.rename_ast_with_uniq cc_prefix ast' in
    Ok ast'
  in
  let ast_printer ast_ = Stdlib.Format.printf "%a\n" Common.Ast_pp.pp_program ast_ in
  print_result ast_printer ast'_t
;;
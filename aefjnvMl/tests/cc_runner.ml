(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AefjnvMl_lib
open Base.Result

let ( let* ) = ( >>= )

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let ast'_t =
    let* ast = Parser.parse s in
    let* ast' = Alpha_converter.rename_ast_with_uniq ast in
    let ast' = Middleend.Closure_conversion.convert_program ast' in
    Ok ast'
  in
  match ast'_t with
  | Ok ast' -> Stdlib.Format.printf "%a\n" Common.Ast_pp.program_pp ast'
  | Error err ->
    (match err with
     | Parser e -> Parser.PP.pp_error Format.std_formatter e
     | Infer e -> Inferencer.PP.pp_error Format.std_formatter e
     | Alpha_converter (Illegal_state_error s) -> Format.print_string s)
;;

(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AefjnvMl_lib
open Middleend
open Result
open Top_utils.Ast_test_utils

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let ast'_t =
    let open Common.Naming in
    (* use [let*!] and [let+!] to exclude infer from pipeline *)
    let* ast = Parser.parse s in
    let*! env_b = Inferencer.check_program ast in
    let () = Pp_env.pp_env_before_modification env_b in
    let* ast' = Alpha_converter.rename_ast_with_uniq alpha_prefix ast in
    let* ast' = ok @@ Middleend.Closure_conversion.convert_program ast' in
    let* ast' = Alpha_converter.rename_ast_with_uniq cc_prefix ast' in
    let* ast' =
      let open Match_elimination in
      let+! m_ast = Match_elim.eliminate_match_in_program ast' in
      let m_ast' = Optimizations.optimize m_ast in
      Me_converter.convert_me_program m_ast'
    in
    let+! env_a = Inferencer.check_program ast' in
    let () = Pp_env.pp_env_after_modification env_a in
    ast'
  in
  let ast_printer ast_ = Stdlib.Format.printf "%a\n" Common.Ast_pp.pp_program ast_ in
  print_result ast_printer ast'_t
;;

(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let pp_me_program ppf prog =
  let ast = Me_converter.convert_program prog in
  Common.Ast_pp.program_pp ppf ast
;;

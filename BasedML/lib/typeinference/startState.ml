(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* open Ast *)
open StatementInfer
open Help

let add_bin_op : string -> Ast.typeName -> Ast.typeName -> Ast.typeName -> (state, unit) t
  =
  fun name _arg1 arg2 _ret ->
  let tp = Ast.TFunction (arg2, arg2) in
  write_flat_var_type name tp *> fail "boba"
;;

let start_state = MapString.empty, [], 0

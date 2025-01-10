(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open StatementInfer
open Help

let generalise : SetPolyType.t -> Ast.pattern -> Ast.type_name -> (state, unit) t =
  fun bound_vars pat tp ->
  let* tp = restore_type tp in
  let* subs = read_subs in
  let used_vars = get_tv_from_tp SetPolyType.empty tp in
  let unbound_vars = SetPolyType.diff used_vars bound_vars in
  let free_vars =
    SetPolyType.filter (fun name -> not (List.mem_assoc name subs)) unbound_vars
  in
  write_scheme_for_pattern free_vars pat tp
;;

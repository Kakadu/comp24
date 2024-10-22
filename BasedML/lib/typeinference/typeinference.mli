(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type res_map = Ast.type_name Help.MapString.t

val pp_res_map : Format.formatter -> res_map -> unit
val show_res_map : res_map -> string
val infer_prog : Ast.declarations -> (res_map, string) result
val test_infer_exp : string -> unit
val test_infer_prog : StatementInfer.state -> string -> unit

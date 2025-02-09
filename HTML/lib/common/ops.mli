(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val prohibited_ops : string list
val first_unop_strings : string list
val suffix_unop_strings : string list
val base_unops : string list
val first_binop_strings : string list
val suffix_binop_strings : string list
val base_binops : string list
val str_to_list : string -> string list
val is_op : string list -> string list -> string -> bool
val is_unop : string -> bool
val is_binop : string -> bool

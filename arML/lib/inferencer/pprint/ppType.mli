(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Prints the type of an expression *)
val pp_type : Format.formatter -> TypeTree.typ -> unit

(** Prints the type of an expression *)
val print_expr_type : TypeTree.typ -> unit

(** Prints the type of a program, mapping type environment to a list of string names *)
val print_program_type : (string, Schema.schema, 'a) Base.Map.t -> string list -> unit

(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Match_elimination

val pp_me_program : Format.formatter -> Me_ast.m_decl list -> unit

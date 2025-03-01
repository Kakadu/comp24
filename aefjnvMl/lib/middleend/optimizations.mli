(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Match_elimination

val optimize : Me_ast.m_decl list -> Me_ast.m_decl list

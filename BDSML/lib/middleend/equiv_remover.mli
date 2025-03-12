(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** remove expressions [let a = b], where [a] and [b] just identifiers *)
val equiv_remove : Anf_ast.absexpr list -> Anf_ast.absexpr list

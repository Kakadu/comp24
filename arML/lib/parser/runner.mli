(* Copyright 2024-2025, raf-nr and ksenmel *)
(* SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Main parsing function *)
val parse : string -> (Ast.structure_item list, string) result

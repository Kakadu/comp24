(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Checks that all variables in the given list of patterns are unique.
    Used to detect several bound errors in tuple patterns,
    list constructor patterns, and effects with arguments. *)
val check_unique_vars : Ast.AbstractSyntaxTree.pattern list -> (TypeTree.VarSet.t, TypeErrors.error) Common.StateResultMonad.t

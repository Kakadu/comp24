(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common.IdentifierStructs

(** Retrieves all identifiers from a given pattern. *)
val get_pattern_identifiers : AbstractSyntaxTree.pattern -> IdentifierSet.t

(** Retrieves all identifiers from a list of patterns. *)
val get_pattern_identifiers_from_list : AbstractSyntaxTree.pattern list -> IdentifierSet.t

(** Retrieves all identifiers from a list of pattern-expression cases. *)
val get_pattern_identifiers_from_cases : AbstractSyntaxTree.case list -> IdentifierSet.t

(** Retrieves all free variables from an expression. *)
val get_expr_free_vars : AbstractSyntaxTree.expression -> IdentifierSet.t

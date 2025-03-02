(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common.IdentifierStructs

(** Retrieves all identifiers from a list of pattern-expression cases. *)
val get_pattern_identifiers_from_cases : Llast.ll_case list -> IdentifierSet.t

(** Replaces identifiers in an expression according to the given map. *)
val substitute_identifiers_ll
  :  AbstractSyntaxTree.identifier IdentifierMap.t
  -> Llast.ll_expr
  -> Llast.ll_expr

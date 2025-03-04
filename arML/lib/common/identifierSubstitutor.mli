(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open IdentifierStructs

(** Replaces identifiers in an expression according to the given map. *)
val substitute_identifiers
  :  AbstractSyntaxTree.identifier IdentifierMap.t
  -> AbstractSyntaxTree.expression
  -> AbstractSyntaxTree.expression

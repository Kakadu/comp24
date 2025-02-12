(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common
open IdentifierStructs

(** [transform_fun expr] converts nested functions into a single function with multiple parameters. *)
val transform_fun : AbstractSyntaxTree.expression -> AbstractSyntaxTree.expression

(** [transform_let_in env expr] processes a `let ... in ...` expression, updating pattern identifiers to avoid conflicts. *)
val transform_let_in : IdentifierSet.t -> AbstractSyntaxTree.expression -> AbstractSyntaxTree.expression StateMonad.t

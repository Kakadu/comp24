(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.IdentifierStructs
open Ast.AbstractSyntaxTree
open Llast

(** [ll_expression] lift all nested functions and
    return tuple of expression without nested functions and
    list of new top-level declarations *)
val ll_expression
  :  IdentifierSet.t
  -> identifier IdentifierMap.t
  -> ll_decl list
  -> expression
  -> (ll_expr * ll_decl list) Common.StateMonad.t

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.IdentifierStructs
open Ast.AbstractSyntaxTree
open Llast

(** [ll_program] lift all nested functions in declarations on the top level *)
val ll_program
  :  (declaration list -> IdentifierSet.t)
  -> declaration list
  -> ll_decl list Common.StateMonad.t

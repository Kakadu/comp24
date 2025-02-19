(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Infers the type of a given expression within the specified type environment.
    Returns a substitution (mapping of type variables) and the inferred type,
    or a type error if inference fails. *)
val infer_expr
  :  TypeEnv.t
  -> Ast.AbstractSyntaxTree.expression
  -> (Substitution.t * TypeTree.typ, TypeErrors.error) Common.StateResultMonad.t

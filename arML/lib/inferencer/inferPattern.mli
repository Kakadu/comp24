(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Infers the type of a given pattern and updates the environment with new bindings.
    Returns the inferred type of the pattern and the updated environment,
    or a type error if inference fails. *)
val infer_pattern
  :  TypeEnv.t
  -> Ast.AbstractSyntaxTree.pattern
  -> (TypeTree.typ * TypeEnv.t, TypeErrors.error) Common.StateResultMonad.t

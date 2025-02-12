(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Infers types for a list of structure items in a program.  
    Returns the updated type environment and a list of declarations names  
    or a type error if inference fails. *)
val infer_program : 
  TypeEnv.t ->
  Ast.AbstractSyntaxTree.declaration list ->
  (TypeEnv.t * string list, TypeErrors.error) Common.StateResultMonad.t

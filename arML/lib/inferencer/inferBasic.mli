(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

(** Generates a fresh type variable. *)
val fresh_var : (TypeTree.typ, TypeErrors.error) Common.StateResultMonad.t

(** Infers the type of a constant. *)
val infer_const : constant -> TypeTree.typ

(** Looks up the type of an identifier in the environment.
    Returns a fresh type variable for `_` or retrieves the type from the environment. *)
val infer_id
  :  TypeEnv.t
  -> string
  -> (Substitution.t * TypeTree.typ, TypeErrors.error) Common.StateResultMonad.t

(** Recursively translates a type defenition from the AST into a type representation. *)
val get_type_by_defenition
  :  type_definition
  -> (TypeTree.typ, TypeErrors.error) Common.StateResultMonad.t

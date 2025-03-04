(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

(** Runs the type inference process on a given expression and returns either the inferred type or a type error. *)
val run_expr_inferencer : expression -> (TypeTree.typ, TypeErrors.error) result

(** Runs the type inference process on a program represented as a list of structure items.
    Returns either a mapping of inferred types for named entities along with a list of declarations names, or a type error. *)
val run_program_inferencer
  :  declaration list
  -> ( (string, Schema.schema, Base.String.comparator_witness) Base.Map.t * string list
       , TypeErrors.error )
       result

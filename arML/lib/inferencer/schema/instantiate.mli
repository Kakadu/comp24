(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Instantiate a schema into a concrete type *)
val instantiate : Schema.schema -> (TypeTree.typ, TypeErrors.error) Common.StateResultMonad.t

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Create schema by a type with respect to the type environment *)
val generalize : TypeEnv.t -> TypeTree.typ -> Schema.schema

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open IdentifierStructs

(** [get_new_name prefix env] generates a new unique name by appending an integer to
    the [prefix], ensuring that the generated name does not already exist in the
    given [env] (an [IdentifierSet]). *)
val get_new_name : string -> IdentifierSet.t -> string StateMonad.t

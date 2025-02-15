(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common
open Common.IdentifierStructs

(** [get_new_name prefix env] generates a new unique name by appending an integer to
    the [prefix], ensuring that the generated name does not already exist in the
    given [env] (an [IdentifierSet]). *)
val get_new_name : string -> IdentifierSet.t -> string StateMonad.t

(** [get_new_arg_name] generates a new unique argument name starting with the prefix
    "cc". It uses [get_new_name] to ensure the generated name does not collide with
    any existing identifier in the environment. *)
val get_new_arg_name : IdentifierSet.t -> string StateMonad.t

(** [get_new_ll_name] generates a new unique label name starting with the prefix
    "ll". It uses [get_new_name] to ensure the generated name does not collide with
    any existing identifier in the environment. *)
val get_new_ll_name : IdentifierSet.t -> string StateMonad.t

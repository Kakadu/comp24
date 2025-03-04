(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

(** Module for handling sets of identifiers *)
module IdentifierSet : Set.S with type elt = identifier

(** Module for handling maps with identifiers as keys *)
module IdentifierMap : Map.S with type key = identifier

(** Removes a set of keys from a given map *)
val remove_keys_from_map : IdentifierSet.t -> 'a IdentifierMap.t -> 'a IdentifierMap.t

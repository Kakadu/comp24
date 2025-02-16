(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

module IdentifierSet = Set.Make (struct
    type t = identifier

    let compare (Id a) (Id b) = String.compare a b
  end)

module IdentifierMap = Map.Make (struct
    type t = identifier

    let compare (Id a) (Id b) = String.compare a b
  end)

let remove_keys_from_map keys map =
  IdentifierSet.fold (fun key acc -> IdentifierMap.remove key acc) keys map
;;

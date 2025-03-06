(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateMonad
open StateMonad.Syntax
open IdentifierStructs

let rec get_new_name prefix env =
  let* new_s = fresh in
  let new_name = String.concat "_" [ prefix; Int.to_string new_s ] in
  if IdentifierSet.mem (Id new_name) env then get_new_name prefix env else return new_name
;;

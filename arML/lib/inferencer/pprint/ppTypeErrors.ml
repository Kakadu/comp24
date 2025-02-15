(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeErrors

let pp_error ppf = function
  | Occurs_check -> Format.fprintf ppf "Type error: occurs check failed."
  | Unification_failed (l, r) ->
    Format.fprintf
      ppf
      "Type error: unification failed - type %a does not match expected type %a"
      PpType.pp_type
      l
      PpType.pp_type
      r
  | Unbound_variable name -> Format.fprintf ppf "Type error: unbound variable '%s'" name
  | InvalidRecursionLeftHand ->
    Format.fprintf ppf "Only variables are allowed as left-hand side of `let rec'"
  | Several_bounds name ->
    Format.fprintf ppf "Type error: variable '%s' is bound several times" name
;;

let print_inferencer_error e =
  let error_str = Format.asprintf "%a" pp_error e in
  Format.printf "%s\n" error_str
;;

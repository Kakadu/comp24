(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

(* Description of pattern parsers *)
type pattern_dispatch =
  { parse_tuple_pattern : pattern_dispatch -> pattern Angstrom.t
  ; parse_list_pattern : pattern_dispatch -> pattern Angstrom.t
  ; parse_constant_pattern : pattern Angstrom.t
  ; parse_identifier_pattern : pattern Angstrom.t
  ; parse_nill_pattern : pattern Angstrom.t
  ; parse_any_pattern : pattern Angstrom.t
  ; parse_pattern_type_defition : pattern_dispatch -> pattern Angstrom.t
  }

(* Main pattern parser *)
val parse_pattern : bool -> pattern Angstrom.t
(* If first argument is true then can parse without brackets around the pattern.
   Otherwise, parentheses are required *)

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common
open Expression
open Declaration
open Angstrom

let parse_expression input = parse_string ~consume:All (many1 parse_expression) input

let parse_program input =
  let sep = option () (skip_wspace *> string ";;" *> return ()) in
  parse_string ~consume:All (many1 (parse_declaration <* sep <* skip_wspace)) input
;;
